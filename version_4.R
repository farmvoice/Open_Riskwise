# =================================================================
# 1. Load Libraries
# =================================================================
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(shinythemes)
library(tidyr)
library(stringr)
library(forcats)

# =================================================================
# 2. Load and Prepare Data (Runs once at app start)
# =================================================================
survey_data <- read_csv("Riskwise_data/cleaned_survey_data.csv", show_col_types = FALSE)

# --- A: Prepare "Decision Ranking" Data ---
decision_ranking_long <- survey_data %>%
  select(starts_with("how_difficult"), region = which_region_are_your_farming_activities_based, grower_group = which_group_are_you_associated_with) %>%
  pivot_longer(cols = starts_with("how_difficult"), names_to = "decision_name", values_to = "difficulty_level") %>%
  mutate(
    decision_name = str_to_title(str_replace_all(str_remove(decision_name, "how_difficult_do_you_find_making_decisions_about_"), "_", " ")),
    decision_name = str_replace(decision_name, "Etc", "(etc)"),
    decision_name = str_replace(decision_name, "Type Mix Fallowing", "(Type / Mix / Fallowing etc)"),
    decision_name = str_replace(decision_name, "Marketing Grain Sales And Contracts", "Marketing Strategies (Grain sales and contracts)")
  ) %>%
  filter(!is.na(difficulty_level), difficulty_level != "Not Relevant") %>%
  mutate(difficulty_level = factor(difficulty_level, levels = c("Not Difficult", "Somewhat Difficult", "Very Difficult")))

# --- B: Prepare "Risk Attitude" and other data ---
# --- B: Prepare "Risk Attitude" and other data ---
risk_attitude_df <- survey_data %>%
  select(
    client_id, # Keep client_id
    what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    what_is_your_risk_attitude_when_making,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision
  ) %>%
  # *** THE FIX: Rename using the original, full column names for reliability ***
  rename(
    intuition_vs_data    = what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    advisor_balance      = what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    production_vs_profit = what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    comfort_with_process = how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    risk_attitude        = what_is_your_risk_attitude_when_making
  )

# --- C: Prepare Filter Choices ---
all_regions <- c("All Regions", sort(unique(na.omit(survey_data$which_region_are_your_farming_activities_based))))
all_groups <- c("All Groups", sort(unique(na.omit(survey_data$which_group_are_you_associated_with))))
decision_choices <- c("All Decisions", sort(unique(na.omit(survey_data$selected_risk_decision))))
client_choices <- sort(unique(na.omit(risk_attitude_df$client_id))) # *** NEW: Choices for user selection ***

plot_vars <- c("Risk Attitude" = "risk_attitude", "Decision Ranking" = "decision_ranking", "Intuition vs. Calculation" = "intuition_vs_data",
               "Comfort with Process" = "comfort_with_process", "Production vs. Profit" = "production_vs_profit", "Advisor Balance" = "advisor_balance")


# =================================================================
# 3. Define the User Interface (UI)
# =================================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Farm Decision-Making Dashboard"),
  
  radioButtons("variable_selector", "Select a Metric to Display:",
               choices = names(plot_vars), selected = "Risk Attitude", inline = TRUE),
  hr(),
  
  fluidRow(
    # --- LEFT COLUMN: Filters ---
    column(3,
           h3("Filters", align = "center"),
           wellPanel(
             h4("Population 1 / Peers", align="center"),
             selectInput("region_filter1", "Region:", choices = all_regions),
             selectInput("group_filter1", "Group:", choices = all_groups),
             conditionalPanel(
               condition = "input.variable_selector != 'Decision Ranking'",
               selectInput("decision_filter1", "Specific Decision:", choices = decision_choices)
             )
           )
    ),
    
    # --- MIDDLE COLUMN: Main Content ---
    column(6,
           uiOutput("summary_output_ui"),
           plotOutput("main_plot", height = "500px"),
           div(align = "center",
               # *** NEW: Radio buttons for mode selection ***
               radioButtons("compare_mode", "View Mode:",
                            choices = c("Single Population" = "single",
                                        "Compare two populations" = "compare_two",
                                        "Compare me to my peers" = "compare_me"),
                            selected = "single", inline = TRUE)
           )
    ),
    
    # --- RIGHT COLUMN: Filters / User Selection ---
    column(3,
           # --- Panel for User Selection ("Compare me" mode) ---
           conditionalPanel(
             condition = "input.compare_mode == 'compare_me'",
             h3("My Response", align = "center"),
             wellPanel(
               selectInput("client_selector", "Select Your ID:", choices = client_choices)
             )
           ),
           # --- Panel for Population 2 Filters ("Compare two" mode) ---
           conditionalPanel(
             condition = "input.compare_mode == 'compare_two'",
             h3("Filters", align = "center"),
             wellPanel(
               h4("Population 2", align="center"),
               selectInput("region_filter2", "Region:", choices = all_regions, selected = all_regions[2]),
               selectInput("group_filter2", "Group:", choices = all_groups),
               conditionalPanel(
                 condition = "input.variable_selector != 'Decision Ranking'",
                 selectInput("decision_filter2", "Specific Decision:", choices = decision_choices)
               )
             )
           )
    )
  )
)

# =================================================================
# 4. Define the Server Logic
# =================================================================
# =================================================================
# 4. Define the Server Logic
# =================================================================
server <- function(input, output) {
  
  difficulty_colors <- c("Not Difficult" = "#2ca02c", "Somewhat Difficult" = "#ffbb78", "Very Difficult" = "#d62728")
  
  # --- Reactive Data Filtering ---
  filtered_data1 <- reactive({
    data <- if (input$variable_selector == "Decision Ranking") decision_ranking_long else risk_attitude_df
    if (input$region_filter1 != "All Regions") data <- data %>% filter(region == input$region_filter1)
    if (input$group_filter1 != "All Groups") data <- data %>% filter(grower_group == input$group_filter1)
    if (input$variable_selector != "Decision Ranking" && input$decision_filter1 != "All Decisions") {
      data <- data %>% filter(risk_decision == input$decision_filter1)
    }
    return(data)
  })
  
  filtered_data2 <- reactive({
    req(input$compare_mode == "compare_two")
    data <- if (input$variable_selector == "Decision Ranking") decision_ranking_long else risk_attitude_df
    if (input$region_filter2 != "All Regions") data <- data %>% filter(region == input$region_filter2)
    if (input$group_filter2 != "All Groups") data <- data %>% filter(grower_group == input$group_filter2)
    if (input$variable_selector != "Decision Ranking" && input$decision_filter2 != "All Decisions") {
      data <- data %>% filter(risk_decision == input$decision_filter2)
    }
    return(data)
  })
  
  selected_user_data <- reactive({
    req(input$compare_mode == "compare_me", input$client_selector)
    risk_attitude_df %>% filter(client_id == input$client_selector)
  })
  
  # --- Dynamic Summary UI ---
  output$summary_output_ui <- renderUI({
    req(input$variable_selector == "Risk Attitude")
    if (input$compare_mode == "compare_two") tableOutput("comparison_summary_table")
    else if (input$compare_mode == "single") uiOutput("single_summary_bar")
  })
  
  output$single_summary_bar <- renderUI({
    data <- filtered_data1()
    risk_data <- data %>% filter(!is.na(risk_attitude))
    if(nrow(risk_data) == 0) return(p("No data for this filter."))
    summary_stats <- risk_data %>% mutate(category = case_when(risk_attitude <= 1 ~ "Very Risk Avoidant", risk_attitude <= 4 ~ "Risk Avoidant", risk_attitude == 5 ~ "Neutral", risk_attitude <= 8 ~ "Risk Tolerant", TRUE ~ "Very Risk Tolerant")) %>% count(category) %>% mutate(percentage = round(n / sum(n) * 100))
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    summary_cols <- lapply(category_order, function(cat) {
      percent_val <- summary_stats$percentage[summary_stats$category == cat]
      if (length(percent_val) == 0) percent_val <- 0
      column(2, style="border:1px solid #ddd; text-align:center; padding:5px; margin:0 5px;", tags$b(cat), tags$p(paste0(percent_val, "%")))
    })
    fluidRow(column(1), summary_cols)
  })
  
  output$comparison_summary_table <- renderTable({
    process_summary <- function(df, pop_name) {df %>% filter(!is.na(risk_attitude)) %>% mutate(Category = case_when(risk_attitude <= 1 ~ "Very Risk Avoidant", risk_attitude <= 4 ~ "Risk Avoidant", risk_attitude == 5 ~ "Neutral", risk_attitude <= 8 ~ "Risk Tolerant", TRUE ~ "Very Risk Tolerant")) %>% count(Category) %>% mutate(!!pop_name := paste0(round(n / sum(n) * 100), "%")) %>% select(Category, !!pop_name)}
    summary1 <- process_summary(filtered_data1(), "Peer Group")
    summary2 <- process_summary(filtered_data2(), "Comparison Group")
    full_join(summary1, summary2, by = "Category") %>% replace(is.na(.), "0%")
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  # --- Main Plot Rendering Logic ---
  output$main_plot <- renderPlot({
    
    selected_var_name <- plot_vars[input$variable_selector]
    
    # --- A. COMPARE TWO POPULATIONS PLOT ---
    if (input$compare_mode == "compare_two") {
      process_plot_data <- function(df, pop_name, var) {df %>% filter(!is.na(.data[[var]])) %>% count(.data[[var]], name = "n") %>% mutate(percentage = n / sum(n)) %>% rename(value = 1) %>% mutate(population = pop_name)}
      combined_data <- bind_rows(process_plot_data(filtered_data1(), "Population 1", selected_var_name), process_plot_data(filtered_data2(), "Population 2", selected_var_name))
      if(nrow(combined_data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
      ggplot(combined_data, aes(x=value, y=percentage, color=population)) + geom_line(size=1.2) + geom_point(size=4) + scale_x_continuous(limits=c(0,10), breaks=seq(0,10,2)) + scale_y_continuous(labels=scales::percent) + labs(title=paste("Comparison of", input$variable_selector), x="Score (0-10)", y="Percentage of Respondents", color="Population") + theme_minimal(base_size=15) + theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom")
      
      # --- B. COMPARE ME TO MY PEERS PLOT ---
    } else if (input$compare_mode == "compare_me") {
      
      req(input$variable_selector != "Decision Ranking") # This mode is not for the ranking chart
      
      peers_data <- filtered_data1()
      user_data <- selected_user_data()
      user_value <- user_data[[selected_var_name]]
      
      peers_plot_data <- peers_data %>%
        filter(!is.na(.data[[selected_var_name]])) %>%
        count(.data[[selected_var_name]], name = "count") %>%
        rename(value = 1)
      
      if (nrow(peers_plot_data) == 0) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="No data for selected peer group.", size=6) + theme_void())
      }
      
      p <- ggplot(peers_plot_data, aes(x = value, y = count)) +
        geom_line(color = "#006699", size = 1.2) +
        geom_point(color = "#006699", size = 4) +
        scale_x_continuous(limits = c(-0.5, 10.5), breaks = seq(0, 10, 2), expand = c(0,0)) +
        scale_y_continuous(expand = expansion(mult = c(0, .1))) +
        labs(title = paste("Your Response vs. Peer Distribution for", input$variable_selector),
             x = "Score (0-10)", y = "Number of Respondents (Peers)") +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      # ... (the code for the plot 'p' before this block remains the same) ...
      
      if (length(user_value) > 0 && !is.na(user_value)) {
        p <- p + 
          # This part for the blue bar is unchanged
          geom_col(data = data.frame(value = user_value, count = max(peers_plot_data$count) * 0.5),
                   aes(x = value, y = count), fill = "#006699", alpha = 0.8, width = 0.4) +
          
          # *** THIS IS THE UPDATED PART FOR THE TEXT ***
          geom_text(data = data.frame(value = user_value, 
                                      # Adjust y position to be in the middle of the bar
                                      count = max(peers_plot_data$count) * 0.25), 
                    aes(label = "My Response"),  # Removed the \n
                    color = "white", 
                    fontface = "bold", 
                    size = 5, 
                    angle = 90,  # Rotate the text 90 degrees
                    hjust = 0.5) # Center the text horizontally
      }
      
      p # Display the final plot
      
      
      # --- C. SINGLE POPULATION PLOT ---
    } else {
      data <- filtered_data1()
      if (input$variable_selector == "Decision Ranking") {
        if(nrow(data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No ranking data.",size=6)+theme_void())
        summary_ranking <- data %>% count(decision_name,difficulty_level) %>% group_by(decision_name) %>% mutate(percentage=n/sum(n)) %>% ungroup()
        ggplot(summary_ranking,aes(y=fct_rev(decision_name),x=percentage,fill=difficulty_level)) + geom_col() + scale_x_continuous(labels=scales::percent,expand=c(0,0)) + scale_fill_manual(values=difficulty_colors,name="Difficulty Level") + labs(title="Decision Ranking by Assessed Difficulty",x="Percentage of Respondents",y="Decision Type") + theme_minimal(base_size=14) + theme(legend.position="bottom", plot.title=element_text(hjust=0.5, face="bold"), axis.text.y=element_text(size=12,face="bold"))
      } else {
        if(nrow(data)==0 || all(is.na(data[[selected_var_name]]))) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data for this filter.",size=6)+theme_void())
        plot_data <- data %>% filter(!is.na(.data[[selected_var_name]])) %>% count(.data[[selected_var_name]],name="count") %>% rename(value=1)
        ggplot(plot_data, aes(x=value, y=count)) + geom_line(color="#006699",size=1.2) + geom_point(color="#006699",size=4) + scale_x_continuous(limits=c(0,10),breaks=seq(0,10,2)) + expand_limits(y=0) + labs(title=paste("Distribution of",input$variable_selector),x="Score (0-10)",y="Number of Respondents") + theme_minimal(base_size=15) + theme(plot.title=element_text(hjust=0.5,face="bold"))
      }
    }
  })
}
# =================================================================
# 5. Run the Shiny App
# =================================================================
shinyApp(ui = ui, server = server)