library(readr)
library(dplyr)

library(ggplot2)
library(readr)
library(shinythemes)  
library(shiny)       # will load fluidPage

library(tidyr)     # For reshaping data (pivot_longer)
library(stringr)   # for cleaning text

library(forcats) # For fct_rev() to reverse the order of plot items

cleaned_survey_data <- read_csv("Riskwise_data/cleaned_survey_data.csv")
#View(cleaned_survey_data)

# --- A: Prepare data for "Decision Ranking" plot ---
# Use select() to get all desired columns in a single, clean command
decision_ranking_df <- cleaned_survey_data %>%
  select(
    starts_with("how_difficult_do_you_find_making_decisions_about"),
    region= which_region_are_your_farming_activities_based,
    grower_group= which_group_are_you_associated_with
  )


colnames(decision_ranking_df) ############# decision ranking dataframe 

# Reshape data from wide to long and clean up names
decision_ranking_long <- decision_ranking_df %>%
  pivot_longer(
    cols = starts_with("how_difficult"),
    names_to = "decision_name",
    values_to = "difficulty_level"
  ) %>%
  # Clean up the long decision names for better plot labels
  mutate(
    decision_name = str_replace(decision_name, "how_difficult_do_you_find_making_decisions_about_", " "),
    decision_name = str_replace_all(decision_name, "_", " "),
    decision_name = str_to_title(decision_name),
    # Handle specific name cases for clarity
    #decision_name = str_replace(decision_name, "Etc", "(etc)"),
    decision_name = str_replace(decision_name, "Type Mix Fallowing", "(Type / Mix / Fallowing etc)")
  ) %>%
  # Remove rows where the difficulty level is NA or Not Relevant
  filter(!is.na(difficulty_level), difficulty_level != "Not Relevant") %>%
  # Convert difficulty_level to a factor to control plotting order and colors
  mutate(difficulty_level = factor(difficulty_level, levels = c("Not Difficult", "Somewhat Difficult", "Very Difficult")))


unique(decision_ranking_long$decision_name)

############# our dataframe decision_ranking_long for decision ranking dashboard plot
nrow(decision_ranking_long)
head(decision_ranking_long)


# --- B: Prepare data for the other plots (Risk Attitude, etc.) ---
# another data frame for risk attitude, which has values number for several keys nitrogen, phosp.....
risk_attitude_df <- cleaned_survey_data %>%
  select(
    starts_with("what_balance"),
    how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    what_is_your_risk_attitude_when_making,
    which_region_are_your_farming_activities_based,
    which_group_are_you_associated_with,
    selected_risk_decision
  )

colnames(risk_attitude_df)

#####
# 2. Rename the risk attitude columns
risk_attitude_df <- risk_attitude_df %>%
  rename(
    intuition_vs_data    = what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    advisor_balance      = what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    production_vs_profit = what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    comfort_with_process = how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    risk_attitude        = what_is_your_risk_attitude_when_making,
    region               = which_region_are_your_farming_activities_based,
    grower_group         = which_group_are_you_associated_with,
    risk_decision        = selected_risk_decision
  )

colnames(risk_attitude_df)
head(risk_attitude_df)


# --- Prepare Filter Choices ---

# Get unique, sorted, non-NA values for the dropdown filters
# The "All" option is added to the start of each list.
#region_choices <- c("All Regions", sort(unique(na.omit(risk_attitude_df$region))))
#group_choices <- c("All Groups", sort(unique(na.omit(risk_attitude_df$grower_group))))
#decision_choices <- c("All Decisions", sort(unique(na.omit(risk_attitude_df$risk_decision))))


# --- C: Prepare Filter Choices ---
# Combine all possible regions and groups from both data frames to have a master list
all_regions <- c("All Regions", sort(unique(c(decision_ranking_df$region, risk_attitude_df$region))))
all_groups <- c("All Groups", sort(unique(c(decision_ranking_df$grower_group, risk_attitude_df$grower_group))))
# This filter only applies to the risk_attitude data
decision_choices <- c("All Decisions", sort(unique(na.omit(risk_attitude_df$risk_decision))))



head(decision_choices)
#head(group_choices)
# Define the variables we want to be able to plot, mapping user-friendly names to column names
plot_vars <- c(
  "Risk Attitude" = "risk_attitude",
  "Decision Ranking" = "decision_ranking", # New option
  "Intuition vs. Calculation" = "intuition_vs_data",
  "Comfort with Process" = "comfort_with_process",
  "Production vs. Profit" = "production_vs_profit",
  "Advisor Balance" = "advisor_balance"
)


# =================================================================
# 2. Define the User Interface (UI)
# =================================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Farm Decision-Making Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      selectInput("region_filter", "Regions:", choices = all_regions),
      selectInput("group_filter", "Groups:", choices = all_groups),
      
      # *** KEY CHANGE: The "Decisions" filter only appears when it's relevant ***
      conditionalPanel(
        condition = "input.variable_selector != 'Decision Ranking'",
        selectInput("decision_filter", "Specific Decision (for line charts):", choices = decision_choices)
      ),
      width = 3
    ),
    mainPanel(
      radioButtons("variable_selector", "Select a Metric to Display:",
                   choices = names(plot_vars),
                   selected = "Risk Attitude",
                   inline = TRUE),
      hr(),
      uiOutput("summary_bar_ui"),
      plotOutput("main_plot", height = "500px"),
      width = 9
    )
  )
)


# =================================================================
# 3. Define the Server Logic
# =================================================================
# =================================================================
# 4. Define the Server Logic
# =================================================================
# =================================================================
# 4. Define the Server Logic
# =================================================================
server <- function(input, output) {
  
  # Define the colors vector inside the server function for robust access
  difficulty_colors <- c(
    "Very Difficult" = "#d62728",
    "Somewhat Difficult" = "#ffbb78", 
    "Not Difficult" = "#2ca02c"
  )
  
  # --- Reactive Data Filtering ---
  
  # Filter for all plots EXCEPT Decision Ranking
  filtered_risk_data <- reactive({
    req(input$variable_selector != 'Decision Ranking')
    data <- risk_attitude_df
    
    # *** THE FIX IS HERE: Changed input. to input$ ***
    if (input$region_filter != "All Regions") {
      data <- data %>% filter(region == input$region_filter)
    }
    if (input$group_filter != "All Groups") {
      data <- data %>% filter(grower_group == input$group_filter)
    }
    if (input$decision_filter != "All Decisions") {
      data <- data %>% filter(risk_decision == input$decision_filter)
    }
    return(data)
  })
  
  # Filter ONLY for the Decision Ranking plot
  filtered_decision_data <- reactive({
    req(input$variable_selector == 'Decision Ranking')
    data <- decision_ranking_long
    
    # *** THE FIX IS HERE: Changed input. to input$ ***
    if (input$region_filter != "All Regions") {
      data <- data %>% filter(region == input$region_filter)
    }
    if (input$group_filter != "All Groups") {
      data <- data %>% filter(grower_group == input$group_filter)
    }
    return(data)
  })
  
  # --- Render the Summary Bar (Only for Risk Attitude) ---
  output$summary_bar_ui <- renderUI({
    req(input$variable_selector == "Risk Attitude")
    
    data <- filtered_risk_data()
    risk_data <- data %>% filter(!is.na(risk_attitude))
    if (nrow(risk_data) == 0) return(p("No data available for these filters."))
    
    summary_stats <- risk_data %>%
      mutate(category = case_when(
        risk_attitude %in% 0:1  ~ "Very Risk Avoidant",
        risk_attitude %in% 2:4  ~ "Risk Avoidant",
        risk_attitude == 5      ~ "Neutral",
        risk_attitude %in% 6:8  ~ "Risk Tolerant",
        risk_attitude %in% 9:10 ~ "Very Risk Tolerant"
      )) %>%
      count(category) %>%
      mutate(percentage = round(n / sum(n) * 100))
    
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    
    summary_cols <- lapply(category_order, function(cat) {
      percent_val <- summary_stats %>% filter(category == cat) %>% pull(percentage)
      if (length(percent_val) == 0) percent_val <- 0
      column(width = 2, style = "border: 1px solid #ddd; text-align: center; padding: 5px; margin: 0 5px;",
             tags$b(cat), tags$p(paste0(percent_val, "%")))
    })
    
    fluidRow(column(width = 1), summary_cols)
  })
  
  # --- Render the Main Plot ---
  output$main_plot <- renderPlot({
    
    # --- PLOT 1: DECISION RANKING (Stacked Bar Chart) ---
    if (input$variable_selector == "Decision Ranking") {
      ranking_data <- filtered_decision_data()
      
      if (nrow(ranking_data) == 0) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No ranking data for selected filters.", size = 6) + theme_void()
      } else {
        summary_ranking_data <- ranking_data %>%
          group_by(decision_name, difficulty_level) %>%
          summarise(n = n(), .groups = 'drop_last') %>%
          mutate(percentage = n / sum(n)) %>%
          ungroup()
        
        ggplot(summary_ranking_data, aes(y = fct_rev(decision_name), x = percentage, fill = difficulty_level)) +
          geom_col() +
          scale_x_continuous(labels = scales::percent, name = "Percentage of Respondents", expand = c(0, 0)) +
          scale_fill_manual(values = difficulty_colors, name = "Difficulty Level") +
          labs(title = "Decision Ranking by Assessed Difficulty", y = "Decision Type") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y = element_text(size = 12, face="bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank()
          )
      }
      
      # --- PLOT 2: ALL OTHER PLOTS (Line Charts) ---
    } else {
      data <- filtered_risk_data()
      selected_var <- plot_vars[input$variable_selector]
      
      if (nrow(data) == 0 || all(is.na(data[[selected_var]]))) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for these filters.", size = 6) + theme_void()
      } else {
        plot_data <- data %>%
          filter(!is.na(.data[[selected_var]])) %>%
          count(.data[[selected_var]], name = "count") %>%
          rename(value = 1)
        
        ggplot(plot_data, aes(x = value, y = count)) +
          geom_line(color = "#006699", size = 1.2) +
          geom_point(color = "#006699", size = 4) +
          scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
          expand_limits(y = 0) +
          labs(title = paste("Distribution of", input$variable_selector),
               x = "Score (0-10)", y = "Number of Respondents") +
          theme_minimal(base_size = 15) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                panel.grid.minor = element_blank())
      }
    }
  })
}  
  



# =================================================================
# 4. Run the Shiny App
# =================================================================
shinyApp(ui = ui, server = server)




