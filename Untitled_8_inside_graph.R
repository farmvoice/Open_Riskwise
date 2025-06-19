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
library(ggrepel)
library(scales) # Added for scales::percent

# =================================================================
# 2. Load and Prepare Data
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
risk_attitude_df <- survey_data %>%
  select(
    client_id,
    what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    what_is_your_risk_attitude_when_making,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision
  ) %>%
  rename(
    intuition_vs_data     = what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    advisor_balance       = what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    production_vs_profit = what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    comfort_with_process = how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    risk_attitude         = what_is_your_risk_attitude_when_making
  )

# --- C. Prepare "Review Process" Data ---
review_process_df <- survey_data %>%
  select(
    client_id,
    review_type = have_you_undertaken_any_review_of_how_you_made_your_last,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision
  ) %>%
  filter(!is.na(review_type))

# --- D. Prepare Filter Choices and Global Variables ---
all_regions <- c("All Regions", sort(unique(na.omit(survey_data$which_region_are_your_farming_activities_based))))
all_groups <- c("All Groups", sort(unique(na.omit(survey_data$which_group_are_you_associated_with))))
decision_choices <- c("All Decisions", sort(unique(na.omit(survey_data$selected_risk_decision))))
client_choices <- sort(unique(na.omit(risk_attitude_df$client_id)))

plot_vars <- c("Risk Attitude" = "risk_attitude", "Decision Ranking" = "decision_ranking",
               "Review Process" = "review_process",
               "Intuition vs. Calculation" = "intuition_vs_data", "Comfort with Process" = "comfort_with_process",
               "Production vs. Profit" = "production_vs_profit", "Advisor Balance" = "advisor_balance")


# --- E. Create Region-to-Group Mapping ---
region_group_map <- list(
  "WA" = c("Corrigin Farm Improvement Group", "Facey Group", "Mingenew Irwin Group (MIG)", "South-East Premium Wheat Growers Association (SEPWA)", "Stirlings to Coast Farmers (SCF)", "The Liebe Group", "Western Australian No-Tillage Farmers Association (WANTFA)", "West Midlands Group (WMG)"),
  "NSW" = c("AgGrow Agronomy", "Agricultural Marketing and Production Systems (AMPS)", "Central West Farming Systems (CWFS)", "FarmLink", "Grain Orana Alliance (GOA)", "Holbrook Landcare", "Irrigation Farmers Network", "Irrigation Research and Extension Committee (IREC)", "Riverine Plains", "Southern Growers"),
  "QLD" = c("Capella", "Gindie", "Roma", "St George", "Goondiwindi", "Meandarra", "Brigalow"),
  "VIC and SA Mallee [BCG Lead]" = c("Coomandook Agriculture Bureau", "Field Applied Research (FAR Australia)", "Mallee Sustainable Farming (MSF)", "Birchup Cropping Group (BCG)"),
  "SA Eyre - [AIR EP Lead]" = c("Buckleboo Farm Improvement Group (BFIG)", "EPAG Research Trust", "Air EP"),
  "SA Central [Hart Lead]" = c("Mid North High Rainfall Zone (MNHRZ)", "Murray Plains Farmers (MPF)", "Northern Sustainable Soils (NSS)", "Upper North Farming Systems (UNFS)", "HART")
)


# =================================================================
# 3. Define the User Interface (UI)
# =================================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Farm Decision-Making Dashboard"),
  
  # --- TOP ROW: Selection Options, View Mode, and ALL Filters ---
  fluidRow(
    # Select Metric
    column(3,
           wellPanel(
             selectInput("variable_selector", "Select Metric to Display:",
                         choices = names(plot_vars), selected = "Risk Attitude")
           )
    ),
    # View Mode (Now conditional and a dropdown)
    column(3,
           wellPanel(
             conditionalPanel(
               condition = "input.variable_selector != 'Decision Ranking'", # Condition to hide for Decision Ranking
               selectInput("compare_mode", "View Mode:", # Changed to selectInput
                           choices = c("Single Population" = "single",
                                       "Compare two populations" = "compare_two",
                                       "Compare me to my peers" = "compare_me"),
                           selected = "single") # Default selected option
             )
           )
    ),
    # Population 1 / Peers Filters
    column(3,
           wellPanel(
             uiOutput("pop1_filter_header"), # Dynamic header for Population 1
             selectInput("region_filter1", "Region:", choices = all_regions),
             selectInput("group_filter1", "Group:", choices = all_groups),
             conditionalPanel(
               condition = "!['Decision Ranking', 'Review Process'].includes(input.variable_selector)",
               selectInput("decision_filter1", "Specific Decision:", choices = decision_choices)
             )
           )
    ),
    # Conditional Filters for Population 2 or My Response
    column(3,
           # This outer conditional panel hides the entire column if Decision Ranking is selected
           # and ensures the filters only appear when compare_mode is active.
           conditionalPanel(
             condition = "input.variable_selector != 'Decision Ranking'",
             conditionalPanel(
               condition = "input.compare_mode == 'compare_me'",
               h4("My Response Selector", align = "center"),
               wellPanel(
                 selectInput("client_selector", "Select Your ID:", choices = client_choices),
                 hr(),
                 uiOutput("user_response_text_ui")
               )
             ),
             conditionalPanel(
               condition = "input.compare_mode == 'compare_two'",
               uiOutput("pop2_filter_header"), # Dynamic header for Population 2
               wellPanel(
                 selectInput("region_filter2", "Region:", choices = all_regions, selected = all_regions[1]),
                 selectInput("group_filter2", "Group:", choices = all_groups),
                 conditionalPanel(
                   condition = "input.variable_selector != 'Review Process'",
                   selectInput("decision_filter2", "Specific Decision:", choices = decision_choices)
                 )
               )
             )
           )
    )
  ),
  hr(), # Horizontal line for separation between top controls and main content
  
  # --- Main Content Area (Plot and Summary) ---
  fluidRow(
    column(12,
           uiOutput("summary_output_ui"),
           plotOutput("main_plot", height = "500px")
    )
  )
)


# =================================================================
# 4. Define the Server Logic
# =================================================================
server <- function(input, output, session) {
  
  difficulty_colors <- c("Not Difficult" = "#2ca02c", "Somewhat Difficult" = "#ffbb78", "Very Difficult" = "#d62728")
  
  # --- Dynamic Group Filter Logic ---
  
  # Observer for Population 1 Filters
  observeEvent(input$region_filter1, {
    selected_region <- input$region_filter1
    
    if (selected_region == "All Regions") {
      choices <- all_groups
    } else {
      choices <- c("All Groups", region_group_map[[selected_region]])
    }
    updateSelectInput(session, "group_filter1", choices = choices)
  })
  
  # Observer for Population 2 Filters
  observeEvent(input$region_filter2, {
    selected_region <- input$region_filter2
    
    if (selected_region == "All Regions") {
      choices <- all_groups
    } else {
      choices <- c("All Groups", region_group_map[[selected_region]])
    }
    updateSelectInput(session, "group_filter2", choices = choices)
  })
  
  # --- Set View Mode to 'single' and hide it when 'Decision Ranking' is selected ---
  observeEvent(input$variable_selector, {
    if (input$variable_selector == "Decision Ranking") {
      updateSelectInput(session, "compare_mode", selected = "single")
    }
  }, ignoreInit = TRUE)
  
  
  # --- Reactive data filtering ---
  filtered_data <- function(pop_num = 1) {
    region_filter <- if(pop_num == 1) input$region_filter1 else input$region_filter2
    group_filter <- if(pop_num == 1) input$group_filter1 else input$group_filter2
    decision_filter <- if(pop_num == 1) input$decision_filter1 else input$decision_filter2
    data <- switch(input$variable_selector, "Decision Ranking" = decision_ranking_long, "Review Process" = review_process_df, risk_attitude_df)
    if (region_filter != "All Regions") data <- data %>% filter(region == region_filter)
    if (group_filter != "All Groups") data <- data %>% filter(grower_group == group_filter)
    if (!input$variable_selector %in% c("Decision Ranking", "Review Process") && decision_filter != "All Decisions") { data <- data %>% filter(risk_decision == decision_filter) }
    return(data)
  }
  
  selected_user_data <- reactive({
    req(input$compare_mode == "compare_me", input$client_selector)
    df <- if (input$variable_selector == "Review Process") review_process_df else risk_attitude_df
    df %>% filter(client_id == input$client_selector)
  })
  
  # --- Reactive expression for Population Size(s) ---
  plot_population_sizes <- reactive({
    # Function to get unique client count or total rows for decision ranking
    get_size <- function(data_df, metric_name) {
      if (metric_name == "Decision Ranking") {
        # For Decision Ranking, it's the number of responses (rows)
        if (is.null(data_df) || nrow(data_df) == 0) return(0)
        nrow(data_df)
      } else {
        # For other metrics, it's the number of unique clients
        if (is.null(data_df) || nrow(data_df) == 0) return(0)
        length(unique(na.omit(data_df$client_id)))
      }
    }
    
    pop1_data <- filtered_data(1)
    pop1_size <- get_size(pop1_data, input$variable_selector)
    
    pop2_size <- NULL # Initialize for population 2
    if (input$compare_mode == "compare_two") {
      pop2_data <- filtered_data(2)
      pop2_size <- get_size(pop2_data, input$variable_selector)
    }
    list(pop1_size = pop1_size, pop2_size = pop2_size)
  })
  
  # --- UI Output for Population 1 Filter Header ---
  output$pop1_filter_header <- renderUI({
    sizes <- plot_population_sizes()
    h4(paste0("Population 1 / Peers Filters (n=", sizes$pop1_size, ")"), align="center")
  })
  
  # --- UI Output for Population 2 Filter Header ---
  output$pop2_filter_header <- renderUI({
    sizes <- plot_population_sizes()
    # This header only appears when compare_mode is 'compare_two', so pop2_size will be available.
    h4(paste0("Population 2 Filters (n=", sizes$pop2_size, ")"), align="center")
  })
  
  
  # --- UI Element to show User's Response Text ---
  output$user_response_text_ui <- renderUI({
    req(input$compare_mode == 'compare_me')
    user_data <- selected_user_data()
    
    # Determine which column to get the response from
    response_text <- if (input$variable_selector == "Review Process") {
      user_data$review_type[1]
    } else if (input$variable_selector != "Decision Ranking") {
      user_data[[plot_vars[input$variable_selector]]][1]
    } else {
      return(NULL) # Don't show for Decision Ranking
    }
    
    if (is.na(response_text)) return(p("No response recorded."))
    
    # Display the formatted text
    div(
      p(strong("Your ID:"), br(), input$client_selector),
      p(strong("My Response:"), br(), paste0("'", response_text, "'"))
    )
  })
  
  # --- Dynamic Summary UI for Risk Attitude Table ---
  output$summary_output_ui <- renderUI({
    req(input$variable_selector == "Risk Attitude")
    if (input$compare_mode == "compare_two") tableOutput("comparison_summary_table")
    else if (input$compare_mode == "single") uiOutput("single_summary_bar")
  })
  output$single_summary_bar <- renderUI({
    data <- filtered_data(1); risk_data <- data %>% filter(!is.na(risk_attitude)); if(nrow(risk_data) == 0) return(p("No data for this filter."))
    summary_stats <- risk_data %>% mutate(category = case_when(risk_attitude <= 1 ~ "Very Risk Avoidant", risk_attitude <= 4 ~ "Risk Avoidant", risk_attitude == 5 ~ "Neutral", risk_attitude <= 8 ~ "Risk Tolerant", TRUE ~ "Very Risk Tolerant")) %>% count(category) %>% mutate(percentage = round(n / sum(n) * 100))
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant"); summary_cols <- lapply(category_order, function(cat) {percent_val <- summary_stats$percentage[summary_stats$category == cat]; if (length(percent_val) == 0) percent_val <- 0; column(2, style="border:1px solid #ddd; text-align:center; padding:5px; margin:0 5px;", tags$b(cat), tags$p(paste0(percent_val, "%")))}); fluidRow(column(1), summary_cols)
  })
  
  output$comparison_summary_table <- renderTable({
    category_order_vector <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    
    # Function to get percentages for a single population, ensuring all categories are present
    get_population_percentages <- function(df, pop_label) {
      df_filtered <- df %>%
        filter(!is.na(risk_attitude))
      
      if (nrow(df_filtered) == 0) {
        # Return a row with all 0% for this population if no data
        return_df <- tibble(
          Population = pop_label,
          `Very Risk Avoidant` = "0%",
          `Risk Avoidant` = "0%",
          Neutral = "0%",
          `Risk Tolerant` = "0%",
          `Very Risk Tolerant` = "0%"
        )
        # Ensure the columns are in the correct order for table binding
        return(return_df %>% select(Population, all_of(category_order_vector)))
      }
      
      df_summary <- df_filtered %>%
        mutate(Category = case_when(
          risk_attitude <= 1 ~ "Very Risk Avoidant",
          risk_attitude <= 4 ~ "Risk Avoidant",
          risk_attitude == 5 ~ "Neutral",
          risk_attitude <= 8 ~ "Risk Tolerant",
          TRUE ~ "Very Risk Tolerant"
        )) %>%
        count(Category) %>%
        mutate(percentage = n / sum(n)) # Calculate raw percentage for internal use
      
      
      # Create a full set of categories with 0% for missing ones
      full_categories_df <- tibble(Category = factor(category_order_vector, levels = category_order_vector)) %>%
        left_join(df_summary, by = "Category") %>%
        mutate(percentage = replace_na(percentage, 0)) %>%
        mutate(display_percentage = paste0(round(percentage * 100), "%")) # Format for display
      
      # Pivot to wide format for a single row
      full_categories_df %>%
        select(Category, display_percentage) %>%
        pivot_wider(names_from = Category, values_from = display_percentage) %>%
        mutate(Population = pop_label) %>%
        select(Population, all_of(category_order_vector)) # Ensure order of columns
    }
    
    summary1_row <- get_population_percentages(filtered_data(1), "Population 1")
    summary2_row <- get_population_percentages(filtered_data(2), "Population 2")
    
    bind_rows(summary1_row, summary2_row)
    
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  
  # --- Main Plot Rendering Logic ---
  output$main_plot <- renderPlot({
    
    # --- RENDER REVIEW PROCESS PLOT ---
    if (input$variable_selector == "Review Process") {
      
      # For "Compare me", the plot is just the peer group pie chart
      # The logic is now the same as the "single" mode.
      if (input$compare_mode == "single" || input$compare_mode == "compare_me") {
        plot_data <- filtered_data(1) %>%
          count(review_type) %>%
          arrange(desc(review_type)) %>%
          mutate(
            percentage = n / sum(n),
            ypos = cumsum(percentage) - 0.5 * percentage,
            label_text = scales::percent(percentage, accuracy = 1)
          )
        if(nrow(plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No data for selected filters.", size=6) + theme_void())
        
        ggplot(plot_data, aes(x = 2, y = percentage, fill = review_type)) +
          geom_col(width = 1, color = "white") +
          coord_polar(theta = "y", start = 0) +
          geom_label_repel(aes(y = ypos, label = label_text), size = 6, show.legend = FALSE, nudge_x = 1, segment.color = "black") +
          scale_fill_brewer(palette = "Pastel1", name = "Review Process", labels = function(x) str_wrap(x, width = 25)) +
          xlim(0.5, 3.5) +
          theme_void() +
          theme(legend.title=element_text(face="bold", size=14), legend.text = element_text(size=12))
        
      } else if (input$compare_mode == "compare_two") {
        data1 <- filtered_data(1) %>% count(review_type) %>% mutate(percentage = n/sum(n), population = "Population 1")
        data2 <- filtered_data(2) %>% count(review_type) %>% mutate(percentage = n/sum(n), population = "Population 2")
        plot_data <- bind_rows(data1, data2)
        if(nrow(plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No data.", size=6) + theme_void())
        ggplot(plot_data, aes(x=review_type, y=percentage, fill=population)) +
          geom_col(position = "dodge") +
          scale_y_continuous(labels = scales::percent) +
          scale_x_discrete(labels = function(x) str_wrap(x, width=15)) +
          labs(title = "Comparison of Review Processes", x = "Review Type", y = "Percentage of Respondents") +
          theme_minimal(base_size = 14) + theme(legend.position="bottom", plot.title=element_text(hjust=0.5, face="bold"))
      }
      
      # --- RENDER ALL OTHER PLOTS ---
    } else {
      selected_var_name <- plot_vars[input$variable_selector]
      if (input$compare_mode == "compare_two") {
        process_plot_data <- function(df, pop_name, var) {df %>% filter(!is.na(.data[[var]])) %>% count(.data[[var]], name = "n") %>% mutate(percentage = n / sum(n)) %>% rename(value = 1) %>% mutate(population = pop_name)}
        combined_data <- bind_rows(process_plot_data(filtered_data(1), "Population 1", selected_var_name), process_plot_data(filtered_data(2), "Population 2", selected_var_name))
        if(nrow(combined_data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
        ggplot(combined_data, aes(x=value, y=percentage, color=population)) +
          geom_line(size=1.2) +
          geom_point(size=4) +
          scale_x_continuous(limits=c(0,10), breaks=seq(0,10,2)) +
          scale_y_continuous(labels=scales::percent, expand = expansion(mult = c(0.03, .15))) +
          labs(title=paste("Comparison of", input$variable_selector), x="Score (0-10)", y="Percentage of Respondents", color="Population") +
          theme_minimal(base_size=15) +
          theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom")
      } else if (input$compare_mode == "compare_me") {
        req(input$variable_selector != "Decision Ranking")
        peers_data <- filtered_data(1)
        user_data <- selected_user_data()
        user_value <- user_data[[selected_var_name]]
        
        peers_plot_data <- peers_data %>%
          filter(!is.na(.data[[selected_var_name]])) %>%
          count(.data[[selected_var_name]], name = "count") %>%
          mutate(percentage = count / sum(count)) %>% # Calculate percentage
          rename(value = 1)
        
        if (nrow(peers_plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No peer data.", size=6) + theme_void())
        
        p <- ggplot(peers_plot_data, aes(x = value, y = percentage)) + # Changed y to percentage
          geom_line(color = "#006699", size = 1.2) +
          geom_point(color = "#006699", size = 4) +
          scale_x_continuous(limits = c(-0.5, 10.5), breaks = seq(0, 10, 2), expand = c(0,0)) +
          scale_y_continuous(labels = scales::percent) + # Changed labels and expand
          labs(title = paste("Your Response vs. Peer Distribution for", input$variable_selector),
               x = "Score (0-10)",
               y = "Percentage of Respondents") + # Changed y-axis label
          theme_minimal(base_size = 15) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        
        # Add user's response as a vertical line and text
        if (length(user_value) > 0 && !is.na(user_value)) {
        band_width <- 0.5 # Define width of the highlight band (e.g., 1 unit on the x-axis)
        p <- p +
          geom_rect(
            aes(xmin = user_value - band_width, xmax = user_value + band_width,
                ymin = 0, ymax = 1.0), # Span full y-axis (0-1 for percentage)
            fill = "#006699", alpha = 0.8, inherit.aes = FALSE # Blue color, semi-opaque
          ) +
          # No geom_vline, as geom_rect replaces it
          geom_text(
            data = data.frame(value = user_value),
            aes(x = user_value, y = 0.5), # Position text vertically in the middle of the plot
            label = "My Response",
            color = "white", fontface = "bold", size = 5,
            angle = 90, # Rotate text by 90 degrees
            hjust = 0.5, vjust = 0.5 # Center text within its bounding box for rotation
          )
        }
        p
      } else {
        data <- filtered_data(1)
        if (input$variable_selector == "Decision Ranking") {
          if(nrow(data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No ranking data.",size=6)+theme_void())
          summary_ranking <- data %>% count(decision_name,difficulty_level) %>% group_by(decision_name) %>% mutate(percentage=n/sum(n)) %>% ungroup()
          ggplot(summary_ranking,aes(y=fct_rev(decision_name),x=percentage,fill=difficulty_level)) +
            geom_col() +
            scale_x_continuous(labels=scales::percent,expand=c(0,0)) +
            scale_fill_manual(values=difficulty_colors,name="Difficulty Level") +
            labs(title="Decision Ranking by Assessed Difficulty",x="Percentage of Respondents",y="Decision Type") +
            theme_minimal(base_size=14) +
            theme(legend.position="bottom", plot.title=element_text(hjust=0.5, face="bold"), axis.text.y=element_text(size=12,face="bold"))
        } else {
          if(nrow(data)==0 || all(is.na(data[[selected_var_name]]))) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
          plot_data <- data %>%
            filter(!is.na(.data[[selected_var_name]])) %>%
            count(.data[[selected_var_name]],name="count") %>%
            mutate(percentage = count / sum(count)) %>% # Calculate percentage
            rename(value=1)
          ggplot(plot_data, aes(x=value, y=percentage)) +
            geom_line(color="#006699",size=1.2) +
            geom_point(color="#006699",size=4) +
            scale_x_continuous(limits=c(0,10),breaks=seq(0,10,2)) +
            scale_y_continuous(labels=scales::percent, expand = expansion(mult = c(0.03, 0.15))) +
            labs(title=paste("Distribution of",input$variable_selector),
                 x="Score (0-10)",
                 y="Percentage of Respondents") +
            theme_minimal(base_size=15) +
            theme(plot.title=element_text(hjust=0.5,face="bold"))
        }
      }
    }
  })
}

# =================================================================
# 5. Run the Shiny App
# =================================================================
shinyApp(ui = ui, server = server)