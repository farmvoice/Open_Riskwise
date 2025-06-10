library(readr)
library(dplyr)

library(ggplot2)
library(readr)
library(shinythemes)  
library(shiny)       # will load fluidPage
cleaned_survey_data <- read_csv("Riskwise_data/cleaned_survey_data.csv")
#View(cleaned_survey_data)


# Use select() to get all desired columns in a single, clean command
decision_ranking <- cleaned_survey_data %>%
  select(
    starts_with("how_difficult_do_you_find_making_decisions_about"),
    which_region_are_your_farming_activities_based,
    which_group_are_you_associated_with
  )


decision_ranking ############# decision ranking dataframe 


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
    grower_group                = which_group_are_you_associated_with,
    risk_decision        = selected_risk_decision
  )

colnames(risk_attitude_df)
head(risk_attitude_df)


# --- Prepare Filter Choices ---

# Get unique, sorted, non-NA values for the dropdown filters
# The "All" option is added to the start of each list.
region_choices <- c("All Regions", sort(unique(na.omit(risk_attitude_df$region))))
group_choices <- c("All Groups", sort(unique(na.omit(risk_attitude_df$grower_group))))
decision_choices <- c("All Decisions", sort(unique(na.omit(risk_attitude_df$risk_decision))))

head(decision_choices)
head(group_choices)
# Define the variables we want to be able to plot, mapping user-friendly names to column names
plot_vars <- c(
  "Risk Attitude" = "risk_attitude",
  "Intuition vs. Calculation" = "intuition_vs_data",
  "Comfort with Process" = "comfort_with_process",
  "Production vs. Profit" = "production_vs_profit",
  "Advisor Balance" = "advisor_balance"
)


# =================================================================
# 2. Define the User Interface (UI)
# =================================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Apply a visual theme
  
  titlePanel("Farm Decision-Making Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      
      selectInput("region_filter", 
                  "Regions:", 
                  choices = region_choices),
      
      selectInput("group_filter", 
                  "Groups:", 
                  choices = group_choices),
      
      selectInput("decision_filter", 
                  "Decisions:", 
                  choices = decision_choices),
      
      width = 3 # Make the sidebar a bit narrower
    ),
    
    mainPanel(
      # Radio buttons to select which variable to plot (styled like the screenshot's tabs)
      radioButtons("variable_selector", 
                   "Select a variable to display:",
                   choices = names(plot_vars),
                   selected = "Risk Attitude",
                   inline = TRUE),
      
      hr(), # A horizontal line for separation
      
      # A dynamic UI for the summary bar (will only show for "Risk Attitude")
      uiOutput("summary_bar_ui"),
      
      # The main plot
      plotOutput("main_plot", height = "500px"),
      
      width = 9 # Make the main panel wider
    )
  )
)

# =================================================================
# 3. Define the Server Logic
# =================================================================
server <- function(input, output) {
  # --- Reactive Data Filtering ---
  # This reactive expression filters the data based on user selections.
  # It will automatically re-run whenever a filter input changes.
  filtered_data <- reactive({
    
    # Start with the full, renamed dataset
    data <- risk_attitude_df
    
    # Apply filters if the user has selected something other than "All"
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
  
  # --- Render the Summary Bar ---
  # This UI element is created dynamically based on the filtered data.
  output$summary_bar_ui <- renderUI({
    
    # Only show this summary bar when "Risk Attitude" is selected
    if(input$variable_selector != "Risk Attitude") {
      return(NULL) # Return nothing if any other variable is selected
    }
    
    data <- filtered_data()
    
    # Prepare data for the summary bar
    risk_data <- data %>%
      filter(!is.na(risk_attitude)) # Exclude rows with no answer for risk_attitude
    
    # If no data remains after filtering, show a message
    if (nrow(risk_data) == 0) {
      return(p("No data available for the selected filters to create a summary."))
    }
    
    # Categorize the risk attitude scores and calculate percentages
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
    
    # Define the order for the categories
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    
    # Create the HTML for the summary bar using a list of columns
    summary_cols <- lapply(category_order, function(cat) {
      
      # Find the percentage for the current category
      percent_val <- summary_stats %>%
        filter(category == cat) %>%
        pull(percentage)
      
      # If the category doesn't exist in the filtered data, show 0%
      if (length(percent_val) == 0) {
        percent_val <- 0
      }
      
      # Create a column with the category name and percentage
      column(width = 2,
             style = "border: 1px solid #ddd; text-align: center; padding: 5px; margin: 0 5px;",
             tags$b(cat),
             tags$p(paste0(percent_val, "%"))
      )
    })
    
    # Combine the columns into a single fluidRow
    fluidRow(
      column(width = 1), # A spacer column
      summary_cols,      # The list of columns we just created
      column(width = 1)  # A spacer column
    )
  })
  
  # --- Render the Main Plot ---
  output$main_plot <- renderPlot({
    
    data <- filtered_data()
    
    # Get the actual column name from our named vector based on user selection
    selected_var <- plot_vars[input$variable_selector]
    
    # Check if there is data to plot
    if (nrow(data) == 0 || all(is.na(data[[selected_var]]))) {
      # If not, display a message instead of an empty plot
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No data available for the selected filters.", size = 6) +
        theme_void()
    } else {
      # Prepare data for the plot: count occurrences of each score
      plot_data <- data %>%
        filter(!is.na(.data[[selected_var]])) %>%
        count(.data[[selected_var]], name = "count") %>%
        rename(value = 1) # Rename the first column to 'value' for consistent plotting
      
      # Create the plot with ggplot2
      ggplot(plot_data, aes(x = value, y = count)) +
        geom_line(color = "#006699", size = 1.2) +
        geom_point(color = "#006699", size = 4) +
        scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
        expand_limits(y = 0) + # Make sure the y-axis starts at 0
        labs(
          title = paste("Distribution of", input$variable_selector),
          x = "Score (0-10)",
          y = "Number of Respondents"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
          panel.grid.minor = element_blank() # Cleaner look
        )
    }
  })
}

# =================================================================
# 4. Run the Shiny App
# =================================================================
shinyApp(ui = ui, server = server)




