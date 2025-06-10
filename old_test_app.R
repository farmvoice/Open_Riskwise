# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(writexl)  # for saving to Excel
library(ggplot2)
######## now we will select few columns for further visualization part
#LOAD THE DATASET
library(readr)
library(DT)
cleaned_survey_data <- read_csv("Riskwise_data/cleaned_survey_data.csv")
#cleaned_survey_data <- read_csv("cleaned_survey_data.csv")  #for production time

#cleaned_survey_data

# LIST COLUMN NAMES
colnames(cleaned_survey_data)


selected_columns <- cleaned_survey_data %>%
  select(
    
    how_difficult_do_you_find_making_decisions_about_nitrogen_application_amount_type_timing,                                                         
    how_difficult_do_you_find_making_decisions_about_buying_major_new_machinery_infrastructure,                                                       
    how_difficult_do_you_find_making_decisions_about_land_purchase_and_leasing,                                                                       
    how_difficult_do_you_find_making_decisions_about_preventative_fungicide_spraying,                                                                
    how_difficult_do_you_find_making_decisions_about_livestock_type_stocking_rate_etc,                                                                
    how_difficult_do_you_find_making_decisions_about_crop_choice_type_mix_fallowing_etc,                                                             
    how_difficult_do_you_find_making_decisions_about_pre_emergent_herbicides,
    how_difficult_do_you_find_making_marketing_strategies_grain_sales_and_contracts_decisions,
    which_region_are_your_farming_activities_based,
    which_group_are_you_associated_with,
    what_is_your_risk_attitude_when_making,
    selected_risk_decision,
    advisor_consultant_agronomy,
    decision_support_tools_e_g_software_spreadsheets_etc,
    do_you_use_an_advisor_for_marketing_strategies_grain_sales_and_contracts_decisions,
    do_you_use_an_advisor_for_input_purchasing_timing_decisions,
    do_you_use_an_advisor_for_crop_choice_type_mix_fallowing_etc_decisions,
    do_you_use_an_advisor_for_livestock_decisions_type_stocking_rate_etc_decisions
  
  ) 

#selected_columns with short renamed


# Step 1: Select relevant columns
selected_columns <- cleaned_survey_data %>%
  select(
    nitrogen_decision        = how_difficult_do_you_find_making_decisions_about_nitrogen_application_amount_type_timing,
    machinery_decision       = how_difficult_do_you_find_making_decisions_about_buying_major_new_machinery_infrastructure,
    land_decision            = how_difficult_do_you_find_making_decisions_about_land_purchase_and_leasing,
    fungicide_decision       = how_difficult_do_you_find_making_decisions_about_preventative_fungicide_spraying,
    livestock_decision       = how_difficult_do_you_find_making_decisions_about_livestock_type_stocking_rate_etc,
    crop_choice_decision     = how_difficult_do_you_find_making_decisions_about_crop_choice_type_mix_fallowing_etc,
    herbicide_decision       = how_difficult_do_you_find_making_decisions_about_pre_emergent_herbicides,
    marketing_difficulty     = how_difficult_do_you_find_making_marketing_strategies_grain_sales_and_contracts_decisions,
    
    region                   = which_region_are_your_farming_activities_based,
    group                    = which_group_are_you_associated_with,
    risk_attitude            = what_is_your_risk_attitude_when_making,
    risk_decision            = selected_risk_decision,
    
    has_agronomy_advisor     = advisor_consultant_agronomy,
    uses_support_tools       = decision_support_tools_e_g_software_spreadsheets_etc,
    
    advisor_marketing        = do_you_use_an_advisor_for_marketing_strategies_grain_sales_and_contracts_decisions,
    advisor_input_timing     = do_you_use_an_advisor_for_input_purchasing_timing_decisions,
    advisor_crop_choice      = do_you_use_an_advisor_for_crop_choice_type_mix_fallowing_etc_decisions,
    advisor_livestock        = do_you_use_an_advisor_for_livestock_decisions_type_stocking_rate_etc_decisions
  )

# Step 2: View the renamed and selected columns
head(selected_columns)



ui <- fluidPage(
  titlePanel("RiskWise Decision Dashboard (Selected Columns)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region:", choices = c("All", sort(unique(selected_columns$region)))),
      selectInput("group", "Group:", choices = c("All", sort(unique(selected_columns$group)))),
      selectInput("question", "Select Question:",
                  choices = c(
                    "Nitrogen Decision" = "nitrogen_decision",
                    "Machinery Decision" = "machinery_decision",
                    "Land Decision" = "land_decision",
                    "Fungicide Decision" = "fungicide_decision",
                    "Livestock Decision" = "livestock_decision",
                    "Crop Choice Decision" = "crop_choice_decision",
                    "Herbicide Decision" = "herbicide_decision",
                    "Marketing Difficulty" = "marketing_difficulty"
                  ))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("bar_chart")),
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("Compare by Region", plotOutput("compare_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- selected_columns
    if (input$region != "All") df <- df[df$region == input$region, ]
    if (input$group != "All") df <- df[df$group == input$group, ]
    df
  })
  
  output$bar_chart <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$question)) +
      geom_bar(fill = "#0072B2") +
      coord_flip() +
      labs(title = paste("Responses for:", gsub("_", " ", input$question)),
           x = NULL, y = "Count") +
      theme_minimal()
  })
  
  output$data_table <- renderDT({
    filtered_data() %>%
      count(!!sym(input$question)) %>%
      rename(Response = !!sym(input$question), Count = n)
  })
  
  output$compare_plot <- renderPlot({
    ggplot(selected_columns, aes_string(x = input$question, fill = "region")) +
      geom_bar(position = "dodge") +
      labs(title = paste("Comparison by Region for:", gsub("_", " ", input$question)),
           x = NULL, y = "Count") +
      theme_minimal()
  })
}

shinyApp(ui, server)

