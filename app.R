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
library(scales)


# Install necessary libraries if you haven't already
if (!require(Microsoft365R)) install.packages("Microsoft365R")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")

library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)
library(dplyr)
library(DT)
library(readr)


library(stringr)


# =================================================================
# 2. pull/download from sharepoint and Prepare Data
# =================================================================

#--------------------------------------------------------------------------------
#Commented Section For Brandon to access information for 
#For the server-server communication without user interaction (server-to-server authentication method) 
#--------------------------------------------------------------------------------
# =================================================================
# 2. pull/download from sharepoint and Prepare Data server-server communication
# =================================================================
pull_sharepoint_data <- function() {
  # --- Non-Interactive Authentication for Shiny App Deployment ---
  # In a server environment like Posit Connect, interactive browser-based
  # login is not possible. We use a service principal (app registration)
  # with client credentials for non-interactive authentication.
  #
  #   # IMPORTANT: Store these credentials as environment variables in your
  #   # deployment environment (e.g., Posit Connect). Do NOT hardcode them.
  #   # You will need to set the following variables in your Posit Connect dashboard's
  #   # 'Vars' tab:
  # - AZURE_TENANT_ID: Your Azure Active Directory Tenant ID
  # - AZURE_CLIENT_ID: The Application (client) ID for your app registration
  # - AZURE_CLIENT_SECRET: The client secret for your app registration
  
  tenant <- Sys.getenv("AZURE_TENANT_ID")
  app_id <- Sys.getenv("AZURE_CLIENT_ID")
  app_secret <- Sys.getenv("AZURE_CLIENT_SECRET")
  
  # 1. SharePoint Site URL
  sharepoint_site_url <- "https://farmvoice.sharepoint.com"
  
  # 2. The display name of your SharePoint List
  sharepoint_list_name <- "New Question type"
  sharepoint_list_name_client_id <- "Client ID"
  #   
  tryCatch({
    # Check if environment variables are set
    if (tenant == "" || app_id == "" || app_secret == "") {
      stop("Authentication Error: One or more environment variables (AZURE_TENANT_ID, AZURE_CLIENT_ID, AZURE_CLIENT_SECRET) are not set in the deployment environment.")
    }
    #     
    message("Attempting to connect to SharePoint site using client credentials...")
    #     
    #     # Authenticate non-interactively and get the SharePoint site
    sharepoint_site <- get_sharepoint_site(
      site_url = sharepoint_site_url,
      tenant = tenant,
      app = app_id,
      password = app_secret,
      auth_type = "client_credentials" # Specify the non-interactive flow
    )
    #     
    message("✅ Successfully connected to site.")
    #     
    sharepoint_list <- sharepoint_site$get_list(sharepoint_list_name)
    sharepoint_list2 <- sharepoint_site$get_list(sharepoint_list_name_client_id)
    message("Fetching list items...")
    #     
    #     # Pull data
    d_data <- sharepoint_list$list_items()
    client_data <- sharepoint_list2$list_items()
    message("✅ Data pull successful.")
    
    return(list(survey_data = d_data, client_info = client_data))
    #     
  }, error = function(e) {
    message("❌ An error occurred during SharePoint connection or data pull:")
    message(e)
    return(NULL)
  })
}
# 
#-------------------------------------------------------------------------
# End of commented section for Brendon to look fro
#----------------------------------------------------------------------------


sharepoint_output <- pull_sharepoint_data()

# FIX 3: Extract the data frames from the list into separate variables
# Check if the function ran successfully before trying to extract data
if (!is.null(sharepoint_output)) {
  d_data <- sharepoint_output$survey_data
  client_data <- sharepoint_output$client_info
  message("Data frames 'd_data' and 'client_data' have been created.")
}

# --- Main Script Body ---
if (!is.null(d_data) && nrow(d_data) > 0) {
  
  # --- ✅ START: COMPLETE COLUMN RENAMING ---
  message("Renaming columns...")
  
  # This block maps the ugly internal SharePoint names to your desired clean names.
  d_data_renamed <- d_data %>%
    rename(
      "ClientID"= "field_1",
      "How difficult do you find making decisions about nitrogen application (amount / type / timing)?"= "field_2",
      "How difficult do you find making decisions about buying major new machinery / infrastructure?"= "field_3",
      "How difficult do you find making decisions about land purchase and leasing?" ="field_4",
      "How difficult do you find making decisions about preventative fungicide spraying?" = "field_5",
      "How difficult do you find making decisions about livestock (type / stocking rate / etc)?"=  "field_6",
      "How difficult do you find making decisions about crop choice (type / mix / fallowing etc)?" = "field_7",
      "How difficult do you find making decisions about pre emergent herbicides?"= "field_8",
      "How difficult do you find making decisions about sowing timing?" = "field_9",
      "How difficult do you find making decisions about varietal choice?" = "field_10",
      "How difficult do you find making decisions about input purchasing timing?" = "field_11",
      "How difficult do you find making decisions about marketing (grain sales and contracts)?"= "MarketingStrategies_x0028_Grains",
      "How difficult do you find making decisions about water buying and use?" ="HowdifficultdoyoufindmakingWater",
      "How difficult do you find making decisions about soil health management?" = "field_13",
      "How difficult do you find making decisions about succession planning?" = "field_14",
      "How difficult do you find making decisions about net zero adaptation?" = "field_15",
      "How difficult do you find making decisions about stored soil water?" = "field_16",
      "How difficult do you find making decisions about sodicity management?" = "field_17",
      "How difficult do you find making decisions about deep P application?" ="field_18",
      "What other risky decisions you have difficulty with?" = "field_19",
      "What should RiskWi$e focus on to help growers make better farm decisions?" = "WhatshouldRiskWi_x0024_efocusont",
      "What areas in your decision making would you like to improve?" = "Whatareasinyourdecisionmakingwou",
      "Which region are your farming activities based?"= "Whichregionareyourfarmingactivit",
      "Which group are you associated with?" = "Whichgroupareyouassociatedwith_x",
      "Which Risky Decision is today's event discussing (Facilitator will advise)." = "WhichRiskyDecisionistodayseventd",
      "Which of the following crop management decisions do you have most difficulty in assessing for risk?" ="Whichofthefollowingcropmanagemen",
      "Selected Risk Decision"= "SelectedRiskDecision",
      "What is your risk attitude when making"= "_x005b_Question2_x005d_Whatisyou",
      "What balance guides you between intuition (gut-feel informed by past experience) and numerical calculation (data-driven) when making a" = "_x005b_Question3_x005d_Whatbalan",
      "How comfortable are you with your decision process (not outcome) when making current" = "_x005b_Question4_x005d_Howcomfor",
      "Have you undertaken any review of how you made your last" = "_x005b_Question5_x005d_Haveyouun",
      "Advisor/ Consultant (Agronomy)" = "Advisor_x002f_Consultant_x0028_A",
      "Farm Business Advisor / Consultant" = "FarmBusinessAdvisor_x002f_Consul",
      "Decision Support Tools (e.g. software / spreadsheets etc.)" = "DecisionSupportTools_x0028_e_x00",
      "Other Growers" = "OtherGrowers",
      "other" = "other",
      "What balance do you believe an Advisor/ Consultant is guided by when advising on" = "_x005b_Question7_x005d_Whatbalan",
      "What balance do you take between production or profitability expectations when making"= "_x005b_Question8_x005d_Whatbalan",
      "Which statement best describes how you approach your" = "_x005b_Question9_x005d_Whichstat",
      "Who is involved in making your" = "Whoisinvolvedinmakingyournitroge",
    )
  
  client_data_renamed <- client_data %>%
    rename(
      "RiskWi$e ID"= "field_1",
      "What is your first name?"= "field_2", 
      "What is your last name?" = "field_3",
      "What is your postcode?" = "field_4", 
      "What is your email?" = "field_10",
      "What is your gender?" = "field_9",
      "Are you a Grower (Land Owner)?" = "Grower_x0028_owner_x0029_",
      "Are you a Grower (Non Owner)?" = "Grower_x0028_NonlandOwner_x0029_",
      "Are you an Advisor / Consultant?" = "Advisor",
      "Do you have other relevant roles?" = "Other",
      "Please specify your other roles" = "Other_x0028_specify_x0029__x003a",
      "Which category best describes your farm operations?" = "field_7",
      "What is the size of dryland area (in ha) that you typically manage?" = "Whatisthesizeofnon_x002d_dryland",
      "What is the size of Irrigated area (in ha) that you typically manage?" = "WhatisthesizeofirrigatedAreayoum",
      "What is the size of cropped area (in ha) that you typically manage?" = "field_5",
      "What is your average Summer rainfall (in mm)?" = "WhatisyouraverageSummerrainfall_",
      "What is the average winter rainfall (in mm) of cropped area that you typically manage?" = "WhatisyouraverageWinterrainfall_",
      "What is the average annual rainfall (in mm) of cropped area that you typically manage?" = "field_6",
      
      "Highest education attained?" = "Highesteducationattained_x003f_",
      "Number of years since you started your farming career?" = "Numberofyearssinceyoustartedyour",
      "Number of full-time equivalent employees (including owner)?" = "Numberoffull_x002d_timeequivalen",
      "Type of business structure?" = "Typeofbusinessstructure_x003f_"
    )
  
  message("✅ All columns successfully renamed.")
  # --- END: COMPLETE COLUMN RENAMING ---
  
  
  # --- START: FIX FOR LIST COLUMNS ---
  message("Checking for and converting any list-columns...")
  for (col_name in names(d_data_renamed)) {
    if (is.list(d_data_renamed[[col_name]])) {
      message(paste("Converting list-column:", col_name))
      d_data_renamed[[col_name]] <- sapply(d_data_renamed[[col_name]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        paste(unlist(x), collapse = "; ")
      })
    }
  }
  
  # --- START: FIX FOR LIST COLUMNS ---
  message("Checking for and converting any list-columns in client_data information ...")
  for (col_name in names(client_data_renamed)) {
    if (is.list(client_data_renamed[[col_name]])) {
      message(paste("Converting list-column:", col_name))
      client_data_renamed[[col_name]] <- sapply(client_data_renamed[[col_name]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        paste(unlist(x), collapse = "; ")
      })
    }
  }
  
  
  #message("Checking for and converting any list-columns client_info...")
  #for (col_name in names(client_data)) {
  #    if (is.list(client_data[[col_name]])) {
  #     message(paste("Converting list-column:", col_name))
  #    client_data[[col_name]] <- sapply(client_data[[col_name]], function(x) {
  #     if (is.null(x) || length(x) == 0) return(NA_character_)
  #    paste(unlist(x), collapse = "; ")
  # })
  #}
  #}
  
  
  # --- START: SAVE TO CSV ---
  output_folder <- "SharePoint_Data"
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
    message(paste("📁 Created folder:", output_folder))
  }
  
  file_name <- file.path(output_folder, paste0("sharepoint_data_", Sys.Date(), ".csv"))
  cleint_info <- file.path(output_folder, paste0("client_id_", Sys.Date(), ".csv"))
  # Write the cleaned data to a CSV file
  write.csv(d_data_renamed, file_name, row.names = FALSE, na = "")
  write.csv(client_data_renamed, cleint_info, row.names = FALSE, na = "")
  
  message(paste("✅ Success! Data saved to:", file_name))
  
  
  
} else {
  message("❌ Data pull was unsuccessful or returned no data. File not saved.")
}




# =================================================================
# 2. Load recently created data and Prepare Data
# =================================================================
# Load base survey data and client identifier data
# Please ensure these file paths are correct for your environment

# Read the dataset
#raw_data <- read_csv("Riskwise_data/New_Question_type_raw.csv")

raw_data <- read_csv(file_name, show_col_types = FALSE)
# Get the original column names
original_colnames <- colnames(raw_data)

# Clean the column names
# 1. Convert to lowercase
# 2. Replace all non-alphanumeric characters with an underscore
# 3. Replace multiple consecutive underscores with a single one
# 4. Remove any trailing underscores
cleaned_colnames <- original_colnames %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", "_") %>%
  str_replace_all("_+", "_") %>%
  str_remove("_$")

# Assign the new, cleaned column names to the data frame
colnames(raw_data) <- cleaned_colnames

# --- Verification ---
# Print the new column names to verify the changes
print(colnames(raw_data))

# Display the first few rows of the data with the new column names
#head(raw_data)

# --- Save the result ---
# Save the data with renamed columns to a new CSV file
file_name_cleaned <- file.path(output_folder, paste0("cleaned_sharepoint_data_", Sys.Date(), ".csv"))
# Write the cleaned data to a CSV file
write.csv(raw_data, file_name_cleaned, row.names = FALSE, na = "")


#=================================================================================================
#Now comes to Next part of Dataset Prepration
#==================================================================================================

#survey_data <- read_csv("Riskwise_data/cleaned_New_Question_type_raw.csv", show_col_types = FALSE)

#survey_data <- read_csv("Riskwise_data/cleaned_sharepoint_data_2025-07-25.csv", show_col_types = FALSE)
survey_data <- read_csv(file_name_cleaned, show_col_types = FALSE)

#survey_client_id <- read_csv("Riskwise_data/Client_id_B.csv", show_col_types = FALSE)
survey_client_id <- read_csv(cleint_info, show_col_types = FALSE)

# Add the 'role' Column based on client responses
client_data_with_roles <- survey_client_id %>%
  mutate(
    role = case_when(
      (`Are you a Grower (Land Owner)?` == "Yes" & !is.na(`Are you a Grower (Land Owner)?`)) |
        (`Are you a Grower (Non Owner)?` == "Yes" & !is.na(`Are you a Grower (Non Owner)?`))   ~ "Grower",
      `Are you an Advisor / Consultant?` == "Yes" & !is.na(`Are you an Advisor / Consultant?`) ~ "Advisor",
      TRUE ~ "Other"
    )
  )

# Select and prepare the role data for joining
roles_to_join <- client_data_with_roles %>%
  select(clientid = `RiskWi$e ID`, role)
#select(ClientID = `RiskWi$e ID`, role)
roles_to_join
# Join the role information to the main survey dataset

survey_data <- survey_data %>%
  left_join(roles_to_join, by = "clientid") %>%
  # FIX 1: Explicitly handle NA values that result from the left_join.
  # This converts any client without a defined role to "Other".
  mutate(role = replace_na(role, "Other"))


# a bit cleaning of duplicates
message(paste("Number of rows after joining with roles data:", nrow(survey_data)))
# Keep only the first occurrence of each unique client_id
survey_data <- survey_data %>%
  distinct(clientid, .keep_all = TRUE)
message(paste("Number of rows after removing duplicates:", nrow(survey_data)))


# --- A: Prepare "Decision Ranking" Data ---
# Define the valid levels first to avoid repeating them and ensure consistency
difficulty_levels_vector <- c("Not Difficult", "Somewhat Difficult", "Very Difficult")

survey_data

decision_ranking_long <- survey_data %>%
  select(clientid,
         starts_with("how_difficult"),
         region = which_region_are_your_farming_activities_based,
         grower_group = which_group_are_you_associated_with,
         role) %>%
  pivot_longer(cols = starts_with("how_difficult"), names_to = "decision_name", values_to = "difficulty_level") %>%
  mutate(
    decision_name = str_to_title(str_replace_all(str_remove(decision_name, "how_difficult_do_you_find_making_decisions_about_"), "_", " ")),
    decision_name = str_replace(decision_name, "Etc", "(etc)"),
    decision_name = str_replace(decision_name, "Type Mix Fallowing", "(Type / Mix / Fallowing etc)"),
    decision_name = str_replace(decision_name, "Marketing Grain Sales And Contracts", "Marketing Strategies (Grain sales and contracts)")
  ) %>%
  # FIX 2: Make the filtering more robust. This will now remove any unexpected values
  # BEFORE the factor() conversion, preventing the creation of new NAs.
  filter(
    !is.na(decision_name),
    decision_name != "",
    !is.na(difficulty_level),
    difficulty_level %in% difficulty_levels_vector
  ) %>%
  mutate(
    # This factor conversion is now completely safe and cannot create NAs.
    difficulty_level = factor(difficulty_level, levels = difficulty_levels_vector)
  )

# --- B: Prepare "Risk Attitude" and other data ---
risk_attitude_df <- survey_data %>%
  select(
    clientid,
    what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    what_is_your_risk_attitude_when_making,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision,
    role
  ) %>%
  rename(
    intuition_vs_data      = what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    advisor_balance        = what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    production_vs_profit = what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    comfort_with_process = how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    risk_attitude          = what_is_your_risk_attitude_when_making
  ) %>%
  mutate(across(c(intuition_vs_data, advisor_balance, production_vs_profit, comfort_with_process, risk_attitude), ~replace_na(., 0)))

# --- Count and Replace NA values in risk_attitude_df ---
numeric_score_columns <- c(
  "intuition_vs_data", "advisor_balance", "production_vs_profit",
  "comfort_with_process", "risk_attitude"
)
risk_attitude_df <- risk_attitude_df %>%
  mutate(across(all_of(numeric_score_columns), ~replace_na(., 0)))


# --- C. Prepare "Review Process" Data ---
review_process_df <- survey_data %>%
  select(
    clientid,
    review_type = have_you_undertaken_any_review_of_how_you_made_your_last,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision,
    role
  ) %>%
  filter(!is.na(review_type))

# --- D. Prepare Filter Choices and Global Variables ---
all_regions <- c("All Regions", sort(unique(na.omit(survey_data$which_region_are_your_farming_activities_based))))
all_groups <- c("All Groups", sort(unique(na.omit(survey_data$which_group_are_you_associated_with))))
all_roles <- c("All Roles", "Grower", "Advisor", "Other")
decision_choices <- c("All Decisions", sort(unique(na.omit(survey_data$selected_risk_decision))))
client_choices <- sort(unique(na.omit(risk_attitude_df$clientid)))

plot_vars <- c("Risk Attitude" = "risk_attitude", "Decision Ranking" = "decision_ranking",
               "Review Process" = "review_process",
               "Intuition vs. Data" = "intuition_vs_data", "Comfort with Process" = "comfort_with_process",
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
  
  fluidRow(
    column(3,
           wellPanel(
             selectInput("variable_selector", "Select Metric to Display:",
                         choices = names(plot_vars), selected = "Risk Attitude")
           )
    ),
    column(3,
           wellPanel(
             conditionalPanel(
               condition = "input.variable_selector != 'Decision Ranking'",
               selectInput("compare_mode", "View Mode:",
                           choices = c("Single Population" = "single",
                                       "Compare two populations" = "compare_two",
                                       "Compare me to my peers" = "compare_me"),
                           selected = "single")
             )
           )
    ),
    column(3,
           wellPanel(
             uiOutput("pop1_filter_header"),
             selectInput("region_filter1", "Region:", choices = all_regions),
             selectInput("group_filter1", "Group:", choices = all_groups),
             selectInput("role_filter1", "Role:", choices = all_roles),
             conditionalPanel(
               condition = "!['Decision Ranking', 'Review Process'].includes(input.variable_selector)",
               selectInput("decision_filter1", "Specific Decision:", choices = decision_choices)
             )
           )
    ),
    column(3,
           conditionalPanel(
             condition = "input.variable_selector != 'Decision Ranking'",
             conditionalPanel(
               condition = "input.compare_mode == 'compare_me'",
               
               wellPanel(
                 h4("My Response Selector", align = "center"),
                 selectInput("client_selector", "Select Your ID:", choices = client_choices),
                 hr(),
                 uiOutput("user_response_text_ui")
               )
             ),
             conditionalPanel(
               condition = "input.compare_mode == 'compare_two'",
               wellPanel(
                 uiOutput("pop2_filter_header"),
                 selectInput("region_filter2", "Region:", choices = all_regions, selected = all_regions[1]),
                 selectInput("group_filter2", "Group:", choices = all_groups),
                 selectInput("role_filter2", "Role:", choices = all_roles),
                 conditionalPanel(
                   condition = "input.variable_selector != 'Review Process'",
                   selectInput("decision_filter2", "Specific Decision:", choices = decision_choices)
                 )
               )
             )
           )
    )
  ),
  hr(),
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
  
  create_decision_ranking_plot <- function(data, colors) {
    if(is.null(data) || nrow(data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No ranking data available for the selected filters.", size = 6) +
               theme_void())
    }
    
    summary_ranking <- data %>%
      count(decision_name, difficulty_level, .drop = FALSE) %>%
      group_by(decision_name) %>%
      filter(sum(n) > 0) %>%
      mutate(percentage = n / sum(n)) %>%
      ungroup() %>%
      group_by(decision_name) %>%
      mutate(sort_metric = sum(percentage[difficulty_level == "Very Difficult"])) %>%
      ungroup() %>%
      mutate(decision_name = fct_reorder(decision_name, sort_metric))
    
    ggplot(summary_ranking, aes(y = decision_name, x = percentage, fill = difficulty_level)) +
      geom_col() +
      scale_x_continuous(labels = scales::percent, expand = c(0, 0.01)) +
      scale_fill_manual(values = colors, name = "Difficulty Level") +
      labs(
        title = "Decision Ranking by Assessed Difficulty",
        x = "Percentage of Respondents",
        y = "Decision Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")
      )
  }
  
  observeEvent(input$region_filter1, {
    selected_region <- input$region_filter1
    if (selected_region == "All Regions") {
      choices <- all_groups
    } else {
      choices <- c("All Groups", region_group_map[[selected_region]])
    }
    updateSelectInput(session, "group_filter1", choices = choices)
  })
  
  observeEvent(input$region_filter2, {
    selected_region <- input$region_filter2
    if (selected_region == "All Regions") {
      choices <- all_groups
    } else {
      choices <- c("All Groups", region_group_map[[selected_region]])
    }
    updateSelectInput(session, "group_filter2", choices = choices)
  })
  
  observeEvent(input$variable_selector, {
    if (input$variable_selector == "Decision Ranking") {
      updateSelectInput(session, "compare_mode", selected = "single")
    }
  }, ignoreInit = TRUE)
  
  filtered_data <- function(pop_num = 1) {
    role_filter      <- if(pop_num == 1) input$role_filter1 else input$role_filter2
    region_filter    <- if(pop_num == 1) input$region_filter1 else input$region_filter2
    group_filter     <- if(pop_num == 1) input$group_filter1 else input$group_filter2
    decision_filter <- if(pop_num == 1) input$decision_filter1 else input$decision_filter2
    
    data <- switch(input$variable_selector, "Decision Ranking" = decision_ranking_long, "Review Process" = review_process_df, risk_attitude_df)
    
    if (!is.null(role_filter) && role_filter != "All Roles") {
      data <- data %>% filter(role == role_filter)
    }
    if (!is.null(region_filter) && region_filter != "All Regions") {
      data <- data %>% filter(region == region_filter)
    }
    if (!is.null(group_filter) && group_filter != "All Groups") {
      data <- data %>% filter(grower_group == group_filter)
    }
    if (!is.null(decision_filter) && !input$variable_selector %in% c("Decision Ranking", "Review Process") && decision_filter != "All Decisions") {
      data <- data %>% filter(risk_decision == decision_filter)
    }
    
    return(data)
  }
  
  selected_user_data <- reactive({
    req(input$compare_mode == "compare_me", input$client_selector)
    df <- if (input$variable_selector == "Review Process") review_process_df else risk_attitude_df
    df %>% filter(clientid == input$client_selector)
  })
  
  plot_population_sizes <- reactive({
    get_size <- function(data_df) {
      if (is.null(data_df) || nrow(data_df) == 0 || !"clientid" %in% names(data_df)) {
        return(0)
      }
      return(n_distinct(data_df$clientid))
    }
    
    pop1_data <- filtered_data(1)
    pop1_size <- get_size(pop1_data)
    
    pop2_size <- NULL
    if (input$compare_mode == "compare_two") {
      pop2_data <- filtered_data(2)
      pop2_size <- get_size(pop2_data)
    }
    list(pop1_size = pop1_size, pop2_size = pop2_size)
  })
  
  population_names <- reactive({
    get_pop_name <- function(num) {
      region <- input[[paste0("region_filter", num)]]
      group <- input[[paste0("group_filter", num)]]
      
      if (!is.null(group) && group != "All Groups") { return(group) }
      if (!is.null(region) && region != "All Regions") { return(region) }
      return(paste("Population", num))
    }
    
    list(
      pop1 = get_pop_name(1),
      pop2 = if (input$compare_mode == "compare_two") get_pop_name(2) else NULL
    )
  })
  
  output$pop1_filter_header <- renderUI({
    req(population_names()$pop1)
    title <- paste0(population_names()$pop1, " (n=", plot_population_sizes()$pop1_size, ")")
    h4(title, align = "center")
  })
  
  output$pop2_filter_header <- renderUI({
    req(input$compare_mode == 'compare_two', population_names()$pop2)
    title <- paste0(population_names()$pop2, " (n=", plot_population_sizes()$pop2_size, ")")
    h4(title, align="center")
  })
  
  output$user_response_text_ui <- renderUI({
    req(input$compare_mode == 'compare_me')
    user_data <- selected_user_data()
    response_text <- if (input$variable_selector == "Review Process") {
      user_data$review_type[1]
    } else if (input$variable_selector != "Decision Ranking") {
      user_data[[plot_vars[input$variable_selector]]][1]
    } else {
      return(NULL)
    }
    if (is.na(response_text)) return(p("No response recorded."))
    div(p(strong("Your ID:"), br(), input$client_selector), p(strong("My Response:"), br(), paste0("'", response_text, "'")))
  })
  
  get_summary_stats <- function(df) {
    risk_data <- df %>% filter(!is.na(risk_attitude))
    if(nrow(risk_data) == 0) return(NULL)
    
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    
    risk_data %>%
      mutate(category = factor(case_when(
        risk_attitude <= 1 ~ "Very Risk Avoidant",
        risk_attitude <= 3 ~ "Risk Avoidant",
        risk_attitude <= 6 ~ "Neutral",
        risk_attitude <= 8 ~ "Risk Tolerant",
        TRUE ~ "Very Risk Tolerant"
      ), levels = category_order)) %>%
      count(category, .drop = FALSE) %>%
      mutate(percentage = round(n / sum(n) * 100))
  }
  
  create_summary_box_ui <- function(summary_stats, title) {
    if (is.null(summary_stats)) {
      return(wellPanel(h4(title, align = "center"), hr(), p("No data available for the selected filters.")))
    }
    
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    
    summary_cols <- lapply(category_order, function(cat) {
      percent_val <- summary_stats$percentage[summary_stats$category == cat]
      
      column(
        2,
        style = "border:1px solid #ddd; text-align:center; padding:5px; margin:0 5px;",
        tags$b(cat),
        tags$p(paste0(percent_val, "%"))
      )
    })
    
    wellPanel(
      h4(title, align = "center"),
      hr(),
      fluidRow(column(1), summary_cols)
    )
  }
  
  output$summary_output_ui <- renderUI({
    req(input$variable_selector == "Risk Attitude")
    
    if (input$compare_mode == "single") {
      stats <- get_summary_stats(filtered_data(1))
      ui <- create_summary_box_ui(stats, title = paste("Summary for", population_names()$pop1))
      fluidRow(column(12, ui))
      
    } else if (input$compare_mode == "compare_two") {
      stats1 <- get_summary_stats(filtered_data(1))
      stats2 <- get_summary_stats(filtered_data(2))
      
      div(
        create_summary_box_ui(stats1, title = population_names()$pop1),
        create_summary_box_ui(stats2, title = population_names()$pop2)
      )
    }
  })
  
  # =================================================================
  # ==== Main Plotting Logic ========================================
  # =================================================================
  
  output$main_plot <- renderPlot({
    pop1_name <- population_names()$pop1
    pop2_name <- population_names()$pop2
    
    if (input$variable_selector == "Review Process") {
      if (input$compare_mode == "single" || input$compare_mode == "compare_me") {
        plot_data <- filtered_data(1) %>% count(review_type) %>% arrange(desc(review_type)) %>%
          mutate(percentage = n / sum(n), ypos = cumsum(percentage) - 0.5 * percentage, label_text = scales::percent(percentage, accuracy = 1))
        if(nrow(plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No data for selected filters.", size=6) + theme_void())
        ggplot(plot_data, aes(x = 2, y = percentage, fill = review_type)) +
          geom_col(width = 1, color = "white") + coord_polar(theta = "y", start = 0) +
          geom_label_repel(aes(y = ypos, label = label_text), size = 6, show.legend = FALSE, nudge_x = 1, segment.color = "black") +
          scale_fill_brewer(palette = "Pastel1", name = "Review Process", labels = function(x) str_wrap(x, width = 25)) +
          xlim(0.5, 3.5) + theme_void() + theme(legend.title=element_text(face="bold", size=14), legend.text = element_text(size=12))
      } else if (input$compare_mode == "compare_two") {
        data1 <- filtered_data(1) %>% count(review_type) %>% mutate(percentage = n/sum(n), population = pop1_name)
        data2 <- filtered_data(2) %>% count(review_type) %>% mutate(percentage = n/sum(n), population = pop2_name)
        plot_data <- bind_rows(data1, data2)
        
        if(nrow(plot_data) > 0) {
          plot_data <- plot_data %>%
            mutate(population = factor(population, levels = c(pop1_name, pop2_name)))
        }
        
        if(nrow(plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No data.", size=6) + theme_void())
        ggplot(plot_data, aes(x=review_type, y=percentage, fill=population)) +
          geom_col(position = "dodge") + scale_y_continuous(labels = scales::percent) +
          scale_x_discrete(labels = function(x) str_wrap(x, width=15)) +
          labs(title = "Comparison of Review Processes", x = "Review Type", y = "Percentage of Respondents", fill = "Population") +
          theme_minimal(base_size = 14) + theme(legend.position="bottom", plot.title=element_text(hjust=0.5, face="bold"))
      }
    } else {
      selected_var_name <- plot_vars[input$variable_selector]
      all_scores_template <- tibble(value = 0:10)
      if (input$compare_mode == "compare_two") {
        process_plot_data <- function(df, pop_name, var) {
          df_filtered <- df %>% filter(!is.na(.data[[var]]))
          total_n <- n_distinct(df_filtered$clientid)
          if (total_n == 0) {
            return(all_scores_template %>% mutate(percentage = 0, population = pop_name))
          }
          df_filtered %>%
            count(.data[[var]], name = "n") %>%
            rename(value = 1) %>%
            right_join(all_scores_template, by = "value") %>%
            mutate(n = replace_na(n, 0), percentage = n / total_n, population = pop_name)
        }
        combined_data <- bind_rows(process_plot_data(filtered_data(1), pop1_name, selected_var_name),
                                   process_plot_data(filtered_data(2), pop2_name, selected_var_name))
        
        if(nrow(combined_data) > 0) {
          combined_data <- combined_data %>%
            mutate(population = factor(population, levels = c(pop1_name, pop2_name)))
        }
        
        if(nrow(combined_data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
        ggplot(combined_data, aes(x=value, y=percentage, color=population)) +
          geom_line(size=1.2) + geom_point(size=4) +
          scale_x_continuous(limits=c(-0.3,10.5), breaks=seq(0,10,2), expand = c(0,0)) +
          scale_y_continuous(labels=scales::percent, expand = expansion(mult = c(0.03, .15))) +
          labs(title=paste("Comparison of", input$variable_selector), x="Score (0-10)", y="Percentage of Respondents", color="Population") +
          theme_minimal(base_size=15) +
          theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom")
      } else if (input$compare_mode == "compare_me") {
        req(input$variable_selector != "Decision Ranking")
        peers_data <- filtered_data(1)
        user_data <- selected_user_data()
        user_value <- user_data[[selected_var_name]]
        peers_filtered <- peers_data %>% filter(!is.na(.data[[selected_var_name]]))
        total_n_peers <- n_distinct(peers_filtered$clientid)
        peers_plot_data <- if (total_n_peers == 0) {
          all_scores_template %>% mutate(percentage = 0)
        } else {
          peers_filtered %>%
            count(.data[[selected_var_name]], name = "count") %>% rename(value = 1) %>%
            right_join(all_scores_template, by = "value") %>%
            mutate(count = replace_na(count, 0), percentage = count / total_n_peers)
        }
        if (nrow(peers_plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No peer data.", size=6) + theme_void())
        p <- ggplot(peers_plot_data, aes(x = value, y = percentage)) +
          geom_line(color = "#006699", size = 1.2) + geom_point(color = "#006699", size = 4) +
          scale_x_continuous(limits = c(-0.5, 10.5), breaks = seq(0, 10, 2)) +
          scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.03, .15))) +
          labs(title = paste("Your Response vs. Peer Distribution for", input$variable_selector), x = "Score (0-10)", y = "Percentage of Respondents") +
          theme_minimal(base_size = 15) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        if (length(user_value) > 0 && !is.na(user_value)) {
          band_width <- 0.5
          p <- p +
            geom_rect(aes(xmin = user_value - band_width, xmax = user_value + band_width, ymin = 0, ymax = Inf),
                      fill = "#006699", alpha = 0.05) +
            geom_text(aes(x = user_value, y = (max(peers_plot_data$percentage, 0.1) * 0.5)),
                      label = "My Response", color = "#006699", fontface = "bold", size = 6.5, angle = 90)
        }
        p
      } else {
        data <- filtered_data(1)
        if (input$variable_selector == "Decision Ranking") {
          
          create_decision_ranking_plot(data, difficulty_colors)
          
        } else {
          if(nrow(data)==0 || all(is.na(data[[selected_var_name]]))) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
          data_filtered <- data %>% filter(!is.na(.data[[selected_var_name]]))
          total_n <- n_distinct(data_filtered$clientid)
          plot_data <- if(total_n == 0) {
            all_scores_template %>% mutate(percentage = 0)
          } else {
            data_filtered %>%
              count(.data[[selected_var_name]], name="count") %>% rename(value=1) %>%
              right_join(all_scores_template, by = "value") %>%
              mutate(count = replace_na(count, 0), percentage = count / total_n)
          }
          ggplot(plot_data, aes(x=value, y=percentage)) +
            geom_line(color="#006699",size=1.2) + geom_point(color="#006699",size=4) +
            scale_x_continuous(limits=c(-0.3,10.5), breaks=seq(0,10,2), expand = c(0,0)) +
            scale_y_continuous(labels=scales::percent, expand = expansion(mult = c(0.03, 0.15))) +
            labs(title=paste("Distribution of",input$variable_selector), x="Score (0-10)", y="Percentage of Respondents") +
            theme_minimal(base_size=15) + theme(plot.title=element_text(hjust=0.5,face="bold"))
        }
      }
    }
  })
}

# =================================================================
# 5. Run the Shiny App
# =================================================================
shinyApp(ui = ui, server = server)
