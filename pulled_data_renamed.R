
# Install necessary libraries if you haven't already
#if (!require(Microsoft365R)) install.packages("Microsoft365R")
#if (!require(dplyr)) install.packages("dplyr")
#if (!require(readr)) install.packages("readr")

library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)
library(dplyr)
library(DT)
library(readr)

pull_sharepoint_data <- function() {
  # 1. SharePoint Site URL
  sharepoint_site_url <- "https://farmvoice.sharepoint.com"
  
  # 2. The display name of your SharePoint List
  sharepoint_list_name <- "New Question type"
  
  data <- data.frame() # Initialize empty dataframe
  
  tryCatch({
    message("Attempting to connect to SharePoint site...")
    sharepoint_site <- get_sharepoint_site(site_url = sharepoint_site_url)
    message("✅ Successfully connected to site.")
    
    sharepoint_list <- sharepoint_site$get_list(sharepoint_list_name)
    message("Fetching list items...")
    
    data <- sharepoint_list$list_items()
    message("✅ Data pull successful.")
    
    return(data)
    
  }, error = function(e) {
    message("❌ An error occurred during SharePoint connection or data pull:")
    message(e)
    return(NULL)
  })
}

# --- Run Function to Get Data ---
d_data <- pull_sharepoint_data()


# --- Main Script Body ---
if (!is.null(d_data) && nrow(d_data) > 0) {
  
  # --- ✅ START: COMPLETE COLUMN RENAMING ---
  message("Renaming columns...")
  
  # This block maps the ugly internal SharePoint names to your desired clean names.
  d_data <- d_data %>%
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
      "What other risky decisions you have difficulty with?" = "field_19"
    )
  
  column_vector <- c(
    
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
    "Are you a part of a grower group participating in the RiskWi$e Initiative?",
    "Which group are you associated with?" = "Whichgroupareyouassociatedwith_x",
    "Workshop Date",
    "Workshop ID" ="WorkshopID",
    "Are you filling online or at an event?",
    "Is a specific risky decision being discussed today?",
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
    "Other sources (specify):",
    "What balance do you believe an Advisor/ Consultant is guided by when advising on" = "_x005b_Question7_x005d_Whatbalan",
    "What balance do you take between production or profitability expectations when making"= "_x005b_Question8_x005d_Whatbalan",
    "Which statement best describes how you approach your" = "_x005b_Question9_x005d_Whichstat",
    "Would you be interested in one-on-one decision coaching on",
    "Who is involved in making your" = "Whoisinvolvedinmakingyournitroge",
    "If Other: who involved in making your",
    "Are there any other comments you would like to make about risk management and decision-making",
    "Do you use an advisor for  Nitrogen Application (amount / type / timing) decisions?",
    "Do you use an advisor for  Buying major new (to your business) machinery/ infrastructure decisions?",
    "Do you use an advisor for  Land Purchase / Leasing Agreements decisions?",
    "Do you use an advisor for  Preventative fungicide sprays decisions?",
    "Do you use an advisor for  Livestock decisions (type / stocking rate / etc) decisions?",
    "Do you use an advisor for  Crop Choice (Type / Mix / Fallowing etc) decisions?",
    "Do you use an advisor for  Pre emergent herbicide choices decisions?",
    "Do you use an advisor for  Sowing times decisions?",
    "Do you use an advisor for  Varietal choices decisions?",
    "Do you use an advisor for  Input purchasing timing decisions?",
    "Do you use an advisor for  Marketing strategies (grain sales and contracts) decisions?",
    "Risk in Crop Selection: I am willing to plant crops with higher potential returns, even if they have a greater risk of failure.",
    "Investment in Equipment: I am comfortable making large financial investments in new farming equipment, even if the benefits are uncertain.",
    "Preparedness for Weather Risks: I prefer to invest in preventive measures to protect my farm from unpredictable weather, even if they are expensive.",
    "Crop Insurance: I am willing to pay for crop insurance even if I may not need it in most years.",
    "Market Price Fluctuations: I am comfortable selling my crops on the market, even if the prices are highly unpredictable.",
    "Adopting New Technologies: I tend to adopt new farming technologies or practices, even if they have not been widely tested.",
    "Farm Diversification: I prefer to diversify my farming activities to reduce the risk of income loss, even if it complicates farm management.",
    "Taking on Debt: I am willing to take on debt to expand my farming operations, even if it increases my financial risk.",
    "Response to Past Losses: After experiencing a significant loss in the past, I have become more cautious in my farming decisions.",
    "Long-Term Risk Tolerance: I am willing to take more risks now if it could mean higher returns in the future, even if the outcome is uncertain.",
    "If you have any more time, the below are optional questions that will help us improve the RiskWi$e Initiative."
  )
  
  length(column_vector)
  
  
  
  length(sharepoint_column_vector)
  message("✅ All columns successfully renamed.")
  # --- END: COMPLETE COLUMN RENAMING ---
  
  
  # --- START: FIX FOR LIST COLUMNS ---
  message("Checking for and converting any list-columns...")
  for (col_name in names(d_data)) {
    if (is.list(d_data[[col_name]])) {
      message(paste("Converting list-column:", col_name))
      d_data[[col_name]] <- sapply(d_data[[col_name]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        paste(unlist(x), collapse = "; ")
      })
    }
  }
  
  
  # --- START: SAVE TO CSV ---
  output_folder <- "SharePoint_Data"
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
    message(paste("📁 Created folder:", output_folder))
  }
  
  file_name <- file.path(output_folder, paste0("sharepoint_data_", Sys.Date(), ".csv"))
  
  # Write the cleaned data to a CSV file
  write.csv(d_data, file_name, row.names = FALSE, na = "")
  
  message(paste("✅ Success! Data saved to:", file_name))
  
  # Display the final, cleaned data in the RStudio viewer
  View(d_data)
  
} else {
  message("❌ Data pull was unsuccessful or returned no data. File not saved.")
}