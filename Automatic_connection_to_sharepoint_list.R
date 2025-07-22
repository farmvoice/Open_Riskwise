library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)
library(DT)



pull_sharepoint_data <- function() {
  
  # 1. Correct Site URL (the root site)
  sharepoint_site_url <- "https://farmvoice.sharepoint.com"
  
  # 2. The display name of your list
  sharepoint_list_name <- "New Question type"
  
  data <- data.frame() # Initialize empty dataframe
  
  tryCatch({
    message("Attempting to connect to SharePoint site...")
    
    # Get the SharePoint site object
    sharepoint_site <- get_sharepoint_site(site_url = sharepoint_site_url)
    
    message("✅ Successfully connected to site.")
    
    # Get the specific list from the site
    sharepoint_list <- sharepoint_site$get_list(sharepoint_list_name)
    
    message("Fetching list items...")
    
    # Retrieve the data
    data <- sharepoint_list$list_items()
    
    message("✅ Data pull successful.")
    
    return(data)
    
  }, error = function(e) {
    # This will print the specific error message if something goes wrong
    message("❌ An error occurred:")
    message(e)
    return(NULL) # Return NULL or an empty dataframe on error
  })
}

# Run the function to get your data
d_data <- pull_sharepoint_data()
d_data



if (!is.null(d_data) && nrow(d_data) > 0) {
  
  # ✨ START: FIX FOR LIST COLUMNS ✨
  # Loop through each column of the dataframe
  for (col_name in names(d_data)) {
    # Check if the column is a list
    if (is.list(d_data[[col_name]])) {
      message(paste("Found list-column:", col_name, "-> converting to character."))
      # Convert each list element to a single character string, separated by semicolons
      d_data[[col_name]] <- sapply(d_data[[col_name]], function(x) {
        # If a cell is empty (NULL), represent it as NA
        if (is.null(x) || length(x) == 0) return(NA_character_)
        # Unlist and paste multiple entries together
        paste(unlist(x), collapse = "; ")
      })
    }
  }
  # 1. Define the name for your new folder
  output_folder <- "SharePoint_Data"
  
  # 2. Check if the folder exists and create it if it doesn't
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
    message(paste("📁 Created folder:", output_folder))
  }
  
  # 3. Create the full file path for the CSV
  file_name <- file.path(output_folder, paste0("sharepoint_data_", Sys.Date(), ".csv"))
  
  # ✨ END: NEW CODE FOR CREATING FOLDER ✨
  
  # Write the data to the new path
  write.csv(d_data, file_name, row.names = FALSE, na = "")
  
  message(paste("✅ Data successfully saved as:", file_name))
  
  }else {
    message("❌ Data pull was unsuccessful or returned no data. File not saved.")
  }



# 2. Save the pulled data to a date-stamped CSV file
# Process and save the data to a date-stamped CSV file
if (!is.null(d_data) && nrow(d_data) > 0) {
  
  # Fix for list-columns (from previous step)
  for (col_name in names(d_data)) {
    if (is.list(d_data[[col_name]])) {
      d_data[[col_name]] <- sapply(d_data[[col_name]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        paste(unlist(x), collapse = "; ")
      })
    }
  }
  
  # Create folder and save file (from previous step)
  output_folder <- "SharePoint_Data"
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  file_name <- file.path(output_folder, paste0("sharepoint_data_", Sys.Date(), ".csv"))
  write.csv(d_data, file_name, row.names = FALSE, na = "")
  
  message(paste("✅ Data successfully saved as:", file_name))
  
} else {
  message("❌ Data pull was unsuccessful or returned no data. File not saved.")
}

library(readr)
#sharepoint_data_2025_07_19 <- read_csv("sharepoint_data_2025-07-19.csv")
#View(sharepoint_data_2025_07_19)
View(d_data)
