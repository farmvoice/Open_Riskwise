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

# 2. Save the pulled data to a date-stamped CSV file
if (!is.null(d_data) && nrow(d_data) > 0) {
  
  file_name <- paste0("sharepoint_data_", Sys.Date(), ".csv")
  
  write.csv(d_data, file_name, row.names = FALSE)
  
  message(paste("✅ Data successfully saved as:", file_name))
  
} else {
  message("❌ Data pull was unsuccessful or returned no data. File not saved.")
}
