library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)
library(DT)

sharepoint_site_url <- "https://farmvoice.sharepoint.com/sites/Communication site"
sharepoint_list_name <- "New Question type"

#Microsoft365R::AzureAuth::set_azure_token_cache_dir("~/Library/Application Support/AzureR")


pull_sharepoint_data <- function() {
  
  data <- data.frame() #initalize empty dataframe
  tryCatch({
    message("attempt to connect")
    sharepoint_siteC <- get_sharepoint_site(site_url = sharepoint_site_url)
    message("connected to site")
    
    sharepoint_list <- sharepoint_siteC$get_list(sharepoint_list_name)
    
    data <- sharepoint_list$list_items()
    return(data)
  })
  
}


d_data <- pull_sharepoint_data()
