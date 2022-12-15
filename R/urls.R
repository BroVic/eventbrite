hostUrl <- function() 
{
  "https://www.eventbrite.com"
}



apiBaseUrl <- function()
{
  "https://www.eventbriteapi.com/v3"
}






api_keys_url <- function() 
{
  "https://www.eventbrite.com/platform/api-keys/"
}






## Path for the API URI
# apiPath <- function(...) {
#   pth <- c(...)
#   paste(pth, collapse = '/')
# }





# Creates the URLs for various actions e.g. setting OAuth and making
# API calls. This bundles URL handling by httr functions and takes care
# of the fact that the API's base URL has a path (i.e. /v3) and has to
# be preserved when creating the URL for making the calls
#' @importFrom httr parse_url
#' @importFrom httr build_url
constructUrl <- function(url, path) 
{
  stopifnot(is.character(url), is.character(path))
  url <- parse_url(url)
  url$path <- if (grepl("^v3", url$path))
    paste(url$path, path, sep = '/')
  else
    path
  build_url(url)
}
