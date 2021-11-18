base_url <- function() {
  "https://www.eventbriteapi.com/v3"
}


api_keys_url <- function() {
  "https://www.eventbrite.com/platform/api-keys/"
}



.fUrl <- function(end) {
  oaurl <- "https://www.eventbrite.com"
  pth <- paste0("oauth/", end)
  .constructUrl(oaurl, pth)
}



#' @importFrom httr parse_url
#' @importFrom httr build_url
.constructUrl <- function(url, path) {
    url <- parse_url(url)
    url$path <- path
    build_url(url)
  }