## Authenticate the Eventbrite personal key
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr parse_url
authenticate_apikey <- function(key) {
  url <- parse_url("https://www.eventbriteapi.com/v3/users/me")
  try({
    GET(url, add_headers(Authorization = paste("Bearer", key)))
  })
}
