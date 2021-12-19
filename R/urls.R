api_baseurl <- function() {
  "https://www.eventbriteapi.com/v3"
}






api_keys_url <- function() {
  "https://www.eventbrite.com/platform/api-keys/"
}





.fUrl <- function(end) {
  oaurl <- "https://www.eventbrite.com"
  pth <- paste("oauth", end, sep = "/")
  .constructUrl(oaurl, pth)
}






## Path for the API URI
apiPath <- function(endpoint = NULL) {
  has.ep <- !is.null(endpoint)
  if (has.ep && !is.character(endpoint))
    stop("'endpoint' supplied should be of type 'character'")
  pth <- paste("v3/users/me", endpoint, sep = '/')
  if (has.ep)
    pth <- paste0(pth, "/")
  pth
}






#' @importFrom httr parse_url
#' @importFrom httr build_url
.constructUrl <- function(url, path) {
    url <- parse_url(url)
    url$path <- path
    build_url(url)
  }
