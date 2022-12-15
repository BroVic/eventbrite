#' Get a User object
#' 
#' @param token The token.
#' @param element The objects field.
#' 
#' @export
get_user <- function(token, element = NULL) 
{
  if (!is.character(token) && !inherits(token, "Token"))
    stop("'token' must be of class 'character' or 'Token'")
  c <- getContent(token, "users/me")
  if (!is.null(element)) {
    elem <- match.arg(element, c("id", "name")) # elements of 'User' object
    c <- c[[elem]]
  }
  c
}







get_organization <- function(token)
{
  # userid <- get_user(token, 'id')
  getContent(token, "users/me/organizations")
}




get_event <- function(token, orgid)
{
  path <- sprintf("organizations/%s/events", orgid)
  getContent(token, path)
}





get_attendee <- function(token, eventid)
{
  path <- sprintf("events/%s/attendees/", eventid)
  getContent(token, path)
}






#' @importFrom httr content
#' @importFrom httr stop_for_status
getContent <- function(token, path, ...)
{
  r <- getApiResponse(token, path, ...)
  stop_for_status(r)
  content(r, 'text')
}





# @param token The token, either an object of class \code{character} or 
# \code{Token}.
# @param endpoint The API endpoint, i.e. the path of a URL
getApiResponse <- function(token, ...)
  UseMethod("getApiResponse")






#' @importFrom httr GET
#' @importFrom httr config
getApiResponse.Token <- function(token, endpoint = character(), ...)
{
  url <- .buildApiCallUrl(endpoint) 
  GET(url, config = config(token = token), ...)
}






#' @importFrom httr GET
#' @importFrom httr add_headers
getApiResponse.character <- function(token, endpoint = character(), ...) 
{
  if (length(token) > 1L) {
    token <- token[1]
    warning("Only the first element of 'token' was used")
  }
  
  url <- .buildApiCallUrl(endpoint)
  
  try({
    GET(url, 
        config = add_headers(Authorization = paste("Bearer", token)),
        ...)
  })
}






getApiResponse.default <- function(token, ...)
{
  message(
    "No method defined for objects of class",
    sQuote(class(token))
  )
}








.buildApiCallUrl <- function(endpoint) {
  constructUrl(apiBaseUrl(), endpoint)
}
