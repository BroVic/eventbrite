#' Get a User object
#' 
#' @importFrom httr content
#' @importFrom httr stop_for_status
#' 
#' @param token The token.
#' @param element The objects field.
#' 
#' @export
get_user <- function(token, element = NULL) 
{
  if (!is.character(token) && !inherits(token, "Token"))
    stop("'token' must be of class 'character' or 'Token'")
  r <- getResource(token)
  r <- stop_for_status(r)
  c <- content(r)
  if (!is.null(element)) {
    elem <- match.arg(element, c("id", "name")) # elements of 'User' object
    c <- c[[elem]]
  }
  invisible(c)
}







# @param token The token, either an object of class \code{character} or 
# \code{Token}.
# @param endpoint The API endpoint, i.e. the path of a URL
getResource <- function(token, ...)
  UseMethod("getResource")






#' @importFrom httr GET
#' @importFrom httr config
getResource.Token <- function(token, endpoint = NULL, ...)
{
  url <- .build_call_url(endpoint) 
  GET(url, config = config(token = token), ...)
}






#' @importFrom httr GET
#' @importFrom httr add_headers
getResource.character <- function(token, endpoint = NULL, ...) 
{
  if (length(token) > 1L) {
    token <- token[1]
    warning("Only the first element of 'token' was used")
  }
  
  url <- .build_call_url(endpoint)
  
  try({
    GET(url, 
        config = add_headers(Authorization = paste("Bearer", token)),
        ...)
  })
}






getResource.default <- function(token, ...)
{
  message(
    "A method has not been defined for objects of class",
    sQuote(class(token))
  )
}






.validate_endpoint <- function(ep) {
  if (is.null(ep))
    return(apiPath(ep))
  if (!is.character(ep))
    stop("Argumeents passed as endpoints must of of type 'character'",
         call. = FALSE)
  invisible(ep)
}






.build_call_url <- function(endpoint) {
  ep <- .validate_endpoint(endpoint)
  constructUrl(apiBaseUrl(), ep)
}
