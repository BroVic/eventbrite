# Source file: getApiResponse.R

# S3 Methods and helper functions for `getApiResponse`


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
    "No method defined for objects of class ",
    paste(sQuote(class(token)), collapse = ", ")
  )
}








.buildApiCallUrl <- function(endpoint) {
  constructUrl(apiBaseUrl(), endpoint)
}
