#' Authorize the Eventbrite personal key
#' 
#' @param token The access token
#' 
#' @importFrom httr GET
#' @importFrom httr add_headers
#' 
#' @export
authorize_api_key <- function(token)
  UseMethod(("authorize_api_key"))



#' @importFrom httr GET
#' @importFrom httr config
#' 
#' @export
authorize_api_key.Token <- function(token)
{
  url <- .constructUrl(base_url(), apiPath())
  GET(url, config(token = token))
}


#' @export
authorize_api_key.character <- function(token) 
{
  if (length(token) > 1L) {
    token <- token[1]
    warning("Only the first element of 'token' was used")
  }
  url <- .constructUrl(base_url(), apiPath())
  try({
    GET(url, add_headers(Authorization = paste("Bearer", token)))
  })
}


#' @export
authorize_api_key.default <- function(token)
{
  message("A method has not been defined for objects of this class")
}


## Path for the API URI
apiPath <- function()
  "v3/users/me"

#' Set up OAuth 2.0 for Eventbrite
#' 
#' Will set up OAuth for the user, with the package acting as 'client'.
#' 
#' @param api_key The app's API Key.
#' @param client_secret The app's Client Secret.
#' 
#' @importFrom httr oauth_endpoint
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' 
#' @return An R6 object of type 'Token'.
#' 
#' @export
setup_eventbrite_oauth <- function(api_key = NULL, client_secret = NULL) {
  .defApp <- list(ApiKey = "EKSSGY6QXISUY4Y2GA", 
                  ClientSecret = "2SYHEIIRCYJUGZQ65J4QKCPD5IW5RWELNTB3UXO7PDUFDRN2BY")
  if (is.null(api_key))
    api_key <- .defApp$ApiKey
  if (is.null(client_secret))
    client_secret <- .defApp$ClientSecret
  oauth2.0_token(
    oauth_endpoint(authorize = .fUrl("authorize"), access = .fUrl("token")),
    oauth_app("rEventbrite", key = api_key, secret = client_secret)
  )
}





