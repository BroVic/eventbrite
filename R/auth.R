#' Authorize the Eventbrite personal key
#' 
#' @param token The access token
#' 
#' @export
get_api_response <- function(token, ...)
  UseMethod("get_api_response")




#' @importFrom httr GET
#' @importFrom httr config
#' 
#' @export
get_api_response.Token <- function(token, endpoint = NULL)
{
  url <- .build_call_url(endpoint) 
  GET(url, config = config(token = token))
}





#' @importFrom httr GET
#' @importFrom httr add_headers
#' 
#' @export
get_api_response.character <- function(token, endpoint = NULL) 
{
  if (length(token) > 1L) {
    token <- token[1]
    warning("Only the first element of 'token' was used")
  }
  
  url <- .build_call_url(endpoint)
  
  try({
    GET(url, config = add_headers(Authorization = paste("Bearer", token)))
  })
}





#' @export
get_api_response.default <- function(token)
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
  .constructUrl(api_baseurl(), ep)
}






#' Set up OAuth 2.0 for Eventbrite
#' 
#' Will set up OAuth for the user, with the package acting as 'client'.
#' 
#' @param api_key The application's API Key.
#' @param client_secret The application's \emph{Client Secret}.
#' 
#' @importFrom httr oauth_endpoint
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' 
#' @return An R6 object of type 'Token'.
#' 
#' @export
setup_eventbrite_oauth <- function(api_key = NULL, client_secret = NULL) {
  defApp <- list(api.key = creds$api.key, 
                  client.secret = creds$client.secret)
  
  if (is.null(api_key))
    api_key <- defApp$api.key
  
  if (is.null(client_secret))
    client_secret <- defApp$client.secret
  
  oauth2.0_token(
    oauth_endpoint(authorize = .fUrl("authorize"), access = .fUrl("token")),
    oauth_app("rEventbrite", key = api_key, secret = client_secret)
  )
}





#' @importFrom httr content
get_user <- function(token, element = NULL) 
{
  if (!is.character(token) && !inherits(token, "Token"))
    stop("'token' must be of class 'character' or 'Token'")
  r <- get_api_response(token)
  r <- stop_for_status(r)
  c <- content(r)
  if (!is.null(element)) {
    elem <- match.arg(element, c("id", "name")) # elements of 'User' object
    c <- c[[elem]]
  }
  invisible(c)
}
