#' Set up OAuth 2.0 for the Eventbrite API
#' 
#' Will set up OAuth for the user, with the package playing the role of 'client'.
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
