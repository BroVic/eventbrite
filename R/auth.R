#' Set up OAuth 2.0 for the Eventbrite API
#' 
#' Will set up OAuth for the user, with the package playing the role of 'client'.
#' 
#' @param api.key The application's API Key.
#' @param client.secret The application's \emph{Client Secret}.
#' 
#' @importFrom httr oauth_endpoint
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' 
#' @return An R6 object of type 'Token'.
#' 
#' @export
setup_eventbrite_oauth <- function(api.key = NULL, client.secret = NULL) {
  defApp <- getDefApp()
  
  if (is.null(api.key) && is.null(client.secret)) {
    api.key <- defApp$api.key
    client.secret <- defApp$client.secret
  }
  
  oauth2.0_token(
    oauth_endpoint(authorize = oauthUrl("authorize"), access = oauthUrl("token")),
    oauth_app("rEventbrite", key = api.key, secret = client.secret)
  )
}
