#' Set up OAuth 2.0 for the Eventbrite API
#' 
#' Will set up OAuth for the user, with the package playing the role of 'client'.
#' 
#' @param api.key The application's \emph{API Key}.
#' @param client.secret The application's \emph{Client Secret}.
#' @param ... Arguments passed to \code{\link[httr]{oauth2.0_token}}.
#' 
#' @importFrom httr oauth_endpoint
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' 
#' @return An R6 object of type 'Token'.
#' 
#' @export
setup_eventbrite_oauth <- function(api.key = NULL, client.secret = NULL, ...) {
    oauthUrl <- function(end)
      constructUrl(hostUrl(), paste("oauth", end, sep = "/"))

    keyIsNul <- is.null(api.key)
    secretIsNul <- is.null(client.secret)
    
    if (xor(keyIsNul, secretIsNul))
      stop("If at all, both 'api.key' and 'client.secret' must be provided")
    
    if (keyIsNul && secretIsNul) {    
      defApp <- getDefApp()
      api.key <- defApp$api.key
      client.secret <- defApp$client.secret
    }
    
    oauth2.0_token(
      endpoint = oauth_endpoint(authorize = oauthUrl("authorize"),
                                access = oauthUrl("token")),
      app = oauth_app("rEventbrite", key = api.key, secret = client.secret),
      ...
    )
  }
