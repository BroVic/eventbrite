#' Get a User object
#' 
#' @param token The token.
#' @param element The object's field.
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






#' @importFrom httr content
#' @importFrom httr stop_for_status
#' @importFrom jsonlite fromJSON
getContent <- function(token, path, ...)
{
  r <- getApiResponse(token, path, ...)
  stop_for_status(r)
  fromJSON(content(r, 'text'))
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
