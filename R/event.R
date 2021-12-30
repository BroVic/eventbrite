#' List Owned Events
#' 
#' Fetch a list of events owned by the user from the Eventbrite API
#' 
#' 
read_events <- function(token)
{
  
}

#' Read Event Data
#'
#' Reads data on events from the server
#'
#' @param eventid Character vector of length \code{1L}; The ID of the given
#' event whose data we want to poll.
#' @param ... arguments passed to \code{authorize_api_key}.
#'
#' @importFrom httr stop_for_status
#'
#' @export
read_event_data <- function(eventid, token) {
  # TODO: Make more encompassing, beyond just attendee data...
  apiurl <- file.path(apiBaseUrl(), "events", eventid, "attendees")

  # try({
  #   r <- getResource(token)  # TODO: Original intent?
  #   stop_for_status(r)
  # })

  # pageInfo <- att.list$pagination
  att.list <- .downloadPage(apiurl, token = token)

  for(i in seq_len(.getPageCount(att.list)))
    att.list$pagination <- NULL

  .f <- function(df) {
    objNm <-
    c(
      "id",
      "created",
      "changed",
      "quantity",
      "profile",
      "checked_in",
      "cancelled",
      "status"
    )

    data.list <- df[objNm]
    prof <- data.list$profile
    prof$addresses <- NULL
    data.list$profile <- NULL
    cbind(data.list, prof)
  }

  dflist <- lapply(att.list, .f)
  Reduce(rbind, dflist)
}
