#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr status_code
#' @importFrom httr stop_for_status
.downloadPage <- function(url, obj = NULL, ...)
{
  if (is.null(obj)) {
    tryCatch({
      r <-
        GET(url,
            add_headers(Authorization = paste("Bearer", ...)))
    }, error = function(e)
      stop(e))
  }
  else {
    tryCatch({
      r <-
        GET(
          url,
          add_headers(Authorization = paste("Bearer", ...)),
          query = list(continuation = .getContinuationToken(obj))
        )
    },
    error = function(e)
      stop(e))
  }
  stop_for_status(
    r,
    task = paste("Reading data failed with status", status_code(r))
  )

  rlist <- .convertToList(r)

  if (.hasMoreItems(rlist)) {
    data <- .downloadPage(url, rlist, ...)
    rlist <- append(rlist, data)
  }
  rlist
}


.hasMoreItems <- function(obj)
{
  obj$pagination$has_more_items
}



.getContinuationToken <- function(obj)
{
  obj$pagination$continuation
}


#' @importFrom httr content
#' @importFrom jsonlite fromJSON
.convertToList <- function(obj)
{
  att <- content(obj, as = 'text', encoding = "ISO-8859-1")
  jsonlite::fromJSON(att, simplifyVector = FALSE, simplifyDataFrame = TRUE)
}


.getPageNumber <- function(obj)
{
  obj$pagination$page_number
}


.getPageCount <- function(obj)
{
  obj$pagination$page_count
}
