#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr status_code
#' @importFrom httr stop_for_status
#' @importFrom magrittr %>% 
fetchPages <- function(url, obj = NULL, token)
{
  if (!isPaginated(obj))
    return(obj)
  tryCatch({
    contoken <- .getContinuationToken(obj)
    r <- getResource(token, query = list(continuation = contoken))
    httr::stop_for_status(
      r,
      paste("Reading data failed with status", status_code(r))
    )
  },
  error = function(e)
    stop(e))
  
  newobj <- httr::content(r)
  rlist <- append(obj, newobj)
  if (.hasMoreItems(newobj)) {
    data <- fetchPages(url, newobj, token)
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


# #' @importFrom httr content
# #' @importFrom jsonlite fromJSON
# .convertToList <- function(obj)
# {
#   att <- content(obj, as = 'text', encoding = "ISO-8859-1")
#   jsonlite::fromJSON(att, 
#                      simplifyVector = FALSE, 
#                      simplifyDataFrame = TRUE)
# }

isPaginated <- function(obj)
{
  stopifnot(is.vector(obj, 'list'))
  'pagination' %in% names(obj)
}

.getPageNumber <- function(obj)
{
  obj$pagination$page_number
}


.getPageCount <- function(obj)
{
  obj$pagination$page_count
}
