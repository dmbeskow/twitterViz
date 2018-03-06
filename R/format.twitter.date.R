#' Format Twitter Date
#'
#' \code{format.twitter.date } Format a twitter date
#'
#' This function clean...
#'
#' @param datestring A logical value indicating whether or not to delete the original files
#' @return vector of POSIX dates
#' @examples \dontrun{
#'
#' df$date2 <- format.twitter.date(df$created_at)
#' }
#'
#' @export
format.twitter.date <- function(datestring){
  datestring <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
  return(datestring)
}
