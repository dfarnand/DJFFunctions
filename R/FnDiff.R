#' fnDiff
#'
#' Function for testing - takes the difference in some statistics (defaults to
#' mean) between elements of a vector, from 1 to element n, and from n+1 to the
#' end of the vector.
#' 
#' @param 
#' @param 
#' @return 
#' @examples
#' @export

fnDiff <- function(x, n, fnctn=mean) {
  return(fnctn(x[1:n])-fnctn(x[(n+1):length(x)]))
}
