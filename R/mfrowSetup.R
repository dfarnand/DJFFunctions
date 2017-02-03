#' mfrow Setup
#'
#' Function to quickly produce mfrow setup for a number of plots and a max number
#' of columns you want to display Defaults to 1x1, so you can quickly use it to
#' reset the plot window as well.
#' @export

mfrowSetup <- function(nPlots=1,maxCols=1,...) {
  par(mfrow = c(ceiling(nPlots/min(c(maxCols,nPlots)))
               ,min(c(maxCols,nPlots))))
}
