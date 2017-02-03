#' Dataframe Addrow
#'
#' Function to Simplify adding a row to a dataframe and updating the rownames.
#' @export

df.addrow <- function(datafr, newrow, rowname) {
  temprows <- rownames(datafr)
  datafr <- rbind(datafr,newrow)

  rownames(datafr) <- c(temprows,rowname)

  return(datafr)
}
