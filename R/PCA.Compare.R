#' PCA Naive Bayes Comparison
#'
#' A function that takes the Principal Component Analysis of a given dataset
#' (classes are assumed to be the last column), and calculates the
#' cross-validated accuracy of Naive Bayes classification with both the original
#' and PCA version, returning the accuracy of each as well as other descriptive
#' data.
#'
#' @param dataset
#' @param verbose
#' @return 
#' @examples
#' @export


require(klaR)
require(e1071)
require(purrr)
require(mlbench)
require(entropy)
require(psych)

NB.Test <- function(dataset, R=10, K=10, clname="Classes", verbose=F) {
  ## Function does a cross-validation test on Naive Bayes analysis for the dataset
  ## Pass it the name of the Class column as clname
  set.seed(2011)
  corRate <- 0

  tryCatch( {
    ## Makes the formula using the columns
    cols <- dimnames(dataset)[[2]]
    len = length(cols)
    paste(cols[1:(len-1)],collapse="+") %>%
      paste(clname,"~",.,sep="") %>%
      as.formula() -> expr

    ## Makes sure the classes column is a factor
    dataset[,clname] <- as.factor(dataset[,clname])

    nObs <- nrow(dataset)
    for (i in 1:R) {
      ind <- sample(rep(1:K,each=nObs/K),replace=F)
      ## Creates a random order of numbers 1-K (equal numbers of each)

      corSum <- 0 #Running total of correct classifications
      for (j in 1:K) {
        ## Leaving out 10% of the data randomly and then using the rest to predict
        ## its points, and checking how many are correctly predicted.
        tryCatch ( {  #Because the first function fails when it finds a zero variance
          tempResult <- NaiveBayes(expr, data=dataset[ind!=j,])
          preds <- predict(tempResult,dataset[ind==j,])$class
        },
        error=function(err) {
          tempResult <- naiveBayes(expr, data=dataset[ind!=j,])
          preds <- predict(tempResult,dataset[ind==j,])
        })

        corCount <- sum(preds==dataset[,clname][ind==j])
        corSum <- corSum + corCount
      }
      corRate <- corRate + (corSum/nObs)
    }
    return(corRate/R)
  },
  error=function(err) {
    if (verbose) print(err)
    return(NA)
  })


}



PCANB.Compare <- function(dataset, verbose=F) {
  tryCatch( {
    len = length(dimnames(dataset)[[2]])
    names(dataset)[len] <- "Cl"
    datapca <- as.data.frame(princomp(dataset[1:(len-1)])$scores)
    datapca$Cl <- dataset$Cl

  },
  error=function(err){
    if (verbose) print(err)
    return(NA)
  })
 # browser()

  tryCatch( {
    results <- vector(mode="list",length=8)
    names(results)=c("Orig","PCA","Diff","NFeatures","NGroups","NObs","Entropy","KMO")

  #  results$Name <- dsname
    results$Orig <- NB.Test(dataset, clname="Cl",verbose=verbose)
    results$PCA <- NB.Test(datapca, clname="Cl",verbose=verbose)
    results$Diff <- results$PCA-results$Orig
    results$NFeatures <- len-1
    results$NGroups <- length(unique(dataset$Cl))
    results$NObs <- length(dimnames(dataset)[[1]])
    results$Entropy <- entropy.plugin(eigen(cor(dataset[,-len]))$values/dim(dataset[,-len])[2])
    results$KMO <- KMO(dataset[,-len])$MSA
    return(results)
  },
  error=function(err){
    if (verbose) print(err)
    return(NA)
  })
}
