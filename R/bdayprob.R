#' Birthday Problem
#'
#' Function that takes the size of the group and the number of simulations to
#' run to calculate the probability of that group having two or more members
#' with the same birthday.
#' @export

bdayprob <- function(groupsize,nsims,n=2)  {
  nrepeats <- 0 # Number of samples with repeated birthdays
  for (i in 1:nsims) {
    bdaycounter <- rep(0,365) #fill the vector full of zeroes so the if statement below will work
    for (bday in genbirthday(groupsize)) {
      bdaycounter[bday] <- bdaycounter[bday] + 1
      if (bdaycounter[bday] > (n - 1)) { #
        nrepeats <- nrepeats + 1
        break
      }
    }
  }
  return(nrepeats/nsims) #Divide by nsims to make it the simulated probability
}
