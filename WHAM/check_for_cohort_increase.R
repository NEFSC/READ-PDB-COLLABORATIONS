#' @title Check for cohort increase in WHAM output
#'
#' @description
#' Random effects can allow a cohort to increase from one year and age to the next year and age. This function makes a plot to quickly identify when this happens.
#' 
#' @param mod A model result from WHAM, can be either single WHAM or multi WHAM.
#'
#' @return A ggplot showing the relative percent change for each cohort (including the plus group) with the number denoting the age and year of the surviving fish. Values above zero (the red dashed line) indicate the cohort has increased. Values above 100 may be cause for concern.
#'
#' @examples
#' mod <- readRDS("m38.RDS")
#' p <- check_for_cohort_increase(mod)
#' ggsave(filename = "my_cohort_check.png", p)
#' Note: for those who do not like ggplot, matplot(naa_percent_rel_diff) can be used instead

check_for_cohort_increase <- function(mod){

  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # check for single WHAM vs multiwham (sum across all stocks and regions)
  if (length(dim(mod)) == 2){
    naa <- mod$rep$NAA
  }else{
    for (i in 1:dim(mod$rep$NAA)[1]){
      for (j in 1:dim(mod$rep$NAA)[2]){
        if (i == 1 & j ==1){
          naa <- mod$rep$NAA[i,j,,]
        }else{
          naa <- naa + mod$rep$NAA[i,j,,]
        }
      }
    }
  }
  #naa <- mod$rep$NAA[1,1,,]
  naa_diff <- matrix(NA, nrow=nrow(naa), ncol=ncol(naa))
  naa_percent_rel_diff <- naa_diff
  for (i in 2:nrow(naa)){
    for (j in 2:(ncol(naa)-1)){
      naa_diff[i,j] <- naa[i,j] - naa[i-1,j-1]
      naa_percent_rel_diff[i,j] <- 100 * naa_diff[i,j] / naa[i-1,j-1]
    }
    j <- ncol(naa)
    naa_diff[i,j] <- naa[i,j] - (naa[i-1,j-1] + naa[i-1, j])
    naa_percent_rel_diff[i,j] <- 100 * naa_diff[i,j] / (naa[i-1,j-1] + naa[i-1, j])
  }
  mydf <- tibble(Year = character(),
                 Age = integer(),
                 reldiff = double())
  
  years <- mod$years_full
  
  for (j in 2:ncol(naa)){
    thisdf <- tibble(Year = years,
                     Age = j,
                     reldiff = naa_percent_rel_diff[,j])
    mydf <- rbind(mydf, thisdf)
  }
  
  p <- ggplot(mydf, aes(x=Year, y=reldiff, label=Age)) +
    geom_text(aes(color = factor(Age))) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ylab("Relative Percent Change in Cohort") +
    theme_bw() +
    theme(legend.position="none")
  print(p)
  
  return(p)
}
