#' @title Inverse function to calculate P* from ABC and OFL
#' 
#' @description takes as inputs the OFL, biomass relative to its target, and the assumed CV for the OFL
#' @source from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
#' 
#' 
#' @param relB ABC - allowable biological catch 
#' @param OFL OFL for the stock
#' @param CV Coefficient of variation for OFL
#' 
#' @return P* value
#' 
#' @example
#' inv_ABC <- inv_ABC(11456,13234,1.0)

inv_ABC <- function(ABC,OFL, CV)
{
  #Convert CV to sigma for lognormal dist
  sd <- sqrt(log(CV*CV+1))
  
  #Calculate ABC using inverse of the lognormal dist
  return(plnorm(ABC, meanlog = log(OFL), sdlog = sd))
}