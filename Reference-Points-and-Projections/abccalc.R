#' @title Function to calculate the ABC using a lognormal distribution
#' 
#' @description Calculates ABC using inputs of the OFL, biomass relative to target and assumed CV for OFL 
#' @source from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
#' 
#' @param OFL OFL for the stock
#' @param relB B/Bmsy
#' @param CV Coefficient of variation for OFL
#' 
#' @return ABC - allowable biological catch 
#' 
#' @example
#' catch <- ABC(12345,1.11,0.6)

ABC <- function(OFL, relB, CV)
{
  #Convert CV to sigma for lognormal dist
  sd <- sqrt(log(CV*CV+1))

  P <- calc_pstar(relB)

  #Calculate ABC using inverse of the lognormal dist
  return(qlnorm(P, meanlog = log(OFL), sdlog = sd))
}
