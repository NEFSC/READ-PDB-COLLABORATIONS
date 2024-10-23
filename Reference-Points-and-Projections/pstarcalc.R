#' @title Function to calculate P* FROM RELATIVE BIOMASS AND COUNCIL POLICY
#' 
#' @description Calculates P* using input of the Biomass relative to target 
#' @author E Liljestrand, expanded code from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
#' 
#' @param relB B/Bmsy
#' 
#' @return P* 
#' 
#' @examples
#' Pstar <- calc_pstar(1.55)

calc_pstar <- function(relB)
{  
  if(relB>=1.5) #at asymptote
  {
    P = 0.49
  }
  else if(relB<=0.1) #below level at which fisheries would be closed
  {
    P = 0.0
  }
  else
  {
    if(relB<1) #relative biomass between 0.1 and 1
    {
      P = -0.05+0.5*relB
    }  
    else  #relative biomass between 1 and 1.5
    {
      P = 0.37+0.08*relB
    }
  }
  return(P)
}
