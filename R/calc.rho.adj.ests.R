#' @title calc.rho.adj.ests
#' @description Calculate rho-adjusted time series based on 90% CI
#' @param series A data frame containing the following columns:
#' * est -  Estimated time series
#' * lo_90 - Lower 90% CI for estimates
#' * hi_90 - Upper 90% CI for estimates
#' @md
#' @param rho Mohn's rho value associated with the time series.
#' 
#' @return A data table containing original and rho-adjusted time series with accompanying 90% CI. 
#' @export

calc.rho.adj.ests <- function(series = NULL,
                              rho = NULL)  {
  result <- series %>% as.data.frame() %>%
    mutate( est.adj = (1/(1+rho))*est,
            lo_90.adj  = (1/(1+rho))*lo_90,
            hi_90.adj  = (1/(1+rho))*hi_90
    )
  
  return(result)
}
