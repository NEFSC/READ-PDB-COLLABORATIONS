#' @param series A data frame containing the following columns:
#' \itemise{
#'   \item{est - Estimated time series}
#'   \item{lo Lower 95% CI for estimates}
#'   \item{hi Upper 95% CI for estimates}
#' @param rho Mohn's rho value associated with the time series.
#' 
#' @return A data table containing original and rho-adjusted time series with accompanying 95% CI. 
calc.rho.adj.ests <- function(series = NULL,
                              rho = NULL)  {
  result <- series %>% as.data.frame() %>%
    mutate( est.adj = (1/(1+rho))*est,
            lo.adj  = (1/(1+rho))*lo,
            hi.adj  = (1/(1+rho))*hi
    )
  
  return(result)
}