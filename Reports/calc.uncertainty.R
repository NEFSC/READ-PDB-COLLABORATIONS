#' @param log.est An estimated time series in log space, no default.
#' @param log.se A vector of log standard errors corresponding to the estimated time series, no default. 
#' 
#' @return A data frame containing:
#' \itemize{
#'   \item{est - Time series of estimated values}
#'   \item{se - Accompanying standard errors}
#'   \item{CV - Accompanying CVs}
#'   \item{lo - Lower 95% confidence intervals}
#'   \item{hi - Upper 95% confidence intervals}
#' }
calc.uncertainty <- function(log.est = NULL,
                             log.se = NULL){
  result <- cbind(log.est, log.se) %>% as.data.frame() %>%
    mutate( est = exp(log.est),
            se = exp(log.se),
            CV = sqrt(exp(log.se*log.se)-1),
            lo = exp(log.est - qnorm(0.975)*log.se),
            hi = exp(log.est + qnorm(0.975)*log.se)) %>%
    select(est, se, CV, lo, hi)
  
  return(result)
}