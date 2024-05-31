#' @param log.est An estimated time series in log space, no default.
#' @param log.se A vector of log standard errors corresponding to the estimated time series, no default. 
#' 
#' @param log.est An estimated time series in log space, no default.
#' @param log.se A vector of log standard errors corresponding to the estimated time series, no default. 
#' 
#' @return A data frame containing:
#' \itemize{
#'   \item{est - Time series of estimated values}
#'   \item{se - Accompanying standard errors}
#'   \item{CV - Accompanying CVs}
#'   \item{lo_95 - Lower 95% confidence intervals}
#'   \item{hi_95 - Upper 95% confidence intervals}
#'   \item{lo_90 - Lower 99% confidence intervals}
#'   \item{hi_90 - Upper 99% confidence intervals}
#' }
calc.uncertainty <- function(log.est = NULL,
                             log.se = NULL){
  result <- cbind(log.est, log.se) %>% as.data.frame() %>%
    mutate( est = exp(log.est),
            se = exp(log.se),
            CV = sqrt(exp(log.se*log.se)-1),
            lo_95 = exp(log.est - qnorm(0.975)*log.se), # 95% CI
            hi_95 = exp(log.est + qnorm(0.975)*log.se),
            lo_90 = exp(log.est - qnorm(0.95)*log.se), # 90% CI, needed for Mohn's rho adjustment
            hi_90 = exp(log.est + qnorm(0.95)*log.se)) %>%
    select(est, se, CV, lo_90, hi_90, lo_95, hi_95)
  
  return(result)
}
