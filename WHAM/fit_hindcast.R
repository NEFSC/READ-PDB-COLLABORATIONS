#' @title Fit hindcast model to data
#' 
#' @description Fit hindcast model to data, dropping specified indices and years of data specified by the peel 
#' 
#' @param model An rds from a WHAM fit
#' @param peel An integer indicating how many years are peeled from the terminal year, just as in a retro
#' @param drop A named list with 'indices' and 'index_paa', where each element is a vector of indices/index_paa to drop in each hindcast. To not drop any indices or index_paa in a hindcast, set to NA.
#' 
#' @return A WHAM model fit
#'
#' @example 
#' fit_hindcast(model=WHAMRUN.rds, peel=1, drop=list(indices=1:2, index_paa=1:2))

fit_hindcast <- function(model, peel, drop){
  temp = list(data = model$env$data, par = model$parList, map = model$env$map, random = unique(names(model$env$par[model$env$random])))
  nyrs = temp$data$n_years_model
  temp$data$use_indices[(nyrs - peel+1):nyrs, drop$indices] = 0  ##JJD changed these 2 lines to include +1 per Tim bug catch Oct 31, 2023
  temp$data$use_index_paa[(nyrs - peel+1):nyrs, drop$index_paa] = 0
  mod <- fit_wham(temp, do.retro = FALSE, do.osa = FALSE)
  return(mod)
}