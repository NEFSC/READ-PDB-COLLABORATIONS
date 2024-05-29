#--------------
#' @description
#' Perform simulation self-test of a WHAM model.
#' 
#' @param mod wham model created with `fit_wham()`
#' @param n_sim number of simulations to perform
#' @ return data.frame of annual true and estimated F, R, and SSB
do_selftest <- function(mod, n_sim) {

  sim_inputs <- replicate(n_sim, sim_fn(mod), simplify=F)
  res = list(reps = list(), par.est = list(), par.se = list(), 
             adrep.est = list(), adrep.se =list())
  j <- 1
  for(i in 1:length(sim_inputs)){
    cat(paste0("sim ", i, " of ", n_sim, "\n"))
    sim_inputs[[i]]$SIM_ID <- i
    tfit <- NULL
    tryCatch(expr = {tfit <- fit_wham(sim_inputs[[i]], do.osa = F,
                                      do.retro = F, MakeADFun.silent = T, do.sdrep = F);},
             error = function(e){
               message("An error occurred:\n", e)
             },
             finally = {
               if (is.null(tfit)) {
                 
               } else {
                 
                 conv <- check_convergence(tfit, ret = T)
                 if(conv$convergence == 0){
                   conv$converged <- TRUE
                 } else conv$converged <- FALSE
                 res$reps[[j]] = tfit$rep
                 res$reps[[j]]$converged <- conv$converged
                 res$reps[[j]]$SIM_ID <- i
                 res$reps[[j]]$years <- tfit$years
                 j <- j + 1
               }
               
             })
  }
  
  true_ssb = map_df(sim_inputs, function(x) data.frame(SIM_ID = x$SIM_ID, 
                                                       YEAR   = x$years, 
                                                       VAR    = "SSB", 
                                                       TRU    = x$data$SSB))
  
  true_f  = map_df(sim_inputs, function(x) data.frame(SIM_ID = x$SIM_ID, 
                                                      YEAR   = x$years, 
                                                      VAR    = "F", 
                                                      TRU    = x$data$`F`))
  
  true_r  = map_df(sim_inputs, function(x) data.frame(SIM_ID = x$SIM_ID, 
                                                      YEAR   = x$years, 
                                                      VAR    = "R", 
                                                      TRU    = x$data$NAA[,1]))
  
  est_ssb  = map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                     YEAR   = x$years,
                                                     VAR    = "SSB",
                                                     EST    = x$SSB))
  
  est_f  = map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                   YEAR   = x$years,
                                                   VAR    = "F",
                                                   EST    = x$`F`))
  
  est_r  = map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                   YEAR   = x$years,
                                                   VAR    = "R",
                                                   EST    = x$NAA[,1]))
  
  conv_df <- map_df(res$reps, function(x) data.frame(SIM_ID = x$SIM_ID,
                                                     converged = x$converged))
  
  # Join all sims
  sim_out <- 
    left_join(true_ssb, est_ssb) %>%
    bind_rows({left_join(true_f, est_f)}) %>%
    bind_rows({left_join(true_r, est_r)}) %>%
    left_join(conv_df)
  
  return(sim_out)
}
