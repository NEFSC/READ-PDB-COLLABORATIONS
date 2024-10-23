#' @title make_simtest_plots
#' @description
#' Automated creation of plots from a WHAM simulation self-test. Main plot shows mean, median, and 5%, 25%, 75%, 95% quantiles. 
#' Optional additional plots show only mean with standard error, only median and quantiles, and numerous realizations. 
#' Percent error (Est-True)/True computed for F, R, and SSB.
#' Requires ggplot2, dplyr.
#' 
#' @param mod_selftest result of do_selftest function, contains annual true and estimated F, R, and SSB for a number of realizations
#' @param saveall Boolean (default=FALSE) when TRUE save all the possible plots, when FALSE save only single main plot
#' @param savepdf Boolean (default=FALSE) when TRUE save the selected plot(s) in a single pdf file 
#' @param savepng Boolean (default=FALSE) when TRUE save the selected plot(s) as separate png files
#' @param myfilename_pdf (default=NULL) name (including filepath if so desired) for pdf to be created
#' @param mypngprefix (default=NULL) prefix added to default names for all saved png files (including filepath is so desired)
#' @return list of 8 figures, main figure is first one  

make_simtest_plots <- function(mod_selftest, saveall=FALSE, savepdf=FALSE, savepng=FALSE, myfilename_pdf=NULL, mypngprefix=NULL){
  
  require(ggplot2)
  require(dplyr)
  
  myfig <- list()
  
  # compute relative percent error
  mod_conv <- mod_selftest %>% 
    filter(converged == TRUE) %>%
    mutate(percent_err = 100 * (EST - TRU)/TRU)
  
  # compute mean bias and standard error of the mean (divide sd by number of realization) as well as 90% confidence interval
  bias <- mod_conv %>%
    group_by(VAR, YEAR) %>%
    summarise(mpe = mean(percent_err),
              se  = sd(percent_err)/n()) %>%
    mutate(mpe_90lo = mpe - 1.645 * se,
           mpe_90hi = mpe + 1.645 * se)
  
  # alternative based on median and quantiles
  mod_quants <- mod_conv %>%
    group_by(VAR, YEAR) %>%
    summarize(q05 = quantile(percent_err, 0.05),
              q25 = quantile(percent_err, 0.25),
              median = median(percent_err),
              q75 = quantile(percent_err, 0.75),
              q95 = quantile(percent_err, 0.95))
  
  # main plot showing mean, median, and quantiles
  myfig[[1]] <- ggplot(mod_quants, aes(x=YEAR, y=median)) +
    geom_line() +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
    geom_line(data=bias, aes(x = YEAR, y = mpe), color="blue") +
    geom_hline(yintercept = 0, col="red") +
    facet_wrap(~VAR, ncol = 3) +
    ylab("Percent error") +
    ggtitle("Self-test mean (blue) and median bias with 5%, 25%, 75%, 95% quantiles") +
    theme_bw()

  # plot mean and CI of bias using standard error (divide sd by number of realization)
  myfig[[2]] <- ggplot(bias, aes(x = YEAR, y = mpe)) +
    geom_line() +
    geom_ribbon(aes(ymin = mpe_90lo, ymax = mpe_90hi), alpha = 0.2) +
    geom_hline(yintercept = 0, col="red") +
    facet_wrap(~VAR) +
    theme_bw() +
    ylab("Mean percent error (Est. - True)") +
    xlab("Year") +
    ggtitle("Self-test mean bias")
  
  # plot just median and quantiles
  myfig[[3]] <- ggplot(mod_quants, aes(x=YEAR, y=median)) +
    geom_line() +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.2) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
    geom_hline(yintercept = 0, col="red") +
    facet_wrap(~VAR, ncol = 3) +
    ylab("Percent error") +
    ggtitle("Self-test median bias with 5%, 25%, 75%, 95% quantiles") +
    theme_bw()
  
  mod_conv_means <- mod_conv %>%
    group_by(VAR, SIM_ID) %>%
    mutate(meanval = mean(percent_err), medianval = median(percent_err))
  
  # take a look at some of the realizations  
  myfig[[4]] <- ggplot(filter(mod_conv, SIM_ID <= 10), aes(x=YEAR, y=percent_err, col=as.factor(SIM_ID))) +
    geom_line() +
    facet_wrap(~VAR, ncol = 3) +
    ggtitle("10 realizations") +
    theme_bw() +
    theme(legend.position = "none")
  
  make_plots <- function(mod_conv, mod_conv_means, myvar, maxplots){
    convid <- unique(mod_conv$SIM_ID)
    if (maxplots > length(convid)){
      maxid <- max(convid)
    }else{
      maxid <- convid[maxplots]
    }
    p1 <- ggplot(filter(mod_conv, VAR==myvar, SIM_ID <= maxid), aes(x=YEAR, y=percent_err)) +
      geom_line() +
      facet_wrap(~SIM_ID, ncol=3) +
      geom_hline(yintercept = 0, col="red") +
      geom_hline(data=filter(mod_conv_means, VAR==myvar, SIM_ID <= maxid), 
                 aes(yintercept = meanval), col="blue", linetype="dashed") +
      ggtitle(myvar) +
      theme_bw()
    return(p1)  
  }
  
  myfig[[5]] <- make_plots(mod_conv, mod_conv_means, "SSB", 12)
  myfig[[6]] <- make_plots(mod_conv, mod_conv_means, "F", 12)
  myfig[[7]] <- make_plots(mod_conv, mod_conv_means, "R", 12)
  
  myfig[[8]] <- ggplot(mod_selftest %>% 
                         filter(converged == TRUE) %>%
                         select(-converged) %>%
                         pivot_longer(cols = c(TRU, EST), names_to = "SOURCE", values_to = "VALUE") %>%
                         filter(SIM_ID %in% unique(SIM_ID)[1:10]), 
                       aes(x = YEAR, y = VALUE, color = SOURCE)) +
    geom_line() +
    facet_grid(VAR~SIM_ID, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Value") +
    ggtitle("Example estimated vs. true ")
  
  # optionally save pdf  
  if (savepdf == TRUE){
    pdf(file = myfilename_pdf)
    if (saveall == TRUE){
      print(myfig)
    }else{
      print(myfig[[1]])
    }
    dev.off()
  }
  
  # optionally save png
  if (savepng == TRUE){
    ggsave(filename = paste0(mypngprefix, "selftest_mean_median_quantiles.png"), myfig[[1]])
    if (saveall == TRUE){
      ggsave(filename = paste0(mypngprefix, "selftest_mean_only.png"), myfig[[2]])
      ggsave(filename = paste0(mypngprefix, "selftest_median_only.png"), myfig[[3]])
      ggsave(filename = paste0(mypngprefix, "selftest_10_realizations_combined.png"), myfig[[4]])
      ggsave(filename = paste0(mypngprefix, "selftest_12_SSB_realizations.png"), myfig[[5]])
      ggsave(filename = paste0(mypngprefix, "selftest_12_F_realizations.png"), myfig[[6]])
      ggsave(filename = paste0(mypngprefix, "selftest_12_R_realizations.png"), myfig[[7]])
      ggsave(filename = paste0(mypngprefix, "selftest_10_estimated_vs_true.png"), myfig[[8]])
    }
  }
  
  return(myfig)
}
