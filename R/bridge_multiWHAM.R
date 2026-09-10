#' @title Bridge from single to multi-wham
#' @description Compares a single-wham model to a multi-wham model with 1 stock and 1 area to bridge from single-wham to multi-wham (allows 1 stock/region models to take advantage of additional features in multi-wham software (e.g. initial stock conditions))
#'
#' @param mod_single A model object fitted in single/regular WHAM, no default.
#' @param mod_multi A model object fitting the same model specification in multi-WHAM, no default.
#' @param fdir A file path where comparison table and plots will be stored, default is project directory.
#' @param table.opts Table options that roughly match compare_wham_models() options, no guarantee that all options operate, default = NULL.
#' @param plot.opts Plot options that roughly match compare_wham_models() options, no guarantee that all options operate, default = NULL.
#' 
#' @return A list containing the following:
#' \itemize{
#'   \item{sdrep_summary - Summary of joined sdrep objects, repeated labels indicate differences in estimated values (rounded to 3 decimals)}
#'   \item{different_pars - Vector of parameter names that had different estimates between the single and multi-wham models}
#'   \item{tab - Table of AIC and Mohn's rho values by model}
#'   \item{out - Comparison plots}
#'   \item{sdrep_diff - Table of parameter estimates that differ between single and multi-wham versions of the model}
#'   #!!! Need to add a check here for the bias correction setting since the defaults differ and this is an easy change to forget about when switching
#' }
#' 
#' @examples
#' mod_single <- readRDS(file = here::here("Bridge_runs", "9_bridge_reviseCV_NEFSC", "9_bridge_model.rds"))
#' mod_multi <- readRDS(file = here::here("Bridge_runs", "12_bridge_updateWHAM", "12_bridge_model.rds"))
#' bridge_multiWHAM(mod_single = mod_single, mod_multi = mod_multi, fdir = here::here("Bridge_runs/12_bridge_updateWHAM"), table.opts= list(save.csv = TRUE))
#' 
#' @export

bridge_multiWHAM <- function(mod_single = NULL, mod_multi = NULL, fdir = here::here(), table.opts = NULL, plot.opts = NULL){
  
  # Read in multi-wham
  sdrep_multi <- mod_multi$sdrep %>% summary() %>% as.data.frame() #!!! summary() dataframe drop last row/year
  multi_names <- as.list(mod_multi$sdrep, all.names = TRUE) %>% summary() %>% rownames() %>% unique() # Could use TMB::as.list.sdreport() to turn into list like input 
  
  # Read in single-wham and rename parameters to match multi-wham
  sdrep_single <- mod_single$sdrep %>% summary() %>% as.data.frame()
  single_names <- as.list(mod_single$sdrep, all.names = TRUE) %>% summary() %>% rownames() %>% unique()
  # log_F1, NAA_sigma, NAA_rho_a, NAA_rho_y,  NAA, log_NAA_rep, log_index_resid, log_catch_resid not in multi?
  # log_SPR0, log_FAA_XSPR_static, log_SPR0_static, log_SSB_all not in single?
  
  rownames(sdrep_single)[grep("log_F1", rownames(sdrep_single), fixed = TRUE)] <- "F_pars" # Rename initial F estimate F_pars so grouped with other F_pars as in multi-wham (should work since listed first in sdrep)
  
  rownames(sdrep_single)[grep("F_devs", rownames(sdrep_single), fixed = TRUE)] <-  paste0("F_pars.", 1:length(rownames(sdrep_single)[grep("F_devs", rownames(sdrep_single), fixed = TRUE)]))# rownames(sdrep_single)[grep("F_devs", rownames(sdrep_single), fixed = TRUE)] %>% gsub(x=., pattern = "F_devs", replacement = "F_pars") # F_devs -> F_pars
  rownames(sdrep_single)[grep("log_N1_pars", rownames(sdrep_single), fixed = TRUE)] <- rownames(sdrep_single)[grep("log_N1_pars", rownames(sdrep_single), fixed = TRUE)] %>% gsub(x=., pattern = "log_N1_pars", replacement = "log_N1") # log_N1_pars -> log_N1
  rownames(sdrep_single)[grep("log_FAA_tot", rownames(sdrep_single), fixed = TRUE)] <- rownames(sdrep_single)[grep("log_FAA_tot", rownames(sdrep_single), fixed = TRUE)] %>% gsub(x=., pattern = "log_FAA_tot", replacement = "log_FAA_by_region") # log_FAA_tot -> log_FAA_by_region, works because this is a single area/stock/region model
  rownames(sdrep_single)[grepl("log_F", rownames(sdrep_single), fixed = TRUE) == TRUE & 
                           grepl("log_FXSPR", rownames(sdrep_single), fixed = TRUE) == FALSE & 
                           grepl("log_F1", rownames(sdrep_single), fixed = TRUE) == FALSE & 
                           grepl("log_FAA", rownames(sdrep_single), fixed = TRUE) == FALSE & 
                           grepl("log_Fbar", rownames(sdrep_single), fixed = TRUE) == FALSE] <- rownames(sdrep_single)[grepl("log_F", rownames(sdrep_single), fixed = TRUE) == TRUE & 
                                                                                                                         grepl("log_FXSPR", rownames(sdrep_single), fixed = TRUE) == FALSE & 
                                                                                                                         grepl("log_F1", rownames(sdrep_single), fixed = TRUE) == FALSE & 
                                                                                                                         grepl("log_FAA", rownames(sdrep_single), fixed = TRUE) == FALSE & 
                                                                                                                         grepl("log_Fbar", rownames(sdrep_single), fixed = TRUE) == FALSE] %>% gsub(x=., pattern = "log_F", replacement = "log_F_tot") # log_F single log_F_tot multi

  
  sdrep_single <- sdrep_single  %>% round(3) %>% mutate(labels = rownames(.))
  #sdrep_single[grep("F_pars", rownames(sdrep_single), fixed = TRUE), "YEAR"] <- mod_single$years[-1] # Single-wham model F_pars start in 2nd year with initial F labeled separately so use YEAR to merge these parameters
  sdrep_multi <- sdrep_multi  %>% round(3) %>% mutate(labels = rownames(.))
  #sdrep_multi[grep("F_pars", rownames(sdrep_multi), fixed = TRUE), "labels"] <- mod_multi$years
  
  
  sdrep_both <- full_join(sdrep_single, sdrep_multi) # If all parameter estimates AND names are the same then nrow(sdrep_signle) = nrow(sdrep_multi) = nrow(sdrep_both), 
  nrow(sdrep_single)+ nrow(sdrep_multi) > nrow(sdrep_both) # If true then at least some parameter estimates match (may not match if not rounded due to incredibly small differences in decimal)
  different_pars <- sdrep_both %>% group_by(labels) %>% summarise(n = n()) %>% filter(n > 1) %>% select(labels) %>% unlist()
 
  # Save different parameters by model in CSV
  sdrep_single_diff <- sdrep_single %>% mutate(model = "single_wham") %>% filter(labels %in% different_pars)
  sdrep_multi_diff <- sdrep_multi %>% mutate(model = "multi_wham") %>% filter(labels %in% different_pars)
  sdrep_both_diff <- full_join(sdrep_single_diff, sdrep_multi_diff) %>% filter(labels %in% different_pars) 
  
  # Generate comparison plots (modify compare_wham_models plots to work with both single and multi-wham) #####
  wham.mods = list(mod_single, mod_multi)
  no.wham = FALSE # Only WHAM models can be compared by this function
  compare.opts <- NULL
  compare.opts$stock = 1
  compare.opts$region = 1
  stock <- compare.opts$stock
  region <- compare.opts$region
  
  ## Generate AIC/Mohn's rho table #####
  if(is.null(table.opts)) table.opts=list(fname = "model_comparison", sort = TRUE, calc.rho = TRUE, calc.aic = TRUE, print=TRUE, save.csv=TRUE)
  if(is.null(table.opts$fname)) table.opts$fname = "model_comparison"
  if(is.null(table.opts$sort)) table.opts$sort = TRUE
  if(is.null(table.opts$calc.rho)) table.opts$calc.rho = TRUE
  if(is.null(table.opts$calc.aic)) table.opts$calc.aic = TRUE
  if(is.null(table.opts$print)) table.opts$print = TRUE
  if(is.null(table.opts$save.csv)) table.opts$save.csv = FALSE
  aic.tab <- aic <- daic <- NULL
  # AIC
  aic <- sapply(wham.mods, function(x){
    k = length(x$opt$par)
    2*(x$opt$obj + k) # AIC
    # 2*(x$opt$obj + k + k*(k+1)/(n-k-1)) # AICc
  })
  aic <- round(aic, 1)
  daic <- round(aic - min(aic), 1)
  aic.tab <- cbind(daic, aic)
  colnames(aic.tab) <- c("dAIC","AIC")
  tab <- cbind(aic.tab)
  
      # Single-wham copy of mohns_rho() function
      npeels = length(mod_single$peels)
      ny = mod_single$env$data$n_years_model
      na = mod_single$env$data$n_ages
      if(npeels)
      {
        rho = c(
          mean(sapply(1:npeels, function(x) mod_single$peels[[x]]$rep$SSB[ny-x]/mod_single$rep$SSB[ny-x] - 1)),
          mean(sapply(1:npeels, function(x) mod_single$peels[[x]]$rep$Fbar[ny-x]/mod_single$rep$Fbar[ny-x] - 1)))#,
        #mean(sapply(1:npeels, function(x) mod_single$peels[[x]]$rep$NAA[ny-x,1]/mod_single$rep$NAA[ny-x,1] - 1)))
        names(rho) = c("SSB","Fbar")#,"R")
        rho.naa = sapply(1:na, function(y)
        {
          mean(sapply(1:npeels, function(x) mod_single$peels[[x]]$rep$NAA[ny-x,y]/mod_single$rep$NAA[ny-x,y] - 1))
        })
        names(rho.naa) = c("R", paste0("N", mod_single$ages.lab[2:na]))
        rho = c(rho, rho.naa)
      }
      rho_single <- t(rho)[,c("R", "SSB", "Fbar")] 
      names(rho_single) <- paste0("rho_", c("R", "SSB", "Fbar"))
      
      # Multi-wham copy of mohns_rho() function
      npeels = length(mod_multi$peels)
      data <- mod_multi$env$data
      ny = data$n_years_model
      na = data$n_ages
      if(npeels)
      {
        rho  = list()
        rho$SSB <- rho$Fbar <- numeric()
        for(i in 1:data$n_stocks) rho$SSB[i] <- mean(sapply(1:npeels, function(x) mod_multi$peels[[x]]$rep$SSB[ny-x, i]/mod_multi$rep$SSB[ny-x, i] - 1))
        for(i in 1:data$n_regions) rho$Fbar[i] <- mean(sapply(1:npeels, function(x) {
          mean(mod_multi$peels[[x]]$rep$Fbar[ny-x,i])/mean(mod_multi$rep$Fbar[ny-x,i]) - 1
        }))
        #for(i in 1:data$n_regions) rho$Fbar[i] <- mean(sapply(1:npeels, function(x) mod_multi$peels[[x]]$rep$Fbar[ny-x,i]/mod_multi$rep$Fbar[ny-x,i] - 1))
        #names(rho) = c("SSB","Fbar")#,"R")
        rho$naa <- array(NA, c(data$n_stocks, data$n_regions, data$n_ages))
        for(s in 1:data$n_stocks) for(r in 1:data$n_regions) for(a in 1:data$n_ages) if(data$NAA_where[s,r,a]) {
          rho$naa[s,r,a] = mean(sapply(1:npeels, function(x) mod_multi$peels[[x]]$rep$NAA[s,r,ny-x,a]/mod_multi$rep$NAA[s,r,ny-x,a] - 1))
        }
        dimnames(rho$naa)[[3]] = c("R", paste0("N", mod_multi$ages.lab[2:na]))
        #rho = c(rho, rho.naa)
      }
      rho_multi <- c(rho$naa[stock,region,1], `SSB` = rho$SSB[stock], `Fbar` = rho$Fbar[region]) %>% t() 
      colnames(rho_multi) <- paste0("rho_",c("R","SSB","Fbar"))
    
  rhoCols = rbind(rho_single = rho_single, rho_multi = rho_multi)
  tab <- cbind(tab, rhoCols) # Append results to output table (not sorted by AIC as in compare_wham_models)
  rownames(tab) <- c("single_wham", "multi_wham")
  
  if(!is.null(tab)){ 
    if(table.opts$save.csv) write.csv(tab, file = paste0(file.path(fdir, table.opts$fname),".csv")) # Option to save table as CSV
    if(table.opts$print) print(tab) # print to console
  }
  
  ## Comparison plots #####
  mods = wham.mods
  if(is.null(plot.opts)) plot.opts=list(out.type='png', ci=TRUE, years=NULL, which=1:10, relative.to=NULL, alpha=0.05, ages.lab=mods[[1]]$ages.lab, kobe.yr=NULL, M.age=NULL, return.ggplot=TRUE, kobe.prob=TRUE)
  if(is.null(plot.opts$out.type)) plot.opts$out.type <- 'png'
  if(!plot.opts$out.type %in% c("pdf","png")) stop("plot.opts$out.type must be 'pdf' or 'png' (default)")
  if(is.null(plot.opts$ci)) plot.opts$ci <- TRUE
  if(!length(plot.opts$ci) %in% c(1,length(mods))) stop("plot.opts$ci must have length = 1 or n.models")
  if(length(plot.opts$ci) == 1) plot.opts$ci <- rep(plot.opts$ci, length(mods))
  all.yrs <- unique(unlist(lapply(mods, function(x) x$years_full)))
  if(is.null(plot.opts$years)) plot.opts$years <- all.yrs
  if(!all(plot.opts$years %in% all.yrs)) stop("plot.opts$years must be a subset of $years_full in model fits")
  if(!is.null(plot.opts$relative.to)) if(!plot.opts$relative.to %in% names(mods)) stop("plot.opts$relative.to must match a model name, e.g. 'm1'.")
  if(is.null(plot.opts$which)) plot.opts$which <- 1:10
  if(!all(plot.opts$which %in% 1:10)) stop("All elements of plot.opts$which must be in 1:10. See ?compare_wham_models for available plots.")
  if(is.null(plot.opts$alpha)) plot.opts$alpha <- 0.05
  if(is.null(plot.opts[["kobe.yr"]])) plot.opts$kobe.yr <- tail(mods[[1]]$years, 1)
  if(!(plot.opts$kobe.yr %in% all.yrs)) stop("plot.opts$kobe.yr must be a year in $years_full from model fits.")
  if(is.null(plot.opts[["return.ggplot"]])) plot.opts$return.ggplot <- TRUE
  if(is.null(plot.opts[["kobe.prob"]])) plot.opts$kobe.prob <- TRUE
  if(is.null(plot.opts[["refpt"]])) plot.opts$refpt <- "XSPR"
  if(!plot.opts$refpt %in% c("XSPR","MSY")) stop("plot.opts$refpt must be either 'XSPR' or 'MSY'.")
  
  if(is.null(plot.opts$ages.lab)){
    if(!no.wham) plot.opts$ages.lab <- wham.mods[[1]]$ages.lab
    if(is.null(plot.opts$ages.lab)) plot.opts$ages.lab <- paste0(1:dim(x[[1]]$MAA)[2], c(rep("",dim(x[[1]]$MAA)[2]-1),"+"))
  }
  if(is.null(plot.opts$M.age)){
    if(!no.wham) plot.opts$M.age <- max(wham.mods[[1]]$env$data$which_F_age)
    if(is.null(plot.opts$M.age)) plot.opts$M.age <- dim(x[[1]]$MAA)[2]
  }
  
x <- NULL
x[[1]] <- read_wham_fit(wham.mods[[1]]) # Pulls out plotting information, labels may still differ between single and multi-wham
x[[2]] <- read_wham_fit(wham.mods[[2]])
names(x) = c("single_wham", "multi_wham")

# Rename mod_single WHAM plotting info to match multi-wham
x[[1]]$log_SSB_all <- x[[1]]$log_SSB
x[[1]]$log_F_tot <- x[[1]]$log_F

x[[1]]$is.wham = TRUE # Since comparing single and multi-wham
x[[2]]$is.wham = TRUE


# Generate plots
g <-  vector("list", 10)
no.SPR <- sapply(x, function(x) !("log_FXSPR" %in% names(x)))
no.MSY <- sapply(x, function(x) !("log_FMSY" %in% names(x)))
if((any(no.SPR) & plot.opts$refpt == "XSPR") | (any(no.MSY) & plot.opts$refpt == "MSY")){
  if(plot.opts$refpt == "XSPR"){
    warning("Not plotting SPR-based reference points and status because at least one model does not have them reported.")
  }
  if(plot.opts$refpt == "MSY"){
    warning("Not plotting MSY-based reference points and status because at least one model does not have them reported.")
  }
  plot.opts$which <- plot.opts$which[which(!(plot.opts$which %in% 8:10))]
}
for(i in plot.opts$which){
  if(i==1) g[[i]] <- suppressWarnings(plot.SSB.F.R.compare(x, compare.opts, plot.opts))
  if(i==2) g[[i]] <- suppressWarnings(plot.cv.compare(x, compare.opts, plot.opts))
  if(i==3) g[[i]] <- suppressWarnings(plot.selectivity.compare(x, plot.opts, type="fleet"))
  if(i==4) g[[i]] <- suppressWarnings(plot.selectivity.compare(x, plot.opts, type="indices"))
  if(i==5) g[[i]] <- suppressWarnings(plot.tile.compare(x, compare.opts, plot.opts, type="selAA"))
  if(i==6) g[[i]] <- suppressWarnings(plot.M.compare(x, compare.opts, plot.opts))
  if(i==7) g[[i]] <- suppressWarnings(plot.tile.compare(x, compare.opts, plot.opts, type="MAA"))
  if(i==8) g[[i]] <- suppressWarnings(plot.refpt.compare(x, plot.opts))
  if(i==9) g[[i]] <- suppressWarnings(plot.rel.compare(x, plot.opts))
}
pdims <- lapply(g, gg_facet_dims)
pdims[[10]] <- c(7,7)
plabs <- c("SSB_F_R","CV","sel_fleets","sel_indices","sel_tile",paste0("M_age",plot.opts$M.age),"M_tile","ref_pts","rel_status_timeseries","rel_status_kobe")
if(plot.opts$out.type == 'pdf'){
  pnames <- file.path(fdir,"compare_pdf",paste0("compare_",plabs,".pdf"))
  if(!dir.exists(file.path(fdir,"compare_pdf"))) dir.create(file.path(fdir,"compare_pdf"))
}
if(plot.opts$out.type == 'png'){
  pnames <- file.path(fdir,"compare_png",paste0("compare_",plabs,".png"))
  if(!dir.exists(file.path(fdir,"compare_png"))) dir.create(file.path(fdir,"compare_png"))
}
for(i in plot.opts$which){
  if(plot.opts$out.type == 'pdf') grDevices::cairo_pdf(filename=pnames[i], height = pdims[[i]][1], width = pdims[[i]][2])
  if(plot.opts$out.type == 'png') png(pnames[i], width=pdims[[i]][2], height=pdims[[i]][1], units="in", res=300)
  if(i < 10) suppressWarnings(print(g[[i]]))
  if(i == 10) g[[i]] <- suppressWarnings(plot.kobe.compare(x, plot.opts))
  dev.off()
}
if(plot.opts$return.ggplot){ 
  out = NULL
  out$g <- g
  }
#}
# 

# Return
return_list = NULL
return_list$sdrep_summary <- sdrep_both # Summary of joined sdrep objects, repeated labels indicate differences in estimated values (rounded to 3 decimals)
return_list$different_pars <- different_pars # Vector of parameter names that had different estimates between the single and multi-wham models
return_list$tab <- tab # Table of AIC and Mohn's rho values by model
return_list$out <- out # Comparison plots
return_list$sdrep_diff <- sdrep_both_diff # Table of parameter estimates that differ between single and multi-wham versions of the model

return(return_list)
} # end of bridge_multiWHAM function


#' @title Helper function: gg_facet_dims
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export
 
# Helper functions pulled from multi-wham compare_wham_models.R with some modifications #####
## gg_facet_dims #####
gg_facet_dims <- function(p){
  if(!is.null(p)){
    nrows <- p %>%
      ggplot2::ggplot_build() %>%
      magrittr::extract2('layout') %>%
      magrittr::extract2('layout') %>%
      magrittr::extract2('ROW') %>%
      unique() %>%
      length()
    ncols <- p %>%
      ggplot2::ggplot_build() %>%
      magrittr::extract2('layout') %>%
      magrittr::extract2('layout') %>%
      magrittr::extract2('COL') %>%
      unique() %>%
      length()
    return(c(2*nrows+2, 3*ncols+2))
  } else return(NULL)
}

#' @title Helper function: fancy_scientific
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## fancy_scientific #####
fancy_scientific <- function(l) {
  if(max(l, na.rm=T) < 100){
    l <- format(l, scientific = FALSE, digits=2)
  } else {
    l <- format(l, scientific = TRUE, digits=2)
    l <- gsub("0e\\+00","0",l)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("e\\+","e",l)
    l <- gsub("e", "%*%10^", l)
    l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
  }
  parse(text=l)
}

#' @title Helper function: get.ci
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## get.ci #####
get.ci = function(par,se, p=0.975, lo = 0, hi = 1, type = "I", k = 1, alpha.ci=0.05, getci=TRUE, asap = FALSE){
  if(!getci) {
    ci <- list(lo = par, hi = par)
    ci$lo[] <- 0
    ci$hi[] <- 0
  } else {
    if(!asap) ci <- list(lo = par - qnorm(1-alpha.ci/2) * se, hi = par + qnorm(1-alpha.ci/2) * se)
    else ci <- list(lo = cbind(par[,1] - qnorm(1-alpha.ci/2)* par[,2]), hi = cbind(par[,1] + qnorm(1-alpha.ci/2)* par[,2]))
  }
  if(type == "I") {
  }
  if(type == "exp") {
    ci <- lapply(ci, exp)
  }
  if(type == "expit") { #Delta-method: V(lo + (hi-lo)/(1 + exp(-x))) ~ ((hi-lo) * p * (1-p))^2 * V(x)
    ci <- lapply(ci, function(x) lo + (hi-lo)/(1+ exp(-k * x)))
  }
  return(ci)
}

#' @title Helper function: plot.timeseries.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.timeseries.compare #####
plot.timeseries.compare <- function(df, x, plot.opts){
  df <- df[df$Year %in% plot.opts$years,]
  df$Model <- factor(df$Model, levels=names(x), labels=names(x))
  df$Year <- as.integer(df$Year)
  if(any(plot.opts$ci)){
    df <- df %>% dplyr::group_by(var) %>% dplyr::mutate(y_max = 1.2*max(val)) %>% as.data.frame   # trim large CIs to zoom to 120% of max MLE
    ind <- which(df$hi > df$y_max)
    df$hi[ind] = df$y_max[ind]
  }
  
  g <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=val, color=Model, group=Model))
  if(any(plot.opts$ci)){
    g <- g + ggplot2::geom_ribbon(ggplot2::aes(x=Year, ymin=lo, ymax=hi, fill=Model), color=NA, alpha=.15) +
      ggplot2::scale_fill_viridis_d()
  }
  g <- g + ggplot2::geom_line(size=.8) +
    ggplot2::facet_wrap(ggplot2::vars(var), scales="free_y", ncol=1, strip.position = "left") +
    ggplot2::ylab(NULL) +
    ggplot2::scale_x_continuous(expand=c(0.01,0.01)) + # breaks=scales::breaks_extended(5)
    ggplot2::scale_colour_viridis_d() +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank(), strip.placement = "outside",
                   legend.position="top", legend.box.margin = ggplot2::margin(0,0,0,0), legend.margin = ggplot2::margin(0,0,0,0))
  # if not relative, force y min to 0
  if(is.null(plot.opts$relative.to)){
    g <- g + ggplot2::scale_y_continuous(expand=c(0.01,0.01), limits = c(0,NA), labels=fancy_scientific)
  } else {
    g <- g + ggplot2::scale_y_continuous(expand=c(0.01,0.01), labels=fancy_scientific)
  }
  # if projections, add vline at terminal year
  last_proj <- sapply(x, function(x) tail(x$years_full,1))
  last_mod <- sapply(x, function(x) tail(x$years,1))
  v.yr <- unique(last_mod[last_proj > last_mod])
  g <- g + ggplot2::geom_vline(xintercept = v.yr, linetype=2, size=.4)
  
  return(g)
}

#' @title Helper function: plot.SSB.F.R.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.SSB.F.R.compare #####
plot.SSB.F.R.compare <- function(x, compare.opts, plot.opts){
  stock <- compare.opts$stock
  region <- compare.opts$region
  df <- data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(df) <- c("Year","var","val","lo","hi","Model")
  if(!is.null(plot.opts$relative.to)){
    # base.i <- which(names(x) == plot.opts$relative.to)
    # plot.opts$ci <- rep(FALSE, length(x)) #no confidence intervals
    # for(i in 1:length(x)){
    #   if(x[[i]]$is.wham){
    #     if(x[[base.i]]$is.wham) ratio <- exp(x[[i]]$log_SSB_all$est)/exp(x[[base.i]]$log_SSB_all$est)
    #     else ratio <- exp(x[[i]]$log_SSB_all$est)/exp(x[[base.i]]$log_SSB[,1])
    #   } else {
    #     if(x[[base.i]]$is.wham) ratio <- exp(x[[i]]$log_SSB[,1])/exp(x[[base.i]]$log_SSB_all$est)
    #     else ratio <- exp(x[[i]]$log_SSB[,1])/exp(x[[base.i]]$log_SSB[,1])
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("SSB relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=ratio, hi=ratio, Model=names(x)[i]))
    #   if(x[[i]]$is.wham){
    #     if(x[[base.i]]$is.wham) ratio <- exp(x[[i]]$log_F_tot$est)/exp(x[[base.i]]$log_F_tot$est)
    #     else ratio <- exp(x[[i]]$log_F_tot$est)/exp(x[[base.i]]$log_F[,1])
    #   } else {
    #     if(x[[base.i]]$is.wham) ratio <- exp(x[[i]]$log_SSB[,1])/exp(x[[base.i]]$log_SSB_all$est)
    #     else ratio <- exp(x[[i]]$log_F[,1])/exp(x[[base.i]]$log_F[,1])
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("F relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=ratio, hi=ratio, Model=names(x)[i]))
    #   if(x[[i]]$is.wham){
    #     if(x[[base.i]]$is.wham) ratio <- exp(x[[i]]$log_NAA_rep$est[stock,region,,1])/exp(x[[base.i]]$log_NAA_rep$est[stock,region,,1])
    #     else ratio <- exp(x[[i]]$log_NAA_rep$est[stock,region,,1])/exp(x[[base.i]]$log_NAA[,1])
    #   } else {
    #     if(x[[base.i]]$is.wham) ratio <- exp(x[[i]]$log_NAA[,1])/exp(x[[base.i]]$log_NAA_rep$est[stock,region,,1])
    #     else ratio <- exp(x[[i]]$log_NAA[,1])/exp(x[[base.i]]$log_NAA[,1])
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("Recruitment relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=ratio, hi=ratio, Model=names(x)[i]))
    # }
  } else {
    for(i in 1:length(x)){
      if(x[[i]]$is.wham){
        df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="SSB", val=exp(x[[i]]$log_SSB_all$est), lo=x[[i]]$log_SSB_all$ci$lo, 
                                   hi=x[[i]]$log_SSB_all$ci$hi, Model=names(x)[i]))
        df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="F", val=exp(x[[i]]$log_F_tot$est), lo=x[[i]]$log_F_tot$ci$lo, 
                                   hi=x[[i]]$log_F_tot$ci$hi, Model=names(x)[i]))
        if(names(x)[i] == "single_wham"){
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="Recruitment", val=exp(x[[i]]$log_NAA_rep$est[,1]), 
                                     lo=x[[i]]$log_NAA_rep$ci$lo[,1], hi=x[[i]]$log_NAA_rep$ci$hi[,1], Model=names(x)[i]))
        } else{ # multi-wham dimensions
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="Recruitment", val=exp(x[[i]]$log_NAA_rep$est[stock,region,,1]), 
                                     lo=x[[i]]$log_NAA_rep$ci$lo[stock,region,,1], hi=x[[i]]$log_NAA_rep$ci$hi[stock,region,,1], Model=names(x)[i]))
        }
      } 
      # else{
      #   ci <- get.ci(x[[i]][["log_SSB"]], plot.opts$alpha, plot.opts$ci[i], asap = TRUE)
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="SSB", val=exp(x[[i]][["log_SSB"]][,1]), lo=ci$lo[,1], hi=ci$hi[,1], Model=names(x)[i]))
      #   ci <- get.ci(x[[i]][["log_F"]], plot.opts$alpha, plot.opts$ci[i], asap = TRUE)
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="F", val=exp(x[[i]][["log_F"]][,1]), lo=ci$lo[,1], hi=ci$hi[,1], Model=names(x)[i]))
      #   ci <- get.ci(cbind(x[[i]]$log_NAA[,1], x[[i]]$NAA_CV[,1]), plot.opts$alpha, plot.opts$ci[i], asap = TRUE)
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="Recruitment", val=exp(x[[i]]$log_NAA[,1]), lo=ci$lo[,1], hi=ci$hi[,1], Model=names(x)[i]))
      # }
    }
  }
  g <- plot.timeseries.compare(df, x, plot.opts)
  return(g)
}

#' @title Helper function: plot.cv.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.cv.compare #####
plot.cv.compare <- function(x, compare.opts, plot.opts){
  stock <- compare.opts$stock
  region <- compare.opts$region
  plot.opts$ci <- rep(FALSE, length(x))
  df <- data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(df) <- c("Year","var","val","lo","hi","Model")
  if(!is.null(plot.opts$relative.to)){
    # base.i <- which(names(x) == plot.opts$relative.to)
    # for(i in 1:length(x)){
    #   if(x[[i]]$is.wham){
    #     if(x[[base.i]]$is.wham) ratio <- x[[i]]$log_SSB_all$se/x[[base.i]]$log_SSB_all$se
    #     else ratio <- x[[i]]$log_SSB_all$se/x[[base.i]]$log_SSB[,2]
    #   } else {
    #     if(x[[base.i]]$is.wham) ratio <- x[[i]]$log_SSB[,2]/x[[base.i]]$log_SSB_all$se
    #     else ratio <- x[[i]]$log_SSB[,2]/x[[base.i]]$log_SSB[,2]
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("CV(SSB) relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=NA, hi=NA, Model=names(x)[i]))
    #   if(x[[i]]$is.wham){
    #     if(x[[base.i]]$is.wham) ratio <- x[[i]]$log_F_tot$se/x[[base.i]]$log_F_tot$se
    #     else ratio <- x[[i]]$log_F_tot$se/x[[base.i]]$log_F[,2]
    #   } else {
    #     if(x[[base.i]]$is.wham) ratio <- x[[i]]$log_F[,2]/x[[base.i]]$log_F_tot$se
    #     else ratio <- x[[i]]$log_F[,2]/x[[base.i]]$log_F[,2]
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("CV(F) relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=NA, hi=NA, Model=names(x)[i]))
    #   if(x[[i]]$is.wham){
    #     if(x[[base.i]]$is.wham) ratio <- x[[i]]$log_NAA_rep$se[stock,region,,1]/x[[base.i]]$log_NAA_rep$se[stock,region,,1]
    #     else ratio <- x[[i]]$log_NAA_rep$se[stock,region,,1]/x[[base.i]]$NAA_CV[,1]
    #   } else {
    #     if(x[[base.i]]$is.wham) ratio <- x[[i]]$NAA_CV[,1]/x[[base.i]]$log_NAA_rep$se[stock,region,,1]
    #     else ratio <- x[[i]]$NAA_CV[,1]/x[[base.i]]$NAA_CV[,1]
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("CV(Rec) relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=NA, hi=NA, Model=names(x)[i]))
    # }
  } else {
    for(i in 1:length(x)){
      if(x[[i]]$is.wham){
        df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(SSB)", val=x[[i]][["log_SSB_all"]]$se, lo=NA, hi=NA, Model=names(x)[i]))
        df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(F)", val=x[[i]][["log_F_tot"]]$se, lo=NA, hi=NA, Model=names(x)[i]))
        if(names(x)[i] == "single_wham"){
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(Rec)", val=x[[i]][["log_NAA_rep"]]$se[,1], lo=NA, hi=NA, Model=names(x)[i]))
        } else{ # Multi-wham dimensions
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(Rec)", val=x[[i]][["log_NAA_rep"]]$se[stock,region,,1], lo=NA, hi=NA, Model=names(x)[i]))
        }
      } 
      # else {
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(SSB)", val=x[[i]][["log_SSB"]][,2], lo=NA, hi=NA, Model=names(x)[i]))
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(F)", val=x[[i]][["log_F"]][,2], lo=NA, hi=NA, Model=names(x)[i]))
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var="CV(Rec)", val=x[[i]][["NAA_CV"]][,1], lo=NA, hi=NA, Model=names(x)[i]))
      # }
    }
  }
  g <- plot.timeseries.compare(df, x, plot.opts)
  return(g)
}

#' @title Helper function: plot.selectivity.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.selectivity.compare #####
plot.selectivity.compare <- function(x, plot.opts, type="fleet"){
  plot.opts$ci <- rep(FALSE, length(x))
  n_ages = length(plot.opts$ages.lab)
  allSame <- function(x) length(unique(x)) == 1
  if(type == 'fleet'){
    if(!allSame(lapply(x, function(y) unname(y$selblock_pointer_fleets)))) {
      cat("Fleet selectivity blocks not identical, cannot produce comparison plot.")
      return(NULL)
    } else {
      selblocks <- unique(as.integer(x[[1]]$selblock_pointer_fleets))
      yrs <- lapply(selblocks, function(y){
        tmp <- which(x[[1]]$selblock_pointer_fleets == y);
        tmp <- tmp %% length(x[[1]]$years);
        tmp[tmp == 0] = length(x[[1]]$years);
        return(tmp)})
    }
  }
  if(type == 'indices'){
    if(!allSame(lapply(x, function(y) unname(y$selblock_pointer_indices)))) stop("Index selectivity blocks not identical, cannot produce comparison plot")
    selblocks <- unique(as.integer(x[[1]]$selblock_pointer_indices))
    yrs <- lapply(selblocks, function(y){
      tmp <- which(x[[1]]$selblock_pointer_indices == y);
      tmp <- tmp %% length(x[[1]]$years);
      tmp[tmp == 0] = length(x[[1]]$years);
      return(tmp)})
  }
  df <- data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(df) <- c("Age","Selectivity","Block","Model")
  for(i in 1:length(x)){
    for(j in selblocks){
      sel <- apply(x[[i]][["selAA"]][[j]][yrs[[which(selblocks==j)]],], 2, mean, na.rm=T)
      df <- rbind(df, data.frame(Age=1:n_ages, Selectivity=sel, Block=paste0("Block ",j), Model=names(x)[i]))
    }
  }
  df$Model <- factor(df$Model, levels=names(x), labels=names(x))
  df$Block <- as.factor(df$Block)
  g <- ggplot2::ggplot(df, ggplot2::aes(x=Age, y=Selectivity, color=Model, group=Model)) +
    ggplot2::geom_line(size=.8) +
    ggplot2::facet_wrap(ggplot2::vars(Block), ncol=1, strip.position = 'right') +
    ggplot2::ylab("Selectivity") +
    ggplot2::scale_y_continuous(limits=c(0,1),expand=c(0.01,0.01), labels=scales::number_format(accuracy=0.1)) +
    ggplot2::scale_x_continuous(expand=c(0.01,0.01), breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="top", legend.box.margin = ggplot2::margin(0,0,0,0), legend.margin = ggplot2::margin(0,0,0,0))
  return(g)
}

#' @title Helper function: plot.tile.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.tile.compare #####
# 2D tile plot by age and year (e.g. selAA, MAA)
plot.tile.compare <- function(x, compare.opts, plot.opts, type="selAA"){
  n_ages = length(plot.opts$ages.lab)
  stock <- compare.opts$stock
  region <- compare.opts$region  
  
  if(type=="selAA"){
    n_years = length(x[[1]]$years)
    df <- data.frame(matrix(NA, nrow=0, ncol=5))
    colnames(df) <- c("Year","Age","Selectivity","Fleet","Model")
    for(j in 1:length(x)){
      selblock_pointer_all <- cbind(x[[j]]$selblock_pointer_fleets, x[[j]]$selblock_pointer_indices)
      n_selblocks <- length(x[[j]]$selAA)
      n_f <- dim(x[[j]]$selblock_pointer_fleets)[2]
      n_i <- dim(x[[j]]$selblock_pointer_indices)[2]
      selblocks_f <- as.numeric(unique(x[[j]]$selblock_pointer_fleets))
      selAA.byfleet <- vector("list", n_f+n_i)
      for(i in 1:(n_f+n_i)){
        selAA.byfleet[[i]] <- matrix(NA, nrow=n_years, ncol=n_ages)
        for(y in 1:n_years){
          selAA.byfleet[[i]][y,] <- x[[j]]$selAA[[selblock_pointer_all[y,i]]][y,]
        }
      }
      names(selAA.byfleet) <- c(paste0("Fleet ",1:n_f), paste0("Index ",1:n_i))
      
      df.selAA <- data.frame(matrix(NA, nrow=0, ncol=n_ages+2))
      colnames(df.selAA) <- c(paste0("Age_",1:n_ages),"Year","Fleet")
      for(i in 1:(n_f+n_i)){
        tmp = as.data.frame(selAA.byfleet[[i]])
        tmp$Year <- x[[j]]$years
        colnames(tmp) <- c(paste0("Age_",1:n_ages),"Year")
        tmp$Fleet = names(selAA.byfleet)[i]
        df.selAA <- rbind(df.selAA, tmp)
      }
      df <- rbind(df, df.selAA %>% tidyr::pivot_longer(-c(Year,Fleet),
                                                       names_to = "Age",
                                                       names_prefix = "Age_",
                                                       names_ptypes = list(Age = character()),
                                                       values_to = "Selectivity") %>%
                    dplyr::mutate(Model = names(x)[j]))
    }
    df$Age <- as.integer(df$Age)
    df$Fleet <- factor(as.character(df$Fleet), levels=names(table(df$Fleet)))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=Age, fill=Selectivity)) +
      ggplot2::facet_grid(cols=ggplot2::vars(Fleet), rows=ggplot2::vars(Model))
  }
  
  if(type=="MAA"){
    n_years = length(x[[1]]$years_full)
    df <- data.frame(matrix(NA, nrow=0, ncol=4))
    colnames(df) <- c("Year","Age","M","Model")
    for(j in 1:length(x)){
      if(x[[j]]$is.wham){
        if(names(x)[j] == "single_wham"){
          MAA <- as.data.frame(x[[j]][["MAA"]])
        } else{ #Multi-wham dimensions
          MAA <- as.data.frame(x[[j]][["MAA"]][stock,region,,])
        }
        
      } else {
        MAA <- as.data.frame(x[[j]][["MAA"]])
      }
      MAA$Year <- x[[j]]$years_full
      colnames(MAA) <- c(paste0("Age_",1:n_ages),"Year")
      df <- rbind(df, MAA %>% tidyr::pivot_longer(-Year,
                                                  names_to = "Age",
                                                  names_prefix = "Age_",
                                                  names_ptypes = list(Age = character()),
                                                  values_to = "M") %>%
                    dplyr::mutate(Model = names(x)[j]))
    }
    df$Age <- as.integer(df$Age)
    g <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=Age, fill=M)) +
      ggplot2::facet_wrap(ggplot2::vars(Model), dir="v")
  }
  g <- g + ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0), breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    ggplot2::theme_bw() +
    viridis::scale_fill_viridis()
  return(g)
}

#' @title Helper function: plot.M.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.M.compare #####
plot.M.compare <- function(x, compare.opts, plot.opts){
  stock <- compare.opts$stock
  region <- compare.opts$region  
  plot.opts$ci <- rep(FALSE, length(x))
  df <- data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(df) <- c("Year","var","val","lo","hi","Model")
  if(!is.null(plot.opts$relative.to)){
    # base.i <- which(names(x) == plot.opts$relative.to)
    # for(i in 1:length(x)){
    #   if(x[[base.i]]$is.wham){
    #     if(x[[i]]$is.wham){
    #       ratio <- x[[i]][["MAA"]][stock,region,,plot.opts$M.age]/x[[i]][["MAA"]][stock,region,,plot.opts$M.age]
    #     } else{
    #       ratio <- x[[i]][["MAA"]][,plot.opts$M.age]/x[[i]][["MAA"]][stock,region,,plot.opts$M.age]
    #     }
    #   } else{
    #     if(x[[i]]$is.wham){
    #       ratio <- x[[i]][["MAA"]][stock,region,,plot.opts$M.age]/x[[base.i]][["MAA"]][,plot.opts$M.age]
    #     } else{
    #       ratio <- x[[i]][["MAA"]][,plot.opts$M.age]/x[[base.i]][["MAA"]][,plot.opts$M.age]
    #     }        
    #   }
    #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("M at age ",plot.opts$M.age," relative to ",plot.opts$relative.to),
    #                              val=ratio, lo=NA, hi=NA, Model=names(x)[i]))
    # }
  } else {
    for(i in 1:length(x)){
      if(x[[i]]$is.wham){
        if(names(x)[i] == "single_wham"){
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("M at age ",plot.opts$M.age), val=x[[i]][["MAA"]][,plot.opts$M.age], lo=NA, hi=NA, Model=names(x)[i]))
        } else{ # Multi-wham dimensions
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("M at age ",plot.opts$M.age), val=x[[i]][["MAA"]][stock,region,,plot.opts$M.age], lo=NA, hi=NA, Model=names(x)[i]))
        }
      } 
      # else {
      #   df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=paste0("M at age ",plot.opts$M.age), val=x[[i]][["MAA"]][,plot.opts$M.age], lo=NA, hi=NA, Model=names(x)[i]))
      # }
    }
  }
  g <- plot.timeseries.compare(df, x, plot.opts)
  return(g)
}

#' @title Helper function: plot.refpt.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.refpt.compare #####
plot.refpt.compare <- function(x, plot.opts){
  df <- data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(df) <- c("Year","var","val","lo","hi","Model")
  nms.df <- c("F_refpt","SSB_refpt","Y_refpt")
  if(plot.opts$refpt == "XSPR"){
    nms <- c("log_FXSPR","log_SSB_FXSPR","log_Y_FXSPR")
  } else{ # MSY ref pt
    nms <- c("log_FMSY","log_SSB_MSY","log_MSY")
  }
  if(!is.null(plot.opts$relative.to)){
    plot.opts$ci <- rep(FALSE, length(x))
    base.i <- which(names(x) == plot.opts$relative.to)
    for(i in 1:length(x)){
      for(j in 1:length(nms)){
        if(x[[base.i]]$is.wham) {
          denom <- cbind(exp(x[[base.i]][[nms[j]]]$est[,]))
          denom <- denom[,NCOL(denom)]
        } else denom <- exp(x[[base.i]][[nms[j]]][,1])
        if(x[[i]]$is.wham) {
          num <- cbind(exp(x[[i]][[nms[j]]]$est))
          num <- num[,NCOL(num)]
        } else num <- exp(x[[i]][[nms[j]]][,1])
        df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=nms.df[j], #changed later
                                   val=num/denom, lo=num/denom, hi=num/denom, Model=names(x)[i]))
      }
    }
  } else {
    for(i in 1:length(x)){
      for(j in 1:length(nms)){
        if(x[[i]]$is.wham) {
          val <- cbind(exp(x[[i]][[nms[j]]]$est))
          col <- NCOL(val)
          val <- val[,col]
          lo <- cbind(x[[i]][[nms[j]]]$ci$lo)[,col]
          hi <- cbind(x[[i]][[nms[j]]]$ci$hi)[,col]
        }
        else {
          val <- exp(x[[i]][[nms[j]]][,1])
          ci <- get.ci(x[[i]][[nms[j]]], plot.opts$alpha, plot.opts$ci[i], asap = TRUE)
          lo <- ci$lo
          hi <- ci$hi
        }
        df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=nms.df[j], val=val, lo=lo, hi=hi, Model=names(x)[i]))
      }
    }
  }
  if(plot.opts$refpt == "XSPR"){
    pSPR <- sapply(x, function(y) y$percentSPR)
    if(length(unique(pSPR)) != 1) stop("FXSPR does not make sense to compare because percent SPR is not equal for all models.")
    pSPR <- as.character(pSPR[1])
  }
  if(!is.null(plot.opts$relative.to)) {
    if(plot.opts$refpt == "XSPR"){
      df$var <- factor(df$var, levels=nms.df, #c("F_refpt","SSB_refpt","Y_refpt"),
                       labels=c(bquote(italic(F)[paste(.(pSPR), "%")] ~relative~to~.(plot.opts$relative.to)),
                                bquote(paste('SSB (', italic(F)[paste(.(pSPR), "%")],')')~relative~to~.(plot.opts$relative.to)),
                                bquote(paste('Yield (',italic(F)[paste(.(pSPR), "%")], ')')~relative~to~.(plot.opts$relative.to))))
    } else {
      df$var <- factor(df$var, levels=nms.df,#c("F_refpt","SSB_refpt","Y_refpt"),
                       labels=c(bquote(italic(F)[MSY] ~relative~to~.(plot.opts$relative.to)),
                                bquote(SSB[MSY]~relative~to~.(plot.opts$relative.to)),
                                bquote(MSY~relative~to~.(plot.opts$relative.to))))      
    }
  } else {
    if(plot.opts$refpt == "XSPR"){
      df$var <- factor(df$var, levels=nms.df, #c("F_refpt","SSB_refpt","Y_refpt"),
                       labels=c(bquote(italic(F)[paste(.(pSPR), "%")]),
                                bquote(paste('SSB (', italic(F)[paste(.(pSPR), "%")],')')),
                                bquote(paste('Yield (',italic(F)[paste(.(pSPR), "%")], ')'))))
    } else {
      df$var <- factor(df$var, levels=nms.df, #c("F_refpt","SSB_refpt","Y_refpt"),
                       labels=c(bquote(italic(F)[MSY]),
                                bquote(SSB[MSY]),
                                bquote(MSY)))      
    }
  }
  g <- plot.timeseries.compare(df, x, plot.opts)
  g <- g + ggplot2::facet_wrap(ggplot2::vars(var), scales="free_y", ncol=1, strip.position = "left", labeller = ggplot2::label_parsed)
  return(g)
}

#' @title Helper function: plot.rel.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.rel.compare #####
plot.rel.compare <- function(x, plot.opts){
  df <- data.frame(matrix(NA, nrow=0, ncol=6))
  colnames(df) <- c("Year","var","val","lo","hi","Model")
  if(!is.null(plot.opts$relative.to)){
    plot.opts$ci <- rep(FALSE, length(x))
    base.i <- which(names(x) == plot.opts$relative.to)
    vars <- c("relSSB", "relF")
    if(plot.opts$refpt == "XSPR"){
      for(i in 1:length(x)){
        for(j in 1:length(vars)){
          vals <- t(sapply(x[[i]][["log_rel_ssb_F_cov"]], function(x) c(x[[1]][j], sqrt(x[[2]][j,j]))))
          vals2 <- t(sapply(x[[base.i]][["log_rel_ssb_F_cov"]], function(x) c(x[[1]][j], sqrt(x[[2]][j,j]))))
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=vars[j],
                                     val=exp(vals[,1])/exp(vals2[,1]), lo=NA, hi=NA, Model=names(x)[i]))
        }
      }
    } else { # msy ref pt
      for(i in 1:length(x)){
        for(j in 1:length(vars)){
          vals <- t(sapply(x[[i]][["log_rel_ssb_F_cov_msy"]], function(x) c(x[[1]][j], sqrt(x[[2]][j,j]))))
          vals2 <- t(sapply(x[[base.i]][["log_rel_ssb_F_cov_msy"]], function(x) c(x[[1]][j], sqrt(x[[2]][j,j]))))
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=vars[j],
                                     val=exp(vals[,1])/exp(vals2[,1]), lo=NA, hi=NA, Model=names(x)[i]))
        }
      }      
    }
    df$lo = df$val
    df$hi = df$val
  } else {
    vars <- c("relSSB", "relF")
    if(plot.opts$refpt == "XSPR"){
      for(i in 1:length(x)){
        for(j in 1:length(vars)){
          vals <- t(sapply(x[[i]][["log_rel_ssb_F_cov"]], function(x) c(x[[1]][j], sqrt(x[[2]][j,j]))))
          ci <- exp(vals[,1] +qnorm(1-plot.opts$alpha/2) * cbind(-vals[,2],vals[,2]))
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=vars[j],
                                     val=exp(vals[,1]), lo=ci[,1], hi=ci[,2], Model=names(x)[i]))
        }
      }
    } else { # msy ref pt
      for(i in 1:length(x)){
        for(j in 1:length(vars)){
          vals <- t(sapply(x[[i]][["log_rel_ssb_F_cov_msy"]], function(x) c(x[[1]][j], sqrt(x[[2]][j,j]))))
          ci <- exp(vals[,1] +qnorm(1-plot.opts$alpha/2) * cbind(-vals[,2],vals[,2]))
          df <- rbind(df, data.frame(Year=x[[i]]$years_full, var=vars[j],
                                     val=exp(vals[,1]), lo=ci[,1], hi=ci[,2], Model=names(x)[i]))
        }
      }
    }
  }
  if(plot.opts$refpt == "XSPR"){
    pSPR <- sapply(x, function(y) y$percentSPR)
    if(length(unique(pSPR)) != 1) stop("Percent SPR is not equal for all models.")
    pSPR <- as.character(pSPR[1])
  }
  if(!is.null(plot.opts$relative.to)) {
    if(plot.opts$refpt == "XSPR"){
      df$var <- factor(df$var, levels=c("relF","relSSB"),
                       labels=c(bquote(italic(F) / italic(F)[paste(.(pSPR), "%")]~relative~to~.(plot.opts$relative.to)),
                                bquote(SSB / SSB[paste(.(pSPR), "%")]~relative~to~.(plot.opts$relative.to))))
    } else { # msy ref pt
      df$var <- factor(df$var, levels=c("relF","relSSB"),
                       labels=c(bquote(italic(F) / italic(F)[MSY]~relative~to~.(plot.opts$relative.to)),
                                bquote(SSB / SSB[MSY]~relative~to~.(plot.opts$relative.to))))
    }
  } else {
    if(plot.opts$refpt == "XSPR"){
      df$var <- factor(df$var, levels=c("relF","relSSB"),
                       labels=c(bquote(italic(F) / italic(F)[paste(.(pSPR), "%")]),
                                bquote(SSB / SSB[paste(.(pSPR), "%")])))
    } else { # msy ref pt
      df$var <- factor(df$var, levels=c("relF","relSSB"),
                       labels=c(bquote(italic(F) / italic(F)[MSY]),
                                bquote(SSB / SSB[MSY])))
    }
  }
  g <- plot.timeseries.compare(df, x, plot.opts)
  g <- g + ggplot2::facet_wrap(ggplot2::vars(var), scales="free_y", ncol=1, strip.position = "left", labeller = ggplot2::label_parsed)
  if(is.null(plot.opts$relative.to)){
    g <- g + ggplot2::geom_hline(yintercept = 1, linetype=2, size=.4) +
      ggplot2::geom_hline(data = df %>% dplyr::filter(var == levels(df$var)[2]) %>% dplyr::mutate(half=0.5), mapping=ggplot2::aes(yintercept = half), color="red", linetype=2, size=.4)
  }
  return(g)
}

#' @title Helper function: plot.kobe.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

plot.kobe.compare <- function(x, plot.opts){
  status.year.ind <- sapply(x, function(x) which(x$years_full == plot.opts$kobe.yr))
  #if any of the models have non-finite values for cov, then don't do that status years
  do.kobe <- sapply(status.year.ind, function(y) !any(sapply(x, function(z) any(!is.finite(z$log_rel_ssb_F_cov[[y]][[2]])))))
  #do.kobe <- unlist(sapply(mapply(function(x,i) x$log_rel_ssb_F_cov[i], x, status.year.ind), function(y) !all(!is.finite(y)))) # only if some non-infinite values for at least some status years
  if(any(do.kobe)){ #length(do.kobe) == length(x)
    #fxn <- function(y,i) y[["log_F"]][i,1]-y[["log_FXSPR"]][i,1]
    fxn <- function(y,i,j) y$log_rel_ssb_F_cov[[i]][[1]][j] #i: yr, j: rel ssb or f 
    rel.f.vals <- mapply(fxn, x, status.year.ind, 2)
    rel.ssb.vals <- mapply(fxn, x, status.year.ind, 1)
    fxn <- function(y,i) y$log_rel_ssb_F_cov[[i]][[2]] #i: yr
    log.rel.ssb.rel.F.cov <- mapply(fxn, x, status.year.ind, SIMPLIFY=F)

    log.rel.ssb.rel.F.ci.regs <- lapply(1:length(x), function(x){
      if(is.na(rel.f.vals[x]) | any(diag(log.rel.ssb.rel.F.cov[[x]])<0)) return(matrix(NA,100,2))
      else return(exp(ellipse::ellipse(log.rel.ssb.rel.F.cov[[x]], centre = c(rel.ssb.vals[x],rel.f.vals[x]), level = 1-plot.opts$alpha)))
      })
    p.ssb.lo.f.lo <- p.ssb.lo.f.hi <- p.ssb.hi.f.lo <- p.ssb.hi.f.hi <- rep(NA,length(status.year.ind))
    for(i in 1:length(status.year.ind)){
      check.zero.sd <- diag(log.rel.ssb.rel.F.cov[[i]])==0 | diag(log.rel.ssb.rel.F.cov[[i]]) < 0
      if(!any(is.na(check.zero.sd))) if(!any(check.zero.sd)){
        p.ssb.lo.f.lo[i] <- mnormt::sadmvn(lower = c(-Inf,-Inf), upper = c(-log(2), 0), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
        p.ssb.lo.f.hi[i] <- mnormt::sadmvn(lower = c(-Inf,0), upper = c(-log(2), Inf), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
        p.ssb.hi.f.lo[i] <- mnormt::sadmvn(lower = c(-log(2),-Inf), upper = c(Inf, 0), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
        p.ssb.hi.f.hi[i] <- mnormt::sadmvn(lower = c(-log(2),0), upper = c(Inf, Inf), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
      }
    }

    vals <- exp(cbind(rel.ssb.vals, rel.f.vals))
    max.x <- max(sapply(log.rel.ssb.rel.F.ci.regs, function(x) max(x[,1],na.rm = TRUE)),1.25, vals[,1])
    max.y <- max(sapply(log.rel.ssb.rel.F.ci.regs, function(x) max(x[,2],na.rm = TRUE)),1.25, vals[,2])

    plot(vals[,1],vals[,2], ylim = c(0,1.05*max.y), xlim = c(0,1.05*max.x), xlab = bquote(paste("SSB / ", SSB[paste(.(x[[1]]$percentSPR),"%")])),
      ylab = bquote(paste(italic(F)," / ", italic(F)[paste(.(x[[1]]$percentSPR),"%")])),type = 'n')
    lims = par("usr")
    tcol <- col2rgb('red')
    tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
    polygon(c(lims[1],0.5,0.5,lims[1]),c(1,1,lims[4],lims[4]), border = tcol, col = tcol)
    tcol <- col2rgb('green')
    tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
    polygon(c(0.5,lims[2],lims[2],0.5),c(lims[3],lims[3],1,1), border = tcol, col = tcol)
    tcol <- col2rgb('yellow')
    tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
    polygon(c(lims[1],0.5,0.5,lims[1]),c(lims[3],lims[3],1,1), border = tcol, col = tcol)
    polygon(c(0.5,lims[2],lims[2],0.5),c(1,1,lims[4],lims[4]), border = tcol, col = tcol)
    if(plot.opts$kobe.prob){
      legend("topleft", legend = paste0(names(x), ": Prob = ", round(p.ssb.lo.f.hi,2)), bty = "n", cex=0.7)
      legend("topright", legend = paste0(names(x), ": Prob = ", round(p.ssb.hi.f.hi,2)), bty = "n", cex=0.7)
      legend("bottomleft", legend = paste0(names(x), ": Prob = ", round(p.ssb.lo.f.lo,2)), bty = "n", cex=0.7)
      legend("bottomright", legend = paste0(names(x), ": Prob = ", round(p.ssb.hi.f.lo,2)), bty = "n", cex=0.7)
    }
    text(vals[,1],vals[,2], paste0(rownames(vals)," (",plot.opts$kobe.yr,")"), cex=0.7)
    for(i in 1:length(status.year.ind)) polygon(log.rel.ssb.rel.F.ci.regs[[i]][,1], log.rel.ssb.rel.F.ci.regs[[i]][,2], lty=i)#, border = gray(0.7))
    return(list(rel.status = vals, p.ssb.lo.f.lo = p.ssb.lo.f.lo, p.ssb.hi.f.lo = p.ssb.hi.f.lo, p.ssb.hi.f.hi = p.ssb.hi.f.hi, p.ssb.lo.f.hi = p.ssb.lo.f.hi))
  } else {
    return(NULL)
  }
}

#' @title Helper function: plot.kobe.compare
#' @description Helper function pulled from multi-wham compare_wham_models.R with some modifications to bridge from single- to multi-wham
#' @export

## plot.kobe.compare #####
plot.kobe.compare <- function(x, plot.opts){
  status.year.ind <- sapply(x, function(x) which(x$years_full == plot.opts$kobe.yr))
  #if any of the models have non-finite values for cov, then don't do that status years
  do.kobe <- sapply(status.year.ind, function(y) !any(sapply(x, function(z) any(!is.finite(z$log_rel_ssb_F_cov[[y]][[2]])))))
  #do.kobe <- unlist(sapply(mapply(function(x,i) x$log_rel_ssb_F_cov[i], x, status.year.ind), function(y) !all(!is.finite(y)))) # only if some non-infinite values for at least some status years
  if(any(do.kobe)){ #length(do.kobe) == length(x)
    #fxn <- function(y,i) y[["log_F"]][i,1]-y[["log_FXSPR"]][i,1]
    fxn <- function(y,i,j) y$log_rel_ssb_F_cov[[i]][[1]][j] #i: yr, j: rel ssb or f 
    rel.f.vals <- mapply(fxn, x, status.year.ind, 2)
    rel.ssb.vals <- mapply(fxn, x, status.year.ind, 1)
    fxn <- function(y,i) y$log_rel_ssb_F_cov[[i]][[2]] #i: yr
    log.rel.ssb.rel.F.cov <- mapply(fxn, x, status.year.ind, SIMPLIFY=F)
    
    log.rel.ssb.rel.F.ci.regs <- lapply(1:length(x), function(x){
      if(is.na(rel.f.vals[x]) | any(diag(log.rel.ssb.rel.F.cov[[x]])<0)) return(matrix(NA,100,2))
      else return(exp(ellipse::ellipse(log.rel.ssb.rel.F.cov[[x]], centre = c(rel.ssb.vals[x],rel.f.vals[x]), level = 1-plot.opts$alpha)))
    })
    p.ssb.lo.f.lo <- p.ssb.lo.f.hi <- p.ssb.hi.f.lo <- p.ssb.hi.f.hi <- rep(NA,length(status.year.ind))
    for(i in 1:length(status.year.ind)){
      check.zero.sd <- diag(log.rel.ssb.rel.F.cov[[i]])==0 | diag(log.rel.ssb.rel.F.cov[[i]]) < 0
      if(!any(is.na(check.zero.sd))) if(!any(check.zero.sd)){
        p.ssb.lo.f.lo[i] <- mnormt::sadmvn(lower = c(-Inf,-Inf), upper = c(-log(2), 0), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
        p.ssb.lo.f.hi[i] <- mnormt::sadmvn(lower = c(-Inf,0), upper = c(-log(2), Inf), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
        p.ssb.hi.f.lo[i] <- mnormt::sadmvn(lower = c(-log(2),-Inf), upper = c(Inf, 0), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
        p.ssb.hi.f.hi[i] <- mnormt::sadmvn(lower = c(-log(2),0), upper = c(Inf, Inf), mean = c(rel.ssb.vals[i],rel.f.vals[i]), varcov = log.rel.ssb.rel.F.cov[[i]])
      }
    }
    
    vals <- exp(cbind(rel.ssb.vals, rel.f.vals))
    max.x <- max(sapply(log.rel.ssb.rel.F.ci.regs, function(x) max(x[,1],na.rm = TRUE)),1.25, vals[,1])
    max.y <- max(sapply(log.rel.ssb.rel.F.ci.regs, function(x) max(x[,2],na.rm = TRUE)),1.25, vals[,2])
    
    plot(vals[,1],vals[,2], ylim = c(0,1.05*max.y), xlim = c(0,1.05*max.x), xlab = bquote(paste("SSB / ", SSB[paste(.(x[[1]]$percentSPR),"%")])),
         ylab = bquote(paste(italic(F)," / ", italic(F)[paste(.(x[[1]]$percentSPR),"%")])),type = 'n')
    lims = par("usr")
    tcol <- col2rgb('red')
    tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
    polygon(c(lims[1],0.5,0.5,lims[1]),c(1,1,lims[4],lims[4]), border = tcol, col = tcol)
    tcol <- col2rgb('green')
    tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
    polygon(c(0.5,lims[2],lims[2],0.5),c(lims[3],lims[3],1,1), border = tcol, col = tcol)
    tcol <- col2rgb('yellow')
    tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
    polygon(c(lims[1],0.5,0.5,lims[1]),c(lims[3],lims[3],1,1), border = tcol, col = tcol)
    polygon(c(0.5,lims[2],lims[2],0.5),c(1,1,lims[4],lims[4]), border = tcol, col = tcol)
    if(plot.opts$kobe.prob){
      legend("topleft", legend = paste0(names(x), ": Prob = ", round(p.ssb.lo.f.hi,2)), bty = "n", cex=0.7)
      legend("topright", legend = paste0(names(x), ": Prob = ", round(p.ssb.hi.f.hi,2)), bty = "n", cex=0.7)
      legend("bottomleft", legend = paste0(names(x), ": Prob = ", round(p.ssb.lo.f.lo,2)), bty = "n", cex=0.7)
      legend("bottomright", legend = paste0(names(x), ": Prob = ", round(p.ssb.hi.f.lo,2)), bty = "n", cex=0.7)
    }
    text(vals[,1],vals[,2], paste0(rownames(vals)," (",plot.opts$kobe.yr,")"), cex=0.7)
    for(i in 1:length(status.year.ind)) polygon(log.rel.ssb.rel.F.ci.regs[[i]][,1], log.rel.ssb.rel.F.ci.regs[[i]][,2], lty=i)#, border = gray(0.7))
    return(list(rel.status = vals, p.ssb.lo.f.lo = p.ssb.lo.f.lo, p.ssb.hi.f.lo = p.ssb.hi.f.lo, p.ssb.hi.f.hi = p.ssb.hi.f.hi, p.ssb.lo.f.hi = p.ssb.lo.f.hi))
  } else {
    return(NULL)
  }
}




