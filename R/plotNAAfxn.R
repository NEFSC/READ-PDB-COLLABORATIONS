#' @title Plot NAA devs from WHAM fit 
#' 
#' @description Plot NAA devs from WHAM fit 
#' 
#' @param mods An rds from a WHAM fit with NAA random effects
#' @param cor NULL - a placeholder for future labeling
#' 
#' @return A ggplot. Must use ggsave after running the function to save the plot
#'
#' @examples 
#' naaplot=plotNAAfxn(mods=mod,cor="NA")
#' ggsave(paste("YOURNAME","_NAAdevs.jpeg"),naaplot,path=YOURPATH)
#' @export

plotNAAfxn=function(mods=NULL,cor=NULL){
#   library(tidyverse)
  df <- as.data.frame(mods$rep$NAA_devs)
  df$Year <- mods$input$years_full[2:length(mods$input$years_full)] #no devs in year 1
  colnames(df) <- c(paste0("Age_",1:(mods$input$data$n_ages)),"Year")
  df$Model <- mods$model_name
  df$NAA_mod = mods$model_name
  df$NAA_cor = cor
  #df.NAA <- rbind(df.NAA, df)
  
  df.new <- df %>% tidyr::pivot_longer(-c(Year,Model,NAA_mod,NAA_cor),
                                       names_to = "Age",
                                       names_prefix = "Age_",
                                       names_transform = list(Age = as.integer),
                                       values_to = "NAA_Dev")
  #df.new$Age <- as.integer(df$Age)
  #df.new$NAA_cor <- factor(df$NAA_cor, levels=c("IID","AR1_year","AR1_age","2D_AR1"))
  #df.new$NAA_mod <- factor(df$NAA_mod, levels=c("Model 8","Model 11","Model 12","Model 13"))
  naadevplot=ggplot(df.new, aes(x=Year, y=Age)) +
    geom_tile(aes(fill=NAA_Dev)) +
    #facet_wrap(~ NAA_cor) +
    scale_y_continuous(breaks=seq(1,10,by=1)) +
    #scale_fill_viridis_c() +
    #coord_equal() + 
    scale_fill_gradient2(low="royalblue", mid = "white", high="red", midpoint = 0) +
    theme_bw() +
    theme(axis.text=element_text(size=12,face="bold"),
          axis.title=element_text(size=15,face="bold"),
          plot.title=element_text(size=15,face="bold"),
          strip.text = element_text(size=15,face="bold"))
  
  return(naadevplot)
}

