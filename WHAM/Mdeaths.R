#' @title Calculate the amount of fish dying given M 
#' 
#' @description Calculates the amount (metric tons) of fish dying given the assumed or estimated M; uses Jan-1 biomass, so an approximation
#' 
#' @param mod An rds from a WHAM fit
#' 
#' @return A dataframe with two columns. Year and Deaths
#'
#' @example
#' mod.dir="m168"
#' write.dir <- paste("C:/Herring/2022 Assessment/Assessments/WHAM",mod.dir,sep="/")
#' setwd(write.dir)
#' mod <- readRDS(paste0(mod.dir,".rds"))
#' m168deaths=Mdeaths(mod=mod)
#' plot(m168deaths$Year,m168deaths$Deaths,type='o',ylab="Dead Fish (000's mt)",xlab="")

Mdeaths=function(mod=NULL){
  MAA=mod$rep$MAA
  ZAA=mod$rep$ZAA
  NAA=mod$rep$NAA
  WAA.pt=mod$input$data$waa_pointer_jan1
  WAA=mod$input$data$waa[WAA.pt,,]
  
  
  cons.aa<-((MAA/ZAA)*(1-exp(-ZAA))*NAA*WAA)/1000
  cons.annual=data.frame(Year=mod$input$years,Deaths=rowSums(cons.aa))
  return(cons.annual)
}


