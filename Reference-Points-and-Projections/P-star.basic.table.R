#' @title Conduct the P* projections for the MAFMC
#' 
#' @description Conducts the P* projections, which incorporates uncertainty and risk in ABC calculation from OFL using a fitted WHAM model 
#'
#' @param mod An rds from a WHAM fit
#' @param SSBmsy the SSBmsy proxy used to calculate ratio between biomass and reference point
#' @param catch.year1 the amount of catch in the assessment year (usually the ABC)
#' @param proj.yr how many years to project into the future
#' @param CV the level of uncertainty
#' @param avg.abc average ABC, if using, specify, otherwise don't specify
#' 
#' 
#' @return A table containing:
#' \itemize{
#'   \item{Year - projection Year}
#'   \item{OFL - overfishing limit}
#'   \item{ABC - allowable biological catch}
#'   \item{SSB/SSBMSY - ratio between SSB in that year and reference point}
#'   \item{F - fishing mortality}
#'   \item{SSB - spawning stock biomass}
#'   \item{P* - Pstar value}
#' }
#' 
#' @example
#' mod <- readRDS("Modeling/Run3/fit.RDS")
#' SSBmsy <- 11225        
#' catch.year1 <- 7557
#' CV <- 60 
#' pstar60 <- pstar(mod,SSBmsy,catch.year1,CV=CV)
#' write.csv(pstar60, "Projections/Pstar60.csv", row.names = FALSE)


pstartable <- function(mod=NULL,SSBmsy=NULL,catch.year1=NULL,projyr=3,CV=1.5,avg.abc=NULL)
{
  # Have to source additional functions:
  source("abccalc.R")
  source("invabccalc.R")
  source("pstarcalc.R")

  # Specify the model and projection years
  model.years <- mod$years
  proj.years <- c((tail(model.years,1)+1):(tail(model.years,1)+projyr))
  
  # Empty variables to collect catch and pstar values
  catch.proj <- rep(0,projyr)
  ratio.proj <- rep(0,projyr-1)
  pstar.proj <- rep(0,projyr-1)
  ofl.proj <- rep(0,projyr-1)
  
  # Catch in the first year of projections is specified
  catch.proj[1] <- catch.year1
  
  for(i in 1:(projyr-1))
  {
    # Tell WHAM to do projection based on catch in first i years ('5') and Fmsy proxy ('3') in following years
    proj_F_opt <- c(rep(5,i),rep(3,projyr-i))
    mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr,proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)
    
    ofl <- tail(apply(mod.proj$rep$pred_catch,1,sum),projyr)[i+1]
    ssb <- tail(apply(mod.proj$rep$SSB,1,sum),projyr)[i]
    ssbratio <- ssb/SSBmsy
    catch <- ABC(ofl,ssbratio,CV)
    
    ratio.proj[i] <- ssbratio
    pstar.proj[i] <- inv_ABC(catch, ofl, CV)
    ofl.proj[i] <- ofl
    
    if(i<projyr)
    {
      catch.proj[i+1] <- catch
      if(!is.null(avg.abc)) catch.proj[i+1] <- avg.abc
    }
  }

  # Create table for memo
  pstartable <- as.data.frame(cbind(ofl.proj[-projyr]))
  pstartable <- cbind(proj.years[-1], pstartable)
  colnames(pstartable) <- c("Year", "OFL")
  pstartable <- cbind(pstartable, rbind(catch.proj[-1]))
  colnames(pstartable)[3] <- "ABC"
  
  # B/BMSY
  bratio <- c(ratio.year2,ratio.year3)
  ratio.proj[-projyr]
  
  # Conduct one last projection to get F and SSB resultant from final ABC
  proj_F_opt <- c(rep(5,projyr))
  mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr,proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)
  # F
  f.proj <- tail(exp(mod.proj$rep$log_F_tot),(projyr-1))
  # SSB
  ssb.proj <- tail(apply(mod.proj$rep$SSB,1,sum),2)
  
  pstartable <- cbind(pstartable, ratio.proj[-projyr], f.proj, ssb.proj)
  colnames(temp)[4:6] <- c("B/BMSY","F", "SSB")
  pstartable <- cbind(pstartable, pstar.proj)
  colnames(temp)[7] <- "P*"
  
  return(pstartable)

}
