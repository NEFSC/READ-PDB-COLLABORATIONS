#' @title Conduct the P* projections for the MAFMC
#' 
#' @description Conducts the P* projections, which incorporates uncertainty and risk in ABC calculation from OFL using a fitted WHAM model 
#' @source from C Adams for butterfish 7-5-2022
#' @expanded by E Liljestrand 7-19-2024
#' 
#' @param mod An rds from a WHAM fit
#' @param SSBmsy the SSBmsy proxy used to calculate ratio between biomass and reference point
#' @param catch.year1 the amount of catch in the assessment year (usually the ABC)
#' @param proj.yr how many years to project into the future
#' @param CV the level of uncertainty
#' @param avg.abc average ABC, if using, specify, otherwise don't specify
#' 
#' @return a model object with the years of projected values based on P* catch advice 
#' 
#' @example
#' mod <- readRDS("Modeling/Run3/fit.RDS")
#' SSBmsy <- 11225        
#' catch.year1 <- 7557
#' CV <- 0.6
#' projyr <- 2
#' pstar_model <- pstar(mod,SSBmsy,catch.year1,projyr,CV)

pstarmodel <- function(mod=NULL,SSBmsy=NULL,catch.year1=NULL,projyr=3,CV=1.5,avg.abc=NULL)
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
  
  # Catch in the first year of projections is specified
  catch.proj[1] <- catch.year1
  
  if(projyr>1)
  {  for(i in 1:(projyr-1))
    {
      # Tell WHAM to do projection based on catch in first i years ('5') and Fmsy proxy ('3') in following years
      proj_F_opt <- c(rep(5,i),rep(3,projyr-i))
      mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr,proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)
      
      ofl <- tail(apply(mod.proj$rep$pred_catch,1,sum),projyr)[i+1]
      ssb <- tail(apply(mod.proj$rep$SSB,1,sum),projyr)[i]
      ssbratio <- ssb/SSBmsy
      
      catch <- ABC(ofl,ssbratio,CV)
      
      if(i<projyr)
      {
        catch.proj[i+1] <- catch
        if(!is.null(avg.abc)) catch.proj[i+1] <- avg.abc
      }
    }
  }
  
  # Conduct one last projection to get F and SSB resultant from final ABC
  proj_F_opt <- c(rep(5,projyr))
  mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr,proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)

  return(mod.proj)

}