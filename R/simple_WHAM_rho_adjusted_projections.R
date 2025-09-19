#' @title Simple WHAM rho-adjusted projections
#' 
#' @description
#' Function to conduct WHAM projections applying a single Mohn's rho adjustment to all ages. Built for multi-WHAM, but not all features supported. Should first run with rho=0 to see how close an approximation the function provides.
#' 
#' @param modproj A model result from WHAM that includes projections.
#' @param rho A single Mohn's rho value that will be applied to all ages at the start of projections.
#' 
#' @return A table of SSB and catch for every projection year showing the original values and rho-adjusted values.
#' 
#' @examples
#' modproj <- readRDS("test.RDS")
#' mytest <- rhoadjustWHAMproj(modproj, rho=0) # make sure get same projected SSB and catch as WHAM did, if not then stop here
#' mytable15 <- rhoadjustWHAMproj(modproj, rho=0.15) # adjust first year of projections using an input rho value
#' mytablerhoSSB <- rhoadjustWHAMproj(modproj, rho=NULL) # use rho for SSB - assumes retro has been run and saved in modproj object


rhoadjustWHAMproj <- function(modproj, rho){
  
  library(dplyr)
  library(wham)
  
  # check for projections in modproj object
  if (length(modproj$years_full) == length(modproj$years)) return("Must have projections in modproj object.")
  
  # check for unsupported features from multi WHAM
  if (dim(modproj$rep$NAA)[1] > 1) return("This function does not support multiple stocks.")
  if (dim(modproj$rep$NAA)[2] > 1) return("This function does not support multiple regions.")
  
  # uses some helper functions
  # function for the difference between a calculated and desired catch
  mydiff <- function(Fmult, natage, selx, waa, M, targetcatch){
    FAA <- Fmult * selx
    ZAA <- FAA + M
    thiscatch <- sum(natage * waa * FAA * (1 - exp(-ZAA)) / ZAA)
    diff <- thiscatch - targetcatch
    return(diff)
  }
  
  # solve for the F needed to achieve a desired catch
  solve_for_F <- function(natage, selx, waa, M, targetcatch){
    # check to make sure F=2 produces catch > targetcatch
    mycheck <- mydiff(2, natage, selx, waa, M, targetcatch)
    if (mycheck < 0){
      stop("Cannot achieve catch with F=2. Try lower projcatch.")
    }
    
    a <-  0
    b <-  2
    tol <- 1e-6
    max_iter <- 100
    
    iter <- 0
    while (abs(b-a) > tol && iter < max_iter) {
      c <- (a + b) / 2
      if (mydiff(c, natage, selx, waa, M, targetcatch) == 0){ # found exact Fmult
        return(c)
      } else if (mydiff(a, natage, selx, waa, M, targetcatch) * mydiff(c, natage, selx, waa, M, targetcatch) < 0){
        b <- c
      } else {
        a <- c
      }
      iter <- iter + 1
    }
    return((a+b)/2) # return midpoint of final interval
  } 
  
  # calculate projections at age applying a rho adjustment (can be zero) to numbers at age in first year
  # can either apply an F or catch in all projection years (can't mix and match)
  calc_proj <- function(NAAstart, nprojyears, projrecruits, rho, selx, waacatch, waassb, mature, fracspawn, MAA, Fflag, Fmult, projcatch){
    
    nages <- length(NAAstart)
    FAA <- matrix(NA, nrow = nprojyears, ncol = nages)
    ZAA <- matrix(NA, nrow = nprojyears, ncol = nages)
    ssbaa <- matrix(NA, nrow = nprojyears, ncol = nages)
    catchaa <- matrix(NA, nrow = nprojyears, ncol = nages)
    NAA <- matrix(NA, nrow = nprojyears, ncol = nages)
    
    rho_mult <- 1/(1+rho)
    NAA[1, ] <- NAAstart * rho_mult
    NAA[2:nprojyears, 1] <- projrecruits
    
    
    for (iyear in 1:nprojyears){
      if (Fflag[iyear] == 1){  # use Fmult in this projection year
        FAA[iyear, ] <- Fmult[iyear] * selx
      } else if (Fflag[iyear] == 0){  # use projcatch in this projection year
        Fmultsolve <- solve_for_F(NAA[1, ], selx, waacatch, MAA[1,], projcatch[iyear])
        FAA[iyear, ] <- Fmultsolve * selx
      }
      ZAA[iyear, ] <- FAA[iyear, ] + MAA[iyear, ]
      if (iyear > 1){
        for (iage in 2:nages){
          NAA[iyear, iage] <- NAA[iyear-1, iage-1] * exp(-ZAA[iyear-1, iage-1])
        }
        NAA[iyear, nages] <- NAA[iyear, nages] + NAA[iyear-1, nages] * exp(-ZAA[iyear-1, nages])
      }
      ssbaa[iyear, ] <- NAA[iyear, ] * exp(-ZAA[iyear, ] * fracspawn[iyear]) * waassb * mature
      catchaa[iyear, ] <- NAA[iyear, ] * FAA[iyear, ] * (1 - exp(-ZAA[iyear, ])) * waacatch / ZAA[iyear, ]
    }
    ssbts <- apply(ssbaa, 1, sum)
    catchts <- apply(catchaa, 1, sum)
    res = list(ssbaa=ssbaa, catchaa=catchaa, ssbts=ssbts, catchts=catchts, NAA=NAA)
    return(res)
  }
  
  # function to extract needed pieces from WHAM object
  # note final three vectors in list assume proj_F_opt used to define whether catch or F projected in a given year
  get_proj_vals <- function(modproj){
    my <- list()
    my$NAAstart <- modproj$rep$NAA[1,1,length(modproj$years)+1, ]
    my$nprojyears <- length(modproj$years_full) - length(modproj$years)
    my$projrecruits <- modproj$rep$NAA[1,1,(length(modproj$years)+2):length(modproj$years_full),1]
    my$selx <- modproj$rep$FAA_static / max(modproj$rep$FAA_static)
    my$waacatch <- modproj$rep$waa_catch_static
    my$waassb <- modproj$rep$waa_ssb_static
    my$mature <- modproj$rep$mature_static
    my$fracspawn <- modproj$rep$fracyr_SSB_all[(length(modproj$years)+1):length(modproj$years_full)]
    my$MAA <- modproj$rep$MAA[1,1,(length(modproj$years)+1):length(modproj$years_full),]
    my$Fflag <- rep(NA, my$nprojyears)
    my$Fmult <- rep(NA, my$nprojyears)
    my$projcatch <- rep(NA, my$nprojyears)
    for (iyear in 1:my$nprojyears){
      if (modproj$input$options$proj$proj_F_opt[iyear] == 5){ # project catch
        my$Fflag[iyear] <- 0
        my$Fmult[iyear] <- 0
        if (!is.null(modproj$input$options$proj$proj.catch)){
          my$projcatch[iyear] <- modproj$input$options$proj$proj.catch[iyear]
        } else if (!is.null(modproj$input$options$pro$proj.Fcatch)){
          my$projcatch[iyear] <- modproj$input$options$pro$proj.Fcatch[iyear]
        }
      } else if (modproj$input$options$proj$proj_F_opt[iyear] %in% c(1, 2, 3, 4, 6)){
        my$Fflag[iyear] <- 1
        my$Fmult[iyear] <- modproj$rep$Fbar[(length(modproj$years)+iyear), 1]
        my$projcatch[iyear] <- 0
      } else {
        return(paste("proj_F_opt = ", modproj$input$options$proj$proj_F_opt[iyear], " not supported currently."))
      }
    }
    return(my)
  }
  
  # run projections using calc_proj function and compare to values in modproj object
  my <- get_proj_vals(modproj)
  my$rho <- ifelse(is.null(rho), mohns_rho(modproj)$SSB, rho)
  myres <- calc_proj(my$NAAstart, my$nprojyears, my$projrecruits, my$rho, my$selx, my$waacatch, my$waassb, my$mature, my$fracspawn, my$MAA, my$Fflag, my$Fmult, my$projcatch)
  
  finaltable <- tibble(Year = modproj$years_full[(length(modproj$years)+1):length(modproj$years_full)],
                       SSB_orig = modproj$rep$SSB[(length(modproj$years)+1):length(modproj$years_full)],
                       SSB_rho_adjusted = myres$ssbts,
                       catch_orig = modproj$rep$pred_catch[(length(modproj$years)+1):length(modproj$years_full)[1]],
                       catch_rho_adjusted = myres$catchts)
  return(finaltable)
}

