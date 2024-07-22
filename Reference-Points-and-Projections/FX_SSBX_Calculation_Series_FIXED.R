# Pull Out FX% from an average of Y years from any WHAM output

# load wham
library(wham)
library(dplyr)
library(tinytex)
library(kableExtra)
library(readxl)

#####################################################################################
# These are Tim's Functions from the WHAM Code that are used in the new Function: (DON'T CHANGE)
#####################################################################################

get_SPR = function(F, M, sel, mat, waassb, fracyrssb, at.age = FALSE)
{
  n_ages = length(sel)
  SPR = numeric()
  n = 1
  F = F * sel
  Z = F + M
  for(a in 1:(n_ages-1))
  {
    SPR[a] = n[a] * mat[a] * waassb[a] * exp(-fracyrssb * Z[a])
    n[a+1] = n[a] * exp(-Z[a])
  }
  n[n_ages] = n[n_ages]/(1-exp(-Z[n_ages]))
  SPR[n_ages] = n[n_ages] * mat[n_ages] * waassb[n_ages] * exp(-fracyrssb * Z[n_ages])
  if(at.age) return(SPR)
  else return(sum(SPR))
}

get_YPR = function(F, M, sel, waacatch, at.age = FALSE)
{
  n_ages = length(sel)
  YPR = numeric()
  n = 1
  F = F * sel
  Z = F + M
  for(a in 1:(n_ages-1))
  {
    YPR[a] = n[a] * F[a] * waacatch[a] * (1.0 - exp(-Z[a]))/Z[a]
    n[a+1] = n[a] * exp(-Z[a])
  }
  n[n_ages] = n[n_ages]/(1 - exp(-Z[n_ages]))
  YPR[n_ages] = n[n_ages] * F[n_ages] * waacatch[n_ages] * (1.0 - exp(-Z[n_ages]))/Z[n_ages]
  if(at.age) return(YPR)
  else return(sum(YPR))
}

# This is not used in the new function, but was skeleton code for building the new function:
plot.SPR.table <- function(mod, nyrs.ave = 5, plot=TRUE)
{
  origpar <- par(no.readonly = TRUE)
  spr.targ.values <- seq(0.2, 0.8, 0.05)
  n.spr <- length(spr.targ.values)
  dat = mod$env$data
  n_ages<- dat$n_ages
  years <- mod$years
  n_years <- length(years)
  avg.ind = (n_years-nyrs.ave+1):n_years
  #fec.age <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
  mat.age <- apply(dat$mature[avg.ind,],2,mean)
  ssb.waa <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
  catch.waa <- apply(dat$waa[dat$waa_pointer_totcatch,,][avg.ind,],2,mean)
  M.age <- apply(mod$rep$MAA[avg.ind,],2,mean)
  sel = apply(apply(mod$rep$FAA[avg.ind,,,drop=FALSE], 2:3, mean),2,sum) #sum across fleet averages 
  #sel = apply(mod$rep$FAA_tot[avg.ind,],2,mean) #average FAA, then do selectivity
  sel <- sel/max(sel)
  spawn.time <- mean(dat$fracyr_SSB[avg.ind])
  spr0 = get_SPR(F=0, M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
  F.start <- 0.11  # starting guess for optimization routine to find F_SPR%
  
  f.spr.vals <- rep(NA, n.spr)
  ypr.spr.vals <- rep(NA, n.spr)
  conv.vals <- rep(NA, n.spr)
  
  for (i in 1:n.spr)
  {
    t.spr <- spr.targ.values[i]
    
    spr.f <- function(F.start)
    {
      spr = get_SPR(F=F.start, M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
      abs(spr/spr0 - t.spr)
    }
    yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
    f.spr.vals[i] <- yyy$par
    ypr.spr.vals[i] = get_YPR(F = f.spr.vals[i], M=M.age, sel = sel, waacatch= catch.waa)
  }  #end i-loop over SPR values
  
  spr.target.table<- as.data.frame(cbind(spr.targ.values, f.spr.vals, ypr.spr.vals))
  colnames(spr.target.table) <- c("%SPR", "F(%SPR)", "YPR")
  par(mfrow=c(1,1), mar=c(4,4,2,4))
  
  if(plot){ # plot, not table
    plot(spr.targ.values, ypr.spr.vals, type='n', xlab="% SPR Target", ylab="", lwd=2, col="blue3",
         ylim=c(0,1.2*max(ypr.spr.vals)), axes = FALSE)
    box(lwd = 2)
    axis(1, lwd = 2)
    abline(v=seq(0.2,0.8, by=0.05), col="grey85")
    lines(spr.targ.values, ypr.spr.vals, lwd=2, col="blue3" )
    points(spr.targ.values, ypr.spr.vals, pch=19, col="blue3" )
    
    scale.f.spr <- max(f.spr.vals)/max(ypr.spr.vals)
    axis(side=2, #at=seq(0,1,by=0.1)/scale.f.spr, lab=seq(0,1,by=0.1),
         las=2, col='blue3', col.axis="blue3", lwd = 2)
    mtext(side = 2, "Yield per Recruit", line = 3, col = "blue3")
    
    
    lines(spr.targ.values, f.spr.vals/scale.f.spr, lwd = 2, col="red")
    points(spr.targ.values, f.spr.vals/scale.f.spr, pch = 19, col="red")
    axis(side=4, at=seq(0,1,by=0.1)/scale.f.spr, lab=seq(0,1,by=0.1), las=2, col='red', col.axis="red", lwd = 2)
    mtext(side=4, "F (%SPR)", line=3, col="red")
    
    title (paste("SPR Target Reference Points (Years Avg = ", nyrs.ave,")", sep=""), outer=T, line=-1)
  }
  
  if(!plot){ # table, not plot
    par(mfrow=c(1,1), mar=c(2,2,2,2))
    plot(seq(1,15), seq(1,15), type='n', axes=F, bty='n',xlab="",ylab="")
    
    text(x=2,y=14, labels="% SPR", font=2, pos=4)
    text(x=5, y=14, labels="F(%SPR)" , font=2, pos=4)
    text(x=9, y=14, labels="YPR", font=2, pos=4 )
    for (i in 1:n.spr)
    {
      text(x=2, y=seq(n.spr,1, by=-1), labels=round(spr.targ.values,2), cex=1.0, pos=4, font=1)
      text(x=5, y=seq(n.spr,1, by=-1), labels=round(f.spr.vals,4), cex=1.0, pos=4, font=1)
      text(x=9, y=seq(n.spr,1, by=-1), labels=round(ypr.spr.vals,4), cex=1.0, pos=4, font=1)
    }
    title (paste("SPR Target Reference Points (Years Avg = ", nyrs.ave,")", sep=""), outer=TRUE, line=-1)
  }
  par(origpar)
  #write.csv( spr.target.table, file=paste(od,"SPR_Target_Table.csv", sep=""),  row.names=F)
} # end function

#####################################################################################
# This is the new Function: (DON'T CHANGE)
#####################################################################################

get_FX_series <- function(mod, XX_percent = 0.4, plot=TRUE, table=TRUE, pred=TRUE, R_series=TRUE, F_proposed=2) # F proposed what years you're taking your average over for WAA/MAA
{
  
  hold = numeric()
  ssb_hold = numeric()
  
  for (p in 2:mod$input$data$n_years_model) {
  
    nyrs.ave = p
    
    origpar <- par(no.readonly = TRUE)
    spr.targ.values <- seq(0.2, 0.8, 0.05)
    n.spr <- length(spr.targ.values)
    dat = mod$env$data
    n_ages<- dat$n_ages
    years <- mod$years
    n_years <- length(years)
    avg.ind = (n_years-nyrs.ave+1):n_years
    #fec.age <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
    mat.age <- apply(dat$mature[avg.ind,],2,mean)
    ssb.waa <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
    catch.waa <- apply(dat$waa[dat$waa_pointer_totcatch,,][avg.ind,],2,mean)
    M.age <- apply(mod$rep$MAA[avg.ind,],2,mean)
    sel = apply(apply(mod$rep$FAA[avg.ind,,,drop=FALSE], 2:3, mean),2,sum) #sum across fleet averages 
    #sel = apply(mod$rep$FAA_tot[avg.ind,],2,mean) #average FAA, then do selectivity
    sel <- sel/max(sel)
    spawn.time <- mean(dat$fracyr_SSB[avg.ind])
    spr0 = get_SPR(F=0, M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
    F.start <- 0.11  # starting guess for optimization routine to find F_SPR%
    
    f.spr.vals <- rep(NA, n.spr)
    ypr.spr.vals <- rep(NA, n.spr)
    conv.vals <- rep(NA, n.spr)
    ssb.spr.vals <- rep(NA, n.spr)
    
    for (i in 1:n.spr)
    {
      t.spr <- spr.targ.values[i]
      
      spr.f <- function(F.start)
      {
        spr = get_SPR(F=F.start, M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
        abs(spr/spr0 - t.spr)
      }
      yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
      f.spr.vals[i] <- yyy$par
      ypr.spr.vals[i] = get_YPR(F = f.spr.vals[i], M=M.age, sel = sel, waacatch= catch.waa)
    }  #end i-loop over SPR values
    
    if (XX_percent == 0.2) {hold = append(hold, f.spr.vals[1])}
    if (XX_percent == 0.25) {hold = append(hold, f.spr.vals[2])}
    if (XX_percent == 0.3) {hold = append(hold, f.spr.vals[3])}
    if (XX_percent == 0.35) {hold = append(hold, f.spr.vals[4])}
    if (XX_percent == 0.4) {hold = append(hold, f.spr.vals[5])}
    if (XX_percent == 0.45) {hold = append(hold, f.spr.vals[6])}
    if (XX_percent == 0.5) {hold = append(hold, f.spr.vals[7])}
    if (XX_percent == 0.55) {hold = append(hold, f.spr.vals[8])}
    if (XX_percent == 0.6) {hold = append(hold, f.spr.vals[9])}
    if (XX_percent == 0.65) {hold = append(hold, f.spr.vals[10])}
    if (XX_percent == 0.7) {hold = append(hold, f.spr.vals[11])}
    if (XX_percent == 0.75) {hold = append(hold, f.spr.vals[12])}
    if (XX_percent == 0.8) {hold = append(hold, f.spr.vals[13])}
    if (XX_percent %in% c(0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8)) {
      # do nothing 
    } else {
      print("XX_percent given is not a valid percent. Please use from list: c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8)")
    }

    if (pred == TRUE) {
      #for (j in 1:length(hold)) {
        SSBR = get_SPR(F=hold[length(hold)], M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
        ssb_hold = append(ssb_hold, (SSBR*mean(mod$rep$pred_NAA[,1])))
      #}
    } else if (pred == FALSE) {
      #for (j in 1:length(hold)) {
        SSBR = get_SPR(F=hold[length(hold)], M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
        ssb_hold = append(ssb_hold, (SSBR*mean(mod$rep$NAA[,1])))
      #}
    }
    
    
    
  }
  


  
  if (XX_percent == 0.2) {lab_hold = "20"}
  if (XX_percent == 0.25) {lab_hold = "25"}
  if (XX_percent == 0.3) {lab_hold = "30"}
  if (XX_percent == 0.35) {lab_hold = "35"}
  if (XX_percent == 0.4) {lab_hold = "40"}
  if (XX_percent == 0.45) {lab_hold = "45"}
  if (XX_percent == 0.5) {lab_hold = "50"}
  if (XX_percent == 0.55) {lab_hold = "55"}
  if (XX_percent == 0.6) {lab_hold = "60"}
  if (XX_percent == 0.65) {lab_hold = "65"}
  if (XX_percent == 0.7) {lab_hold = "70"}
  if (XX_percent == 0.75) {lab_hold = "75"}
  if (XX_percent == 0.8) {lab_hold = "80"}
  
  #return(hold)
  hold2 = hold
  
  if (plot) {
    to = paste0("F_SSB_",lab_hold,"_Calculation_Series.png")
    png(to, width=6, height=10, units="in", res=1200)
    par(mfrow=c(2,1))
    plot(2:mod$input$data$n_years_model, hold2, 
         xlab = paste0("# Years used in calculation of F",lab_hold), 
         ylab = paste0("F",lab_hold),
         ylim = c((min(hold2)-0.1),(max(hold2)+0.1)),
         xlim = c(0,mod$input$data$n_years_model))
    points(F_proposed-1, hold2[F_proposed-1], col="red", pch=19)
    abline(h=hold2[F_proposed-1], col="red")
    #title (paste0("SSB",lab_hold,"% Calculation Series"), outer=T, line=-1)
    plot(2:mod$input$data$n_years_model, ssb_hold, 
         xlab = paste0("# Years used in calculation of SSB",lab_hold), 
         ylab = paste0("SSB",lab_hold),
         ylim = c((min(ssb_hold)-200),(max(ssb_hold)+200)),
         xlim = c(0,mod$input$data$n_years_model))
    points(F_proposed-1, ssb_hold[F_proposed-1], col="red", pch=19)
    abline(h=ssb_hold[F_proposed-1], col="red")
    title (paste0("F",lab_hold,"% and SSB",lab_hold,"% Calculation Series"), outer=T, line=-1)
    dev.off()
  }
  
  if (table) {
    hold3 = as.data.frame(cbind(c(2:mod$input$data$n_years_model), hold2, ssb_hold))
    names(hold3) = c("# Years Used", paste0("F",lab_hold), paste0("SSB",lab_hold))
    write.csv(hold3, paste0("F_SSB_",lab_hold,"_Calculation_Series.csv"))
  }
  
  if (R_series == TRUE) {
    
    for (r in 1:(length(mod$rep$pred_NAA[,1])-1)) {
      
      ssb_hold = numeric()
      if (pred == TRUE) {
        for (j in 1:length(hold)) {
          
          p=j+1
          nyrs.ave = p
          
          origpar <- par(no.readonly = TRUE)
          spr.targ.values <- seq(0.2, 0.8, 0.05)
          n.spr <- length(spr.targ.values)
          dat = mod$env$data
          n_ages<- dat$n_ages
          years <- mod$years
          n_years <- length(years)
          avg.ind = (n_years-nyrs.ave+1):n_years
          #fec.age <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
          mat.age <- apply(dat$mature[avg.ind,],2,mean)
          ssb.waa <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
          catch.waa <- apply(dat$waa[dat$waa_pointer_totcatch,,][avg.ind,],2,mean)
          M.age <- apply(mod$rep$MAA[avg.ind,],2,mean)
          sel = apply(apply(mod$rep$FAA[avg.ind,,,drop=FALSE], 2:3, mean),2,sum) #sum across fleet averages 
          #sel = apply(mod$rep$FAA_tot[avg.ind,],2,mean) #average FAA, then do selectivity
          sel <- sel/max(sel)
          spawn.time <- mean(dat$fracyr_SSB[avg.ind])
          
          SSBR = get_SPR(F=hold[j], M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
          ssb_hold = append(ssb_hold, (SSBR*mean(mod$rep$pred_NAA[(nrow(mod$rep$pred_NAA)-r):nrow(mod$rep$pred_NAA),1])))
        }
      } else if (pred == FALSE) {
        for (j in 1:length(hold)) {
          
          p=j+1
          nyrs.ave = p
          
          origpar <- par(no.readonly = TRUE)
          spr.targ.values <- seq(0.2, 0.8, 0.05)
          n.spr <- length(spr.targ.values)
          dat = mod$env$data
          n_ages<- dat$n_ages
          years <- mod$years
          n_years <- length(years)
          avg.ind = (n_years-nyrs.ave+1):n_years
          #fec.age <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
          mat.age <- apply(dat$mature[avg.ind,],2,mean)
          ssb.waa <- apply(dat$waa[dat$waa_pointer_ssb,,][avg.ind,],2,mean)
          catch.waa <- apply(dat$waa[dat$waa_pointer_totcatch,,][avg.ind,],2,mean)
          M.age <- apply(mod$rep$MAA[avg.ind,],2,mean)
          sel = apply(apply(mod$rep$FAA[avg.ind,,,drop=FALSE], 2:3, mean),2,sum) #sum across fleet averages 
          #sel = apply(mod$rep$FAA_tot[avg.ind,],2,mean) #average FAA, then do selectivity
          sel <- sel/max(sel)
          spawn.time <- mean(dat$fracyr_SSB[avg.ind])
          
          SSBR = get_SPR(F=hold[j], M=M.age, sel=sel, mat=mat.age, waassb=ssb.waa, fracyrssb = spawn.time)
          ssb_hold = append(ssb_hold, (SSBR*mean(mod$rep$NAA[(nrow(mod$rep$NAA)-r):nrow(mod$rep$NAA),1])))
        }
      }
      if (r == 1) {
        ssb.m = ssb_hold
      } else if (r > 1) {
        ssb.m = cbind(ssb.m, ssb_hold)
      }
      
    }
    
    if (XX_percent == 0.2) {lab_hold = "20"}
    if (XX_percent == 0.25) {lab_hold = "25"}
    if (XX_percent == 0.3) {lab_hold = "30"}
    if (XX_percent == 0.35) {lab_hold = "35"}
    if (XX_percent == 0.4) {lab_hold = "40"}
    if (XX_percent == 0.45) {lab_hold = "45"}
    if (XX_percent == 0.5) {lab_hold = "50"}
    if (XX_percent == 0.55) {lab_hold = "55"}
    if (XX_percent == 0.6) {lab_hold = "60"}
    if (XX_percent == 0.65) {lab_hold = "65"}
    if (XX_percent == 0.7) {lab_hold = "70"}
    if (XX_percent == 0.75) {lab_hold = "75"}
    if (XX_percent == 0.8) {lab_hold = "80"}
    
    #return(hold)
    hold2 = hold
    
    ssb.m2 = as.data.frame(ssb.m)
    
    if (table) {
      hold4 = ssb.m2
      names(hold4) = c(paste0(as.character(2:(nrow(mod$rep$NAA))),"_yr_avg_R"))
      write.csv(hold4, paste0("SSB_",lab_hold,"_Calculation_Series_with_varying_avg_R.csv"))
    }
    
    library(rgl)
    
    if (pred == FALSE) {
    persp3d(2:mod$input$data$n_years_model, 2:mod$input$data$n_years_model, ssb.m, col="blue",
            xlab = "Years used for average WAA",
            ylab = "Years used for average R",
            zlab = "SSB40%",
            zlim = c(0,max(ssb.m)+0.1*(max(ssb.m))))
    } else if (pred == TRUE) {
      persp3d(2:mod$input$data$n_years_model, 2:mod$input$data$n_years_model, ssb.m, col="red",
              xlab = "Years used for average WAA",
              ylab = "Years used for average R",
              zlab = "SSB40%",
              zlim = c(0,max(ssb.m)+0.1*(max(ssb.m))))
    }
    
  }
  
  
  if (!plot & !table) {return(hold2)}
  
} 

#####################################################################################
# Run the new Function (CHANGE FOR YOUR MODELS)
#####################################################################################

# Read in Model(s)
m164_GSI = readRDS(paste0("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/ECOV Runs/ECOV_m164/SNEMA_m4er_GSI_DEVEL/m4er_GSI_DEVEL.rds"))
m95_GSI  = readRDS(paste0("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/ECOV Runs/SNEMA_m4er_GSI_DEVEL/m4er_GSI_DEVEL.rds"))
m164     = readRDS(paste0("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/SNEMA_m164/m164.rds"))
m95      = readRDS(paste0("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/SNEMA_m95/m95.rds"))

# Change where you want to save plots and tables then run function on model
setwd("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/ECOV Runs/ECOV_m164/SNEMA_m4er_GSI_DEVEL")
get_FX_series(m164_GSI, pred=TRUE) 
get_FX_series(m164_GSI, pred=FALSE) 
plot.SPR.table(m164_GSI, plot=FALSE, nyrs.ave = 2)
plot.SPR.table(m164_GSI, plot=FALSE, nyrs.ave = 5)
plot.SPR.table(m164_GSI, plot=FALSE, nyrs.ave = 50)

# Change where you want to save plots and tables then run function on model
setwd("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/SNEMA_m164")
get_FX_series(m164, pred=TRUE) 
get_FX_series(m164, pred=FALSE) 
plot.SPR.table(m164, plot=FALSE, nyrs.ave = 2)
plot.SPR.table(m164, plot=FALSE, nyrs.ave = 5)
plot.SPR.table(m164, plot=FALSE, nyrs.ave = 50)

# Change where you want to save plots and tables then run function on model
setwd("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/ECOV Runs/SNEMA_m4er_GSI_DEVEL")
get_FX_series(m95_GSI, pred=FALSE) 
plot.SPR.table(m164_GSI, plot=FALSE)

# Change where you want to save plots and tables then run function on model
setwd("C:/Users/cameron.hodgdon/Documents/WHAM Output/Yellowtail/SNEMA_m95")
get_FX_series(m95, pred=FALSE) 
plot.SPR.table(m95, plot=FALSE)















# Notes for adding SSBX%

# log_SSB_FXSPR[year] = log(PredR[year]) + log_SPR_FXSPR[year]










