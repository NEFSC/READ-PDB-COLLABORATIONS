# Homemade Change Point Analysis Function
# Cameron Hodgdon
# 2024 (Last updated: 5/30/2025)

################################################################################
# The Function Itself
################################################################################

ChangePoint = function(E.Data, Y.Data, breaks=1, plot=T, type="Mean") { 
  
  if (type == "Linear") {
  
    if (breaks == 1) {
      
      hold = numeric()
      for (i in 3:(length(Y.Data)-3)) {
        lmGSI1 = lm(E.Data[1:i]~Y.Data[1:i]) 
        lmGSI2 = lm(E.Data[(i+1):length(Y.Data)]~Y.Data[(i+1):length(Y.Data)])
        hold = append(hold, (sum(abs(summary(lmGSI1)[["residuals"]])) + sum(abs(summary(lmGSI2)[["residuals"]]))))
      }
      a = hold
      b = min(hold)
      c = which(hold == min(hold))+2
      d = Y.Data[c]
      
      if (plot == T) {
        Y.Data.1 = Y.Data[1:c]
        Y.Data.2 = Y.Data[(c+1):length(Y.Data)]
        
        E.Data.1 = E.Data[1:c]
        E.Data.2 = E.Data[(c+1):length(E.Data)]
        
        par(mar=c(5.1,4.1,4.1,2.1))
        plot(Y.Data, E.Data, typ="l", ylab = "Environmental Data", xlab = "Year", 
             main = paste0(type," with ",breaks," Break: ",d,"/",d+1))
        lines(Y.Data.1, E.Data.1, col = "blue", lwd=2)
        lines(Y.Data.2, E.Data.2, col = "red", lwd=2)
        abline(v=d+0.5)
      }
      
      lmGSI1 = lm(E.Data[1:c]~Y.Data[1:c]) 
      lmGSI2 = lm(E.Data[(c+1):length(Y.Data)]~Y.Data[(c+1):length(Y.Data)])
      print(paste0("Linear with 1 Break: ",d,"/",d+1))
      print(paste0("Linear equation ",Y.Data[1],"-",d,": ","y = (",
                   round(as.numeric(lmGSI1$coefficients[2]),4),")*x + (",
                   round(as.numeric(lmGSI1$coefficients[1]),4),")"))
      print(paste0("Linear equation ",d+1,"-",Y.Data[length(Y.Data)],": ","y = (",
                   round(as.numeric(lmGSI2$coefficients[2]),4),")*x + (",
                   round(as.numeric(lmGSI2$coefficients[1]),4),")"))
      
      return(list(Block_1=as.vector(lmGSI1$coefficients), Block_2=as.vector(lmGSI2$coefficients)))
    
    } else if (breaks == 2) {
      
      hold = numeric()
      holdi = holdj = numeric()
      for (i in 3:(length(Y.Data)-6)) {
        for (j in (i+3):(length(Y.Data)-3)) {
          lmGSI1 = lm(E.Data[1:i]~Y.Data[1:i]) 
          lmGSI2 = lm(E.Data[(i+1):j]~Y.Data[(i+1):j])
          lmGSI3 = lm(E.Data[(j+1):length(Y.Data)]~Y.Data[(j+1):length(Y.Data)])
          hold = append(hold, (sum(abs(summary(lmGSI1)[["residuals"]])) + 
                               sum(abs(summary(lmGSI2)[["residuals"]])) +
                               sum(abs(summary(lmGSI3)[["residuals"]]))))
          holdi = append(holdi, i)
          holdj = append(holdj, j)
        }
      }
      a = hold
      b = min(hold)
      c = which(hold == min(hold))
      ii = holdi[c]
      jj = holdj[c]
      d1 = Y.Data[ii]
      d2 = Y.Data[jj]
      
      if (plot == T) {
        Y.Data.1 = Y.Data[1:ii]
        Y.Data.2 = Y.Data[(ii+1):jj]
        Y.Data.3 = Y.Data[(jj+1):length(Y.Data)]
        
        E.Data.1 = E.Data[1:ii]
        E.Data.2 = E.Data[(ii+1):jj]
        E.Data.3 = E.Data[(jj+1):length(E.Data)]
        
        par(mar=c(5.1,4.1,4.1,2.1))
        plot(Y.Data, E.Data, typ="l", ylab = "Environmental Data", xlab = "Year", 
             main = paste0(type," with ",breaks," Breaks: ",d1,"/",d1+1," & ",d2,"/",d2+1))
        lines(Y.Data.1, E.Data.1, col = "blue", lwd=2)
        lines(Y.Data.2, E.Data.2, col = "green", lwd=2)
        lines(Y.Data.3, E.Data.3, col = "red", lwd=2)
        abline(v=d1+0.5)
        abline(v=d2+0.5)
      }
      
      lmGSI1 = lm(E.Data[1:ii]~Y.Data[1:ii]) 
      lmGSI2 = lm(E.Data[(ii+1):jj]~Y.Data[(ii+1):jj])
      lmGSI2 = lm(E.Data[(jj+1):length(Y.Data)]~Y.Data[(jj+1):length(Y.Data)])
      print(paste0("Linear with 2 Breaks: ",d1,"/",d1+1," & ",d2,"/",d2+1))
      print(paste0("Linear equation ",Y.Data[1],"-",d1,": ","y = (",
                   round(as.numeric(lmGSI1$coefficients[2]),4),")*x + (",
                   round(as.numeric(lmGSI1$coefficients[1]),4),")"))
      print(paste0("Linear equation ",d1+1,"-",d2,": ","y = (",
                   round(as.numeric(lmGSI2$coefficients[2]),4),")*x + (",
                   round(as.numeric(lmGSI2$coefficients[1]),4),")"))
      print(paste0("Linear equation ",d2+1,"-",Y.Data[length(Y.Data)],": ","y = (",
                   round(as.numeric(lmGSI3$coefficients[2]),4),")*x + (",
                   round(as.numeric(lmGSI3$coefficients[1]),4),")"))
      
      return(list(Block_1=as.vector(lmGSI1$coefficients), Block_2=as.vector(lmGSI2$coefficients), Block_3=as.vector(lmGSI3$coefficients)))
      
    } else {
      
      print("Please specify a number of breaks equal to 1 or 2.")
      
    }
    
  }
  
  if (type == "Mean") {
    
    if (breaks == 1) {
      
      hold = numeric()
      for (i in 3:(length(Y.Data)-3)) {
        lmGSI1 = mean(E.Data[1:i]) 
        lmGSI2 = mean(E.Data[(i+1):length(Y.Data)])
        hold = append(hold, (sum(abs(E.Data[1:i]-lmGSI1)) + sum(abs(E.Data[(i+1):length(Y.Data)]-lmGSI2))))
      }
      a = hold
      b = min(hold)
      c = which(hold == min(hold))+2
      d = Y.Data[c]
      
      if (plot == T) {
        Y.Data.1 = Y.Data[1:c]
        Y.Data.2 = Y.Data[(c+1):length(Y.Data)]
        
        E.Data.1 = E.Data[1:c]
        E.Data.2 = E.Data[(c+1):length(E.Data)]
        
        par(mar=c(5.1,4.1,4.1,2.1))
        plot(Y.Data, E.Data, typ="l", ylab = "Environmental Data", xlab = "Year", 
             main = paste0(type," with ",breaks," Break: ",d,"/",d+1))
        lines(Y.Data.1, E.Data.1, col = "blue",lwd=2)
        lines(Y.Data.2, E.Data.2, col = "red",lwd=2)
        abline(v=d+0.5)
      }
      
      print(paste0("Mean with 1 Break: ",d,"/",d+1))
      print(paste0("Mean value ",Y.Data[1],"-",d,": ",round(mean(E.Data.1),4)))
      print(paste0("Mean value ",d+1,"-",Y.Data[length(Y.Data)],": ",round(mean(E.Data.2),4)))
      
      return(list(Block_1=mean(E.Data.1), Block_2=mean(E.Data.2)))
      
    } else if (breaks == 2) {
      
      hold = numeric()
      holdi = holdj = numeric()
      for (i in 3:(length(Y.Data)-6)) {
        for (j in (i+3):(length(Y.Data)-3)) {
          lmGSI1 = mean(E.Data[1:i]) 
          lmGSI2 = mean(E.Data[(i+1):j])
          lmGSI3 = mean(E.Data[(j+1):length(Y.Data)])
          hold = append(hold, (sum(abs(E.Data[1:i]-lmGSI1)) + 
                               sum(abs(E.Data[(i+1):j]-lmGSI2)) +
                               sum(abs(E.Data[(j+1):length(Y.Data)]-lmGSI3))))
          holdi = append(holdi, i)
          holdj = append(holdj, j)
        }
      }
      a = hold
      b = min(hold)
      c = which(hold == min(hold))
      ii = holdi[c]
      jj = holdj[c]
      d1 = Y.Data[ii]
      d2 = Y.Data[jj]
      
      if (plot == T) {
        Y.Data.1 = Y.Data[1:ii]
        Y.Data.2 = Y.Data[(ii+1):jj]
        Y.Data.3 = Y.Data[(jj+1):length(Y.Data)]
        
        E.Data.1 = E.Data[1:ii]
        E.Data.2 = E.Data[(ii+1):jj]
        E.Data.3 = E.Data[(jj+1):length(E.Data)]
        
        par(mar=c(5.1,4.1,4.1,2.1))
        plot(Y.Data, E.Data, typ="l", ylab = "Environmental Data", xlab = "Year", 
             main = paste0(type," with ",breaks," Breaks: ",d1,"/",d1+1," & ",d2,"/",d2+1))
        lines(Y.Data.1, E.Data.1, col = "blue", lwd=2)
        lines(Y.Data.2, E.Data.2, col = "green", lwd=2)
        lines(Y.Data.3, E.Data.3, col = "red", lwd=2)
        abline(v=d1+0.5)
        abline(v=d2+0.5)
      }
      
      print(paste0("Mean with 2 Breaks: ",d1,"/",d1+1," & ",d2,"/",d2+1))
      print(paste0("Mean value ",Y.Data[1],"-",d1,": ",round(mean(E.Data.1),4)))
      print(paste0("Mean value ",d1+1,"-",d2,": ",round(mean(E.Data.2),4)))
      print(paste0("Mean value ",d2+1,"-",Y.Data[length(Y.Data)],": ",round(mean(E.Data.3),4)))
      
      return(list(Block_1=mean(E.Data.1), Block_2=mean(E.Data.2), Block_3=mean(E.Data.3)))
      
    } else {
      
      print("Please specify a number of breaks equal to 1 or 2.")
      
    }
    
  }
  
}

################################################################################
# Explanation of Methods
################################################################################

# Run the function with these inputs:
# (1) E.data = Vector of environmental Data over 'Y' years (e.g. see "GSI" in example below)
# (2) Y.data = Vector of 'Y' Years (e.g. 1972:2022)
# (3) breaks = How many breaks you want to find (1 or 2),
# (4) plot   = Whether you want plots or not (TRUE or FALSE).
# (5) type   = How you want to estimate change points ("Linear" or "Mean") 

# "Linear" uses linear models fit to subsets of the time series. Whichever pair (or triplet) of fits 
# leads to the lowest absolute sum of the residuals across the fits determines the change points.

# "Mean" takes the mean of values in different subsets of the time series. Whichever pair 
# (or triplet) of means leads to the lowest absolute sum of the residuals of the means determines 
# the change points. 

# Notes: 
# (1) This function cannot handle more than 2 breakpoints (three regimes). 
# (2) You should not compare fits across tests with different numbers of breaks. 
#     More breaks will ALWAYS lead to better fits to the series, but a series of 50 years doesn't have 
#     50 regimes (which would have zero residuals in the fit). Whether to use 1 or 2 breaks is therefore 
#     somewhat subjective and should be based on knowledge of the series/covariate/ecosystem. 
# (3) If the goal in using this code is to determine a period to use for projections in a stock assessment model (e.g. WHAM), 
#     You should NOT use results from the "Linear" method. This does not represent a period of stability 
#     for which to assume a mean over and there would be issues implementing the data in WHAM because you cannot specify 
#     uncertainty of the covariate in the projections with this method.
# (4) This function does not help you decide whether or not there is a regime shift or change point, it only 
#     answers the question: "if we assume there is a change point in this time series, where most likely does it occur?"

# For any questions, please contact cameron.hodgdon@noaa.gov

################################################################################
# Example
################################################################################

# some sample data
Years = 1972:2022
GSI   = c(-0.109712575,  0.076363721,  0.632893334, -0.053468684,  0.480264522,
           0.256000464, -0.500067920, -0.265355758, -0.033772743, -0.601448520,
          -0.456744514, -0.277220145,  0.382058094,  0.622163360,  0.159591758,
          -0.704556000, -0.656188905, -0.914378798,  0.001404853,  0.747481844,
           0.220873278,  0.530615220,  0.760266352,  0.974364837, -0.499655267,
          -0.456565539, -0.321096794,  1.058633957,  0.766316609,  0.287781290,
           1.280140505,  0.174175069, -0.316501735, -0.251944572, -0.040604510,
          -0.834357963,  0.207317969,  0.647687509,  0.308852735, -0.099931768,
           0.680647049,  0.684298838,  1.100395716,  1.271662940,  1.468743790,
           0.962001083,  1.250245373,  1.581595740,  1.244002658,  1.300514748,
           1.337110139)


par(mfrow=c(2,2)) # run only if you want all four plots on the same panel

# Using type = "Mean"
ChangePoint(E.Data=GSI, Y.Data=Years, breaks=1, plot=T, type="Mean")
ChangePoint(E.Data=GSI, Y.Data=Years, breaks=2, plot=T, type="Mean")
# When using type = "Mean", output will be: 
#   - Years that comprise each regime block
#   - Mean values of the covariate for each regime block.

# Using type = "Linear"
ChangePoint(E.Data=GSI, Y.Data=Years, breaks=1, plot=T, type="Linear")
ChangePoint(E.Data=GSI, Y.Data=Years, breaks=2, plot=T, type="Linear")
# When using type = "Linear", output will be: 
#   - Years that comprise each regime block 
#   - Slopes and intercepts of the linear equation fit to each regime block.

