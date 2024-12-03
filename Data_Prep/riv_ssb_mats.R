
# There are three functions below. The first calculates the Rivard (Jan 1)
# WAA. The second calculates the SSB weights. The third function
# uses the first two to get both Rivard and SSB weights in one go.
# I ended up putting them all together in a single function for posting
# to this github page since it's a little more contained that way.

# calc.rivard.wts() -- used to get the Rivard weights -- wasn't written by
# me; thanks to Alan Seaver, Liz Brooks and possibly others.

# Inputs---
# my_mat:       Mid-year WAA matrix (as an object of class matrix)
# riv_ty_avg_n: The SSB WAA calculations require entries in year y+1 so
#               Rivard WAA are needed in that year. This parameter is the
#               number of years to average over in the Rivard matrix to
#               get that vector (e.g., average over 5 years). If you only
#               care about the Rivard WAA not the SSB WAA you can enter 1.
# frac_ann:     The fraction of the year that has elapsed when spawning occurs.
#               The Rivard VB program did this by month and the assumption was
#               that spawning occurred on the first of the month, so if you
#               entered November it would be Nov 1, indicating spawning through 
#               the end of October and, translated to frac_ann, would be 10/12.

# Outputs---
# RIVARD_WAA:        The Rivard (Jan 1) WAA matrix
# SSB_WAA:           The SSB WAA matrix
# RIVARD_LY_AVG_WAA: The Rivard WAA matrix, including the additional year
#                    (y+1) that was used to calculate SSB_WAA
# specs:             The information that was passed to the function arguments


# Function that combines the J1 Rivard weights and the SSB weights into one
riv_ssb_mats <- function(my_mat, riv_ty_avg_n, frac_ann){
  
  if(!('matrix' %in% class(my_mat))){
    stop('my_mat should be class \'matrix\'')
  }
  
  ## ---------------------------------- ##
  # Rivard weights function, based on Calcrivard.vb (Rivard weights function
  # in NFT). Same as that, though with no non-plus-group option.
  
  # The following Rivard calculation assumes that there is a plus group.
  # If there isn't the code needs to be revised
  calc.rivard.wts <- function(input.data, term.yr.avg)
  {
    # input.data <- avg.wt.age.allyrs
    # term.yr.avg <- 5
    
    nages <- ncol(input.data)
    
    nyrs <- nrow(input.data)
    lyr <- tail(rownames(input.data),1)
    
    
    # Step 1: Put catch WAA data on log scale
    W1 <- log(input.data)
    
    
    # Step 2: Since last age is a plus group, calculate average weights for 1:nages-1
    W2 <- W1
    W2[,] <- NA
    for (a in (2:(nages-1)))
    {
      for (y in (2:nyrs))
      {
        W2[y,a] = (W1[y,a] + W1[y-1,a-1]) / 2
      } # End of year loop
    } # End of age loop
    
    
    # Step 3: Calculate W for first ages  
    for (y in (1:(nyrs-1)))
    {
      W2[y,1] = 2*W1[y,1] - W2[y+1,2]
    } # End of year loop
    
    
    # Step 4: Calculate W for first year
    for (a in (1:(nages-2)))
    {
      W2[1,a] <- 2*W1[1,a] - W2[2,a+1]
    } # End of age loop  
    
    
    # Step 5: Calculate W for nage-1 in first year
    W2[1,nages-1] <- (W1[1,nages-1] + W1[1,nages-2]) / 2
    
    
    # Step 6: Calculate W for age-1 in last year
    W2[nyrs,1] = 2*W1[nyrs,1] - W2[nyrs,2]
    
    
    # Step 7: Calculate W for plus group
    for (y in (1:nyrs))
    {
      W2[y,nages] = W1[y,nages]
    } # End of year loop
    
    
    # Step 8: Convert back to normal scale
    W3 <- exp(W2)
    
    
    # Step 9: Rivard weight for terminal year + 1    
    # For all ages, use arithmetic average for last term.yr.avg years
    W4 <- as.data.frame(W3)
    new.yr <- as.character(as.numeric(lyr)+1)
    W4[new.yr,] <- NA
    
    for (a in (1:nages))
    {
      x <- 0
      for (y in (1:term.yr.avg))
      {
        x <- x + W4[(nyrs-y+1),a]
      } # End of year loop
      W4[nyrs+1,a] <- x/term.yr.avg
    } # End of age loop
    
    W4
    
  }  # End of calc.rivard.wts. fx
  
  ## ---------------------------------- ##
  ## ---------------------------------- ##
  # Function to calculate the SSB WAA based on Rivard and mid-year WAA as well
  # as the fraction of the year that has elapsed before spawning.
  calc.ssb.wts <- function(riv_mat, my_mat, frac_ann){
    
    # Calculations are done on log scale
    my_mat_log <- log(my_mat)
    riv_mat_log <- log(riv_mat)
    
    # Set up the SSB matrix
    ssb_mat_log <- matrix(nrow = nrow(my_mat), ncol = ncol(my_mat))
    
    # Fill in the SSB matrix
    # Cycle through the years
    for(y in 1:nrow(my_mat)){
      
      # The calculation differs depending on whether you're in the first
      # or second half of the year. In both cases it's based on the difference
      # between the J1 Rivard weights and the mid-year weights adjusted by the
      # fraction of the year that's elapsed, only in the first semester it's
      # looking at this year's J1 weight and in the second semester it's looking
      # at next year's J1 weight.
      
      # If first half of the year
      if(frac_ann >= 0 & frac_ann <= 0.5){
        
        # Cycle through the ages
        for(a in 1:ncol(my_mat)){
          
          # Take the annual fraction and convert it into a fraction of a semester
          frac_sem <- frac_ann / 0.5
          
          # Fill in the log SSB WAA matrix
          ssb_mat_log[y,a] <- riv_mat_log[y,a] + 
            (my_mat_log[y,a] - riv_mat_log[y,a]) * 
            frac_sem
          
        }
        
      # If second half of the year  
      }else if(frac_ann > 0.5 & frac_ann < 1){
        
        # Cycle through ages -- in this case since we use a+1 you can't go all
        # the way to the last age
        for(a in 1:(ncol(my_mat)-1)){
          
          # Take the annual fraction and convert it into a fraction of a semester
          frac_sem <- (frac_ann - 0.5) / 0.5
          
          # Fill in the log SSB WAA matrix
          ssb_mat_log[y,a] <- (my_mat_log[y,a] +
                                 (riv_mat_log[y+1,a+1] - my_mat_log[y,a]) *
                                 frac_sem)
        }
        
        # Assume that in the 2nd semester case the plus group SSB WAA is equal
        # to the Rivard weights for that year and age
        ssb_mat_log[y,ncol(ssb_mat_log)] <- riv_mat_log[y,ncol(riv_mat_log)]
        
      }else{
        stop('frac_ann must be a scalar [0,1)')
      }
    }
    
    ssb_mat <- exp(ssb_mat_log)
    # Preserve names if they exist
    row.names(ssb_mat) <- row.names(my_mat)
    colnames(ssb_mat) <- colnames(my_mat)
    
    return(ssb_mat)
  }
  
  ## ---------------------------------- ##
  ## ---------------------------------- ##
  
  
  # Get the J1 Rivard weights
  riv_waa <- calc.rivard.wts(input.data = my_mat,
                             term.yr.avg = riv_ty_avg_n)
  
  # Get the SSB weights
  ssb_waa <- calc.ssb.wts(riv_mat = riv_waa,
                          my_mat = my_mat,
                          frac_ann = frac_ann)
  
  # input specifications so they aren't totally lost
  sp <- list(my_mat = my_mat,
             riv_ty_avg_n = riv_ty_avg_n,
             frac_ann = frac_ann)
  
  # Write out the results. Return both the Rivard ending in the terminal
  # year and the Rivard that includes year y+1 with the n-years average
  # that was used in the SSB calcs
  return(list(RIVARD_WAA = riv_waa[-nrow(riv_waa),],
              SSB_WAA = ssb_waa,
              RIVARD_LY_AVG_WAA = riv_waa,
              specs = sp))
}


