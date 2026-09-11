# Ismooth_functions.R
# generic functions useful for running Ismooth

#remotes::install_packages("cmlegault/PlanBsmooth")
library(PlanBsmooth)

library(ggplot2)
library(dplyr)
library(tidyr)

#' @title clean_stockeff_survey_data
#' @description cleans StockEff survey data by filtering on seasons and index type as well as filling holes 
#' @param data tibble with columns YEAR, SEASON, PURPOSE_CODE, INDEX_TYPE, INDEX (can be read from StockEff csv file, other columns allowed)
#' @param mypurpose code for survey type in StockEff (NEFSC bottom trawl survey = 10)
#' @param myseasons vector of seasons, e.g., c("SPRING", "FALL") 
#' @param myindextype character string "Biomass (kg/tow)" or "Abundance (numbers/tow)"
#' @param myfills tibble of Year, Series, Howfill (latter can be "avg", "pre", "post"), set to NULL if none, default=NULL
#' 
#' @return tibble of Year, Series, Index with Series having up to FILLED_SPRING, FALL, FILLED_FALL, FILLED_FALL_LAGGED levels
clean_stockeff_survey_data <- function(mydata, mypurpose, myseasons, myindextype, myfills=NULL){
  data <- mydata |>
    rename(Year = YEAR,
           Series = SEASON,
           Index = INDEX) |>
    filter(PURPOSE_CODE == mypurpose, 
           Series %in% myseasons,
           INDEX_TYPE == myindextype) |>
    select(Year, Series, Index) 
  
  filled_data <- data 
  
  if (!is.null(myfills)){
    nfills <- dim(myfills)[1]
    for (ifill in 1:nfills){
      filled_data <- fill_hole(filled_data, myfills$Year[ifill], myfills$Series[ifill], myfills$Howfill[ifill])
    }
  }

  filled_data <- filled_data |>
    mutate(Series = case_when(
      Series == "SPRING" ~ "FILLED_SPRING",
      Series == "FALL" ~ "FILLED_FALL",
      .default = "Error"
    ))
  
  lagged_data <- data |>
    filter(Series == "FALL") |>
    mutate(Series = "FALL_LAGGED") |>
    mutate(Index = lag(Index))

  filled_lagged_data <- filled_data |>
    filter(Series == "FILLED_FALL") |>
    mutate(Series = "FILLED_FALL_LAGGED") |>
    mutate(Index = lag(Index)) 
    

  res_data <- rbind(data, filled_data, lagged_data, filled_lagged_data) |>
    mutate(Series = factor(Series, levels = c("SPRING", "FILLED_SPRING", "FALL", "FILLED_FALL", "FALL_LAGGED", "FILLED_FALL_LAGGED")))
  
  return(res_data)
}


#' @title fill_hole
#' @description fills a hole in the survey time series with average of pre and post values, or just the pre or post value
#' @param data tibble with columns Year, Series, Index
#' @param year year to fill hole
#' @param season season to fill hole
#' @param howfill one of "avg", "pre", "post" determining how hole is filled
#' 
#' @return tibble of same size with hole filled (note the order of the series can change)
fill_hole <- function(data, year, season, howfill){
  filled_data <- data |>
    filter(Series == season)
  
  not_filled_data <- data |>
    filter(Series != season)
  
  if(howfill == "avg"){
    filled_data$Index[filled_data$Year == year] <- (filled_data$Index[filled_data$Year == (year-1)] + 
                                                    filled_data$Index[filled_data$Year == (year+1)]) / 2
  } else if(howfill == "pre"){
    filled_data$Index[filled_data$Year == year] <- filled_data$Index[filled_data$Year == (year-1)]
  } else if(howfill == "post"){
    filled_data$Index[filled_data$Year == year] <- filled_data$Index[filled_data$Year == (year+1)]
  } else {
    stop("Something wrong with howfill value, must be 'avg', 'pre', or 'post'")
  }
  
  return(rbind(filled_data, not_filled_data))                                      
}


#' @title plot_6_ts
#' @description creates standardized plot of tibble resulting from clean_stockeff_survey_data function
#' @param data tibble with columns Year, Series, Index
#' @param mytitle character string that will appear at top of plot
#'
#' @return ggplot object than can be seen using print() command
plot_6_ts <- function(data, mytitle){
  p <- ggplot(data, aes(x=Year, y=Index, colour = Series)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Series, ncol=1) +
    ggtitle(mytitle) +
    theme_bw() +
    expand_limits(y=0) +
    theme(legend.position="none")
  
  return(p)
}


#' @title standardize_and_combine_indices
#' @description standardizes data by dividing by mean during defined time period and averages the series
#' @param data tibble with columns Year, Series, Index
#' @param YearStart data filtered to years >= this value
#' @param YearEnd data filtered to years <= this value
#' @param myseries vector of series names in quotes that subsets the Series column
#' @param mynarm Boolean to remove NA values when calculating annual average index, default=FALSE
#' 
#' @return list with std_data tibble that adds Index_Standardized column to data and avg tibble the has columns Year and avg
standardize_and_combine_indices <- function(data, YearStart, YearEnd, myseries, mynarm=FALSE){
  
  std_data <- data |>
    filter(Year >= YearStart, Year <= YearEnd) |>
    filter(Series %in% myseries) |>
    group_by(Series) %>%
    mutate(Index_Standardized = Index / mean(Index, na.rm = TRUE)) 
  
  avg <- std_data |>
    group_by(Year) |>
    summarize(avg = mean(Index_Standardized, na.rm = mynarm))
  
  return(res = list(std_data=std_data, avg=avg))
}


#' @title plot_standardized_and_avg
#' @description plots the results of standardize_and_combine_indices function in a single panel with all series and average labeled
#' @param data list with std_data and avg tibbles
#' @param mytitle character string to appear at top of plot
#' 
#' @return ggplot object that can be seen using print() command
plot_standardized_and_avg <- function(data, mytitle){
  
  p <- ggplot(data$std_data, aes(x=Year, y=Index_Standardized, color=Series)) +
    geom_point() +
    geom_line() +
    geom_point(data=data$avg, aes(x=Year, y=avg, shape = "Average"), color="black", size=2) +
    scale_shape_manual(name = "", values = c("Average" = 16)) +
    theme_bw()
  
  return(p)
}


#' @title wrapIsmooth
#' @description wrapper function to standardize and combine time series, optionally plot, and run Ismooth
#' @param data tibble with columns Year, Series, Index
#' @param YearStart data filtered to years >= this value
#' @param YearEnd data filtered to years <= this value
#' @param myseries vector of series names in quotes that subsets the Series column
#' @param mytitle character string that will appear on top of plot_standardized_and_avg plot and Ismooth plot
#' @param mynarm Boolean to remove NA values when calculating annual average index, default=FALSE
#' @param print_plots when TRUE plots standardized and avg plot as well as Ismooth time series plot, default=FALSE
#' 
#' @return list with std_data tibble, avg tibble, standardized and average plot, and Ismooth results
wrapIsmooth <- function(data, YearStart, YearEnd, myseries, mytitle, mynarm=FALSE, print_plots=FALSE){
  
  sadata <- standardize_and_combine_indices(data, YearStart, YearEnd, myseries, mynarm)
  
  sap <- plot_standardized_and_avg(sadata, mytitle)
  
  Ismoothres <- ApplyPlanBsmooth(sadata$avg, my.title = mytitle, showplots = FALSE)
  
  if (print_plots == TRUE){
    print(sap)
    print(Ismoothres$tsplot)
  }
  
  res <- list(std_data = sadata$std_data,
              avg = sadata$avg,
              sap = sap,
              Ismooth = Ismoothres)
  
  return(res)
}


#' @title filltest
#' @description compare full time series of data with missing and filled time series to see which is preferred
#' @param data tibble with columns Year, Series, Index
#' @param YearStart data filtered to years >= this value
#' @param termyearstart first year of terminal years to be used in analysis
#' @param termyearend last year of terminal years to be used in analysis
#' @param myseries vector of series names in quotes that subsets the Series column
#' @param myfilltests tibble with columns relYear, Series, Howfill where relYear is the year relative to the terminal year in each evaluation
#'
#' @return tibble with columns TermYear (terminal year for that evaluation), Mult_All (Ismooth multiplier using all data), Treatment (either Filled or Missing), Multiplier (Ismooth multiplier for that Treatment), error (Multiplier - Mult_All)
filltest <- function(data, YearStart, termyearstart, termyearend, myseries, myfilltests){
  
  termyears <- seq(termyearstart, termyearend)
  ntermyears <- length(termyears)
  
  # set up results holders
  multall <- rep(NA, ntermyears)
  multmiss <- rep(NA, ntermyears)
  multfill <- rep(NA, ntermyears)
  
  # loop over terminal years
  for (iyear in 1:ntermyears){

    thistermyear <- termyears[iyear]
    
    # make the three data sets
    dall <- data |>
      filter(Year %in% seq(YearStart, thistermyear))
    dmiss <- dall
    dfill <- dall
    
    # use myfilltests to set survey values to NA or average values
    for (ifill in 1:length(myfilltests$relYear)){  
      
      thisfillyear <- thistermyear + myfilltests$relYear[ifill]
      
      dmiss <- dmiss |>
        mutate(Index = case_when(
          Year == thisfillyear & Series == myfilltests$Series[ifill] ~ NA,
          .default = Index
        ))
      
      dfill <- fill_hole(dfill, thisfillyear, myfilltests$Series[ifill], myfilltests$Howfill[ifill]) 
      
    }
    
    # run Ismooth on all three datasets
    resall <- wrapIsmooth(dall, YearStart, thistermyear, myseries, NULL, FALSE)
    resmiss <- wrapIsmooth(dmiss, YearStart, thistermyear, myseries, NULL, FALSE)
    resfill <- wrapIsmooth(dfill, YearStart, thistermyear, myseries, NULL, FALSE)
    
    # collect multipliers
    multall[iyear] <- round(resall$Ismooth$multiplier, 3)
    multmiss[iyear] <- round(resmiss$Ismooth$multiplier, 3)
    multfill[iyear] <- round(resfill$Ismooth$multiplier, 3)
  }
  
  res <- tibble(TermYear = seq(termyearstart, termyearend),
                Mult_All = multall, 
                Missing = multmiss, 
                Filled = multfill) |>
    pivot_longer(cols = c(Missing, Filled),
                 names_to = "Treatment",
                 values_to = "Multiplier") |>
    mutate(error = Multiplier - Mult_All)
  
  return(res)
}


#' @title plot_filltest
#' @description create three plots from results of filltest function: XY, Difference Distribution, Difference Time Series
#' @param res tibble resulting from filltest function
#' @param mytitle character string to appear at top of plot
#' @param printplots Boolean whether to automatically print plots (default=FALSE)
#' 
#' @return list with three plots and root mean square error between multipliers using all data and treatments
plot_filltest <- function(res, mytitle, printplots=FALSE){
  p1 <- ggplot(res, aes(x=Mult_All, y=Multiplier, color=Treatment)) +
    geom_point() +
    geom_abline(slope = 1, linetype="dashed") +
    geom_smooth(method='lm') +
    facet_wrap(~Treatment) +
    xlab("Multiplier Using All Data") +
    ggtitle(mytitle) +
    theme_bw() +
    theme(legend.position = "none")
  
  if (printplots == TRUE) print(p1)
  
  rmse <- res |>
    group_by(Treatment) |>
    summarize(rmse = sqrt(mean(error^2, na.rm=TRUE)),
              label_text = paste0("RMSE = ", round(sqrt(mean(error^2, na.rm=TRUE)), 3)))

  maxerror = max(res$error, na.rm=TRUE)
  
  p2 <- ggplot(res, aes(x=Treatment, y=error, fill=Treatment)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_text(data=rmse, aes(x=Treatment, y=maxerror, label=label_text), vjust = -0.5) +
    theme_bw() +
    labs(
      title = mytitle,
      subtitle = "Distribution of Multiplier Differences",
      x = "Treatment",
      y = "Difference (Treatment - All Data)"
    ) +
    theme(legend.position = "none")
  if (printplots == TRUE) print(p2)
 
  p3 <- ggplot(res, aes(x=TermYear, y=error, color=Treatment, fill=Treatment)) +
    geom_point() +
    geom_smooth() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~Treatment) +
    theme_bw() +
    labs(
      title = mytitle,
      subtitle = "Differences over Time",
      x = "Terminal Year in Ismooth",
      y = "Difference (Treatment - All Data)"
    ) +
    theme(legend.position = "none")
  if (printplots == TRUE) print(p3)
  
  return(list(rmse=rmse, p1=p1, p2=p2, p3=p3))
}
