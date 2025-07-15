#' @title calcIndex
#' @description Use calibrated survey data to replicate stockEff survey index calculations. Both biomass and abundance indices will be calculated. Only those calibrations already applied by stockEff can be used.
#' @param IndexType A string specifying the type of index calculation to perform, options include:
#' \itemize{
#'   \item{"Default" - Function runs SQL queries to stockEff oracle backend to calculate abundance and biomass indices as done in stockEff, requires a network connection and login credentials}
#'   \item{"Custom" - Function calculates abundance and biomass indices using the provided surveyTowData matrix}
#' }
#' @param doLogin Only required if IndexType == "Default". A boolean, if TRUE then function will ask for oracle login credentials to set up a "connection" login object, if FALSE then function assumes oracle connection has already been made and code will use existing global "connection" object to run. Default = FALSE. 
#' @param surveyTowData Only required if IndexType == "Custom". A matrix of custom survey tows with the following columns that match those in the STOCKEFF.I_SV_MERGED_CATCH_CALIB_O oracle product, unless otherwise specified
#' \itemize{
#'   \item{COMMON_NAME}
#'   \item{SPECIES_ITIS}
#'   \item{SVSPP}
#'   \item{STOCK_ABBREV}
#'   \item{STOCK_NAME}
#'   \item{PURPOSE_CODE}
#'   \item{YEAR}
#'   \item{SEASON}
#'   \item{CRUISE6}
#'   \item{STRATUM}
#'   \item{TOW}
#'   \item{STATION}
#'   \item{SEX}
#'   \item{SEX_TYPE}
#'   \item{CATCH_WT_CAL = One of the following weight calibrations: CATCH_WT_V_CAL, CATCH_WT_D_CAL, CATCH_WT_G_CAL, CATCH_WT_B_CAL available in STOCKEFF.I_SV_MERGED_CATCH_CALIB_O}
#'   \item{CATCH_NO_CAL = One of the following number calibrations: CATCH_NO_V_CAL, CATCH_NO_D_CAL, CATCH_NO_G_CAL, CATCH_NO_B_CAL available in STOCKEFF.I_SV_MERGED_CATCH_CALIB_O}
#'   \item{STRATUM_AREA = Area of each survey strata, available in STOCKEFF.E_SV_STRATA_C}
#' }
#' @param calibration A string specifying the type of calibrations to apply, not all stocks will apply all calibrations but stockEff applies those specified cumulatively in order: vessel, door, gear, bigelow (all applied in B_CAL option). Default = "B_CAL" (i.e. all calibrations specified in stockEff will be applied, if Bigelow calibrations are not applied then "G_CAL" and "B_CAL" columns will contain the same information)
#' \itemize{
#'   \item{None - no calibrations, requires CATCH_WT and CATCH_NO columns in tow data}
#'   \item{V_CAL - vessel calibrations, requires CATCH_WT_V_CAL and CATCH_NO_V_CAL columns in tow data}
#'   \item{D_CAL - door + vessel calibrations, requires CATCH_WT_D_CAL and CATCH_NO_D_CAL columns in tow data}
#'   \item{G_CAL - gear + door + vessel calibrations, requires CATCH_WT_G_CAL and CATCH_NO_G_CAL columns in tow data}
#'   \item{B_CAL - bigelow + all calibrations, requires CATCH_WT_B_CAL and CATCH_NO_B_CAL columns in tow data}
#' }
#' @param summary_strata A vector of strata for which strata-specific mean weight and numbers (by year, season, survey) should be returned in the strata_means_summary object, default returns means for all strata.
#'
#' @return A list containing: If IndexType == "Custom", a list containing calculated indices, if IndexType == "Default" a list containing the following:
#' \itemize{
#'   \item{indices - A matrix containing stratified mean indices by weight (WT) and numbers (NO), by year, season, and survey}
#'   \item{strata_means_summary - A matrix of strata means used in index calculations by weight (CATCH_WT) and numbers (CATCH_NO) for each year, season, and survey. Strata included in summary are specified using summary_strata argument}
#' }
#' If IndexType == "Default" also include the following in the returned list:
#' \itemize{
#'   \item{indices - A matrix containing stratified mean indices calculated by weight (WT) and numbers (NO) for specified species, year and season, returned for all IndexTypes}
#'   \item{all_tows - A matrix of all tows used in the default calculation which can be subset to populate a custom surveyTowData input}
#'   \item{pass_strata_check - A boolean, if TRUE strata-specific CATCH_WT and CATCH_NO calculations match stockEff backend V_SV_STRAT_SUMMARY_O product exactly, if false look at check_strata and strata_diff returns}
#'   \item{check_strata - If pass_strata_check == FALSE, returns a table of strata/year/season combinations with different CATCH_WT or CATCH_NO compared to stockEff backend V_SV_STRAT_SUMMARY_O product, if pass_strata_check == TRUE returns NA}
#'   \item{strata_diff - If pass_strata_check == FALSE, summarizes spread of differences between CATCH_WT or CATCH_NO calculations compared to stockEff backend V_SV_STRAT_SUMMARY_O product (if small, differences are due to rounding errors), if pass_strata_check == TRUE returns NA}
#'   \item{pass_index_check - A boolean, if TRUE STRAT_MEAN_WT and STRAT_MEAN_NO indices and their associated variance, standard error, and CVs match stockEff backend V_SV_STRAT_IND_O product exactly, if false look at check_index and index_diff returns}
#'   \item{check_index - If pass_index_check == FALSE, returns a table of index/year/season combinations with different STRAT_MEAN, VAR, STDERROR, or CV calculations for WT or NO compared to stockEff backend V_SV_STRAT_IND_O product, if pass_index == TRUE returns NA}
#'   \item{index_diff - If pass_index_check == FALSE, summarizes spread of differences between STRAT_MEAN, VAR, STDERROR, and CV calculation for WT and NO indices compared to stockEff backend V_STRAT_IND_O product (if small, differences are due to rounding errors), if pass_index_check == TRUE returns NA}
#' }
#' 
#' NOTES:
#' - When a strata is not sampled for a given year/season the index is calculated without that strata (i.e. the TOTAL_SAMP_AREA will be smaller because the strata is excluded from calculations)
#' - If specified for the species/stock, this code will also calculate indices other than the NEFSC BTS (PURPOSE_CODE == 10)
#' - stockEff steps through V_CAL, D_CAL, G_CAL and B_CAL calibration steps but will only apply those based on the stock specification, if a calibrations was not applied then the corresponding column will match the prior column (e.g. if bigelow calibrations aren't applied then G_CAL = B_CAL)
#'
#' @examples
#' cod <- calcIndex(species_itis = 164712,
#'                             stock_abbrev = "GBK",
#'                             IndexType = "Default",
#'                             doLogin = FALSE,
#'                             calibration = "B_CAL")
#' 
#' plaice <- calcIndex(species_itis = 172877,
#'                     stock_abbrev = "UNIT",
#'                     IndexType = "Default",
#'                     doLogin = FALSE,
#'                     calibration = "B_CAL")
#' 
#' winterFlounder <- calcIndex(species_itis = 172905,
#'                             stock_abbrev = "GOMWF",
#'                             IndexType = "Default",
#'                             doLogin = FALSE,
#'                             calibration = "B_CAL")
#' 
customExample <- calcIndex(IndexType = "Custom",
                           surveyTowData = winterFlounder$all_tows) # Because tows were not resampled this will create the same output as IndexType = "Default"


library(tidyverse)
library(ROracle)

calcIndex <- function(species_itis = NULL,
                      stock_abbrev = NULL,
                      IndexType = "Default",
                      doLogin = FALSE,
                      surveyTowData = NULL,
                      calibration = "B_CAL",
                      summary_strata = NULL){
  
  # Oracle login info
  if(doLogin == TRUE){
    connection <- dbConnect(drv = dbDriver("Oracle"),
                            username = rstudioapi::askForPassword("Oracle user name"),
                            password = rstudioapi::askForPassword("Oracle password"),
                            dbname = rstudioapi::askForPassword("Oracle database name"))
  } 
  
  ##### Default index tow data from stockEff #####
  if(IndexType == "Default"){
    ###### Run ROracle queries up front ######
    # Start with calibrated data: stockeff.i_sv_merged_catch_calib_o 
    catch_calib_o <- ROracle::dbGetQuery(connection, statement = paste0("select * from STOCKEFF.I_SV_MERGED_CATCH_CALIB_O where species_itis = '", species_itis, "' and stock_abbrev = '", stock_abbrev,"'"))
    
    # Add station counts from STOCKEFF.I_SV_STATION_O
    stations <- ROracle::dbGetQuery(connection, statement = paste0("select * from STOCKEFF.I_SV_STATION_O where species_itis = '", species_itis, "' and stock_abbrev = '", stock_abbrev, "'"))
    
    # Strata areas from STOCKEFF.E_SV_STRATA_C 
    strata_areas <- ROracle::dbGetQuery(connection, statement = "select * from STOCKEFF.E_SV_STRATA_C")
    
    # Check strata mean calculations
    V_SV_STRAT_SUMMARY_O <-  ROracle::dbGetQuery(connection, statement = paste0("select * from STOCKEFF.V_SV_STRAT_SUMMARY_O where species_itis = '", species_itis, "' and stock_abbrev = '", stock_abbrev, "'")) 
    
    # Check index calculations
    V_SV_STRAT_IND_O <- ROracle::dbGetQuery(connection, statement = paste0("select * from STOCKEFF.V_SV_STRAT_IND_O where species_itis = '", species_itis, "' and stock_abbrev = '", stock_abbrev, "'"))  # from V_SV_STRAT_IND_O
    
    ###### Survey data processing ######
  # Collapse duplicate WT/NO inputs when multiple lengths sampled
  catch_cal_temp <- catch_calib_o %>% 
    select(COMMON_NAME, SPECIES_ITIS, SVSPP, STOCK_ABBREV, STOCK_NAME, PURPOSE_CODE, YEAR, SEASON, CRUISE6, STRATUM, STATION, SEX, SEX_TYPE, TOW, CATCH_WT_B_CAL, CATCH_NO_B_CAL) %>% 
    distinct()  # Don't sum across duplicate WT/NO inputs when multiple lengths 
    
  #Pick specified calibration
  if(calibration == "None"){
    catch_cal <- catch_cal_temp %>%
      group_by(COMMON_NAME, SPECIES_ITIS, SVSPP, STOCK_ABBREV, STOCK_NAME, PURPOSE_CODE, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW) %>% # If multiple Sex or SEX_TYPE categories exist sum over them
      dplyr::mutate(CATCH_WT_CAL = sum(CATCH_WT), # No calibrations applied
                    CATCH_NO_CAL = sum(CATCH_NO)) 
  } else if(calibration == "V_CAL"){
    catch_cal <- catch_cal_temp %>%
      group_by(COMMON_NAME, SPECIES_ITIS, SVSPP, STOCK_ABBREV, STOCK_NAME, PURPOSE_CODE, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW) %>% # If multiple Sex or SEX_TYPE categories exist sum over them
      dplyr::mutate(CATCH_WT_CAL = sum(CATCH_WT_V_CAL), # Vessel calibrations applied
                    CATCH_NO_CAL = sum(CATCH_NO_V_CAL))  
  } else if(calibration == "D_CAL"){
    catch_cal <- catch_cal_temp %>%
      dplyr::mutate(CATCH_WT_CAL = sum(CATCH_WT_D_CAL), # Door + vessel calibrations applied
                    CATCH_NO_CAL = sum(CATCH_NO_D_CAL)) 
  } else if(calibration == "G_CAL"){
    catch_cal <- catch_cal_temp %>%
      group_by(COMMON_NAME, SPECIES_ITIS, SVSPP, STOCK_ABBREV, STOCK_NAME, PURPOSE_CODE, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW) %>% # If multiple Sex or SEX_TYPE categories exist sum over them
      dplyr::mutate(CATCH_WT_CAL = sum(CATCH_WT_G_CAL), # Gear + door + vessel calibrations applied
                    CATCH_NO_CAL = sum(CATCH_NO_G_CAL)) 
  } else if(calibration == "B_CAL"){
    catch_cal <- catch_cal_temp %>%
      group_by(COMMON_NAME, SPECIES_ITIS, SVSPP, STOCK_ABBREV, STOCK_NAME, PURPOSE_CODE, YEAR, SEASON, CRUISE6, STRATUM, STATION, TOW) %>% # If multiple Sex or SEX_TYPE categories exist sum over them
      dplyr::mutate(CATCH_WT_CAL = sum(CATCH_WT_B_CAL), # Bigelow + all calibrations applied
                    CATCH_NO_CAL = sum(CATCH_NO_B_CAL)) 
  }
  
  
  # Mimic processing used to generate STOCKEFF.V_SV_STRAT_SUMMARY_O view
  all_tows <- full_join(catch_cal, stations) %>% # Join station metadata with calibrated data so both positive and zero tows characterized
    mutate(CATCH_WT_CAL = case_when(is.na(CATCH_WT_CAL) == TRUE ~ 0,  # Replace NAs (stations that didn't see species of interest) with zeros
                                    .default = CATCH_WT_CAL),
           CATCH_NO_CAL = case_when(is.na(CATCH_NO_CAL) == TRUE ~ 0,
                                    .default = CATCH_NO_CAL)) 
  
  # Add STRATUM_AREA required to Mimic processing used to generate STOCKEFF.V_SV_STRAT_IND_O view
  all_tows <-  left_join(all_tows, strata_areas, by = "STRATUM") 
  
  # End IndexType == Default if statement
  ##### Custom index tow data #####
  } else if(IndexType == "Custom"){
    all_tows <- surveyTowData 
  } 
  
  ##### Index calculations #####
  strata_means <- all_tows %>%
    group_by(COMMON_NAME,
             SPECIES_ITIS,
             STOCK_ABBREV,
             PURPOSE_CODE,
             YEAR,
             SEASON,
             CRUISE6,
             STRATUM,
             SEX_TYPE,
             STRATUM_AREA) %>%
    add_tally(name = "STATIONS") %>% # Count number of stations per strata
    dplyr::reframe(STATIONS = STATIONS,
                   CATCH_WT = mean(CATCH_WT_CAL), # Average catch weight
                   CATCH_NO = mean(CATCH_NO_CAL), # Average catch numbers
                   CATCH_WT_VAR = var(CATCH_WT_CAL)/STATIONS,
                   CATCH_WT_STDERROR = sqrt(CATCH_WT_VAR),
                   CATCH_WT_CV = CATCH_WT_STDERROR/CATCH_WT,
                   CATCH_NO_VAR = var(CATCH_NO_CAL)/STATIONS,
                   CATCH_NO_STDERROR = sqrt(CATCH_NO_VAR),
                   CATCH_NO_CV = CATCH_NO_STDERROR/CATCH_NO) %>% # variance, std error and CV by stratum/season/year for catch weight and numbers
    distinct() %>%
    mutate(CATCH_WT_CV = case_when(is.nan(CATCH_WT_CV) ~ NA, # If CV can't be calculated (e.g. no stations in a stratum) set to NA
                                   .default = CATCH_WT_CV),
           CATCH_NO_CV = case_when(is.nan(CATCH_NO_CV) ~ NA, # If CV can't be calculated (e.g. no stations in a stratum) set to NA
                                   .default = CATCH_NO_CV))
  
  if(IndexType == "Default"){
    ###### Check strata means ######
    # If strata specific CATCH_WT and CATCH_NO match to 4 digits, if yes proceed without issue, if not proceed with caution an confirm that differences are small (i.e. likely due to rounding differences but worth checking) 
    # Rounding differences more prevalent in var, stderror, cv so not part of check
    
    ref_dim <- dim(V_SV_STRAT_SUMMARY_O)
    check_strata_means <- strata_means %>% # Round to 3 digits to compare values without rounding errors being an issue
      mutate(CATCH_WT = round(CATCH_WT,4),
             CATCH_WT_VAR = round(CATCH_WT_VAR,4),
             CATCH_WT_STDERROR = round(CATCH_WT_STDERROR,4),
             CATCH_WT_CV = round(CATCH_WT_CV,4),
             CATCH_NO = round(CATCH_NO,4),
             CATCH_NO_VAR = round(CATCH_NO_VAR,4),
             CATCH_NO_STDERROR = round(CATCH_NO_STDERROR,4),
             CATCH_NO_CV = round(CATCH_NO_CV,4))
    check_V_SV_STRAT_SUMMARY_O <- V_SV_STRAT_SUMMARY_O %>% DataExplorer::drop_columns(c("STATIONS_TEMP_VALUES", "STATIONS_POS_CATCH")) %>% # Remove columns not calculated in this code
      mutate(CATCH_WT = round(CATCH_WT,4),
             CATCH_WT_VAR = round(CATCH_WT_VAR,4),
             CATCH_WT_STDERROR = round(CATCH_WT_STDERROR,4),
             CATCH_WT_CV = round(CATCH_WT_CV,4),
             CATCH_NO = round(CATCH_NO,4),
             CATCH_NO_VAR = round(CATCH_NO_VAR,4),
             CATCH_NO_STDERROR = round(CATCH_NO_STDERROR,4),
             CATCH_NO_CV = round(CATCH_NO_CV,4)) 
    check_dim <- full_join(check_V_SV_STRAT_SUMMARY_O, check_strata_means, by = c("COMMON_NAME", "SPECIES_ITIS", "STOCK_ABBREV", "PURPOSE_CODE", "YEAR", "SEASON", "CRUISE6", "STRATUM", "SEX_TYPE", "STATIONS", "CATCH_WT",  "CATCH_NO")) %>%
      # group_by(YEAR,SEASON,STRATUM) %>%
      # add_tally() %>% filter(n > 1) %>%
      # arrange(YEAR,SEASON,STRATUM)
      # filter(YEAR == 1988, SEASON == "FALL", STRATUM == "01220")
      dim()
    
    if(check_dim[1] == ref_dim[1]){
      pass_strata_check <- TRUE
      check_strata <- NA
      strata_diff <- NA
    } else{
      pass_strata_check <- FALSE
      check_strata <- full_join(check_V_SV_STRAT_SUMMARY_O, check_strata_means, by = c("COMMON_NAME", "SPECIES_ITIS", "STOCK_ABBREV", "PURPOSE_CODE", "YEAR", "SEASON", "CRUISE6", "STRATUM", "SEX_TYPE", "STATIONS", "CATCH_WT",  "CATCH_NO")) %>%
        group_by(YEAR,SEASON,STRATUM) %>%
        add_tally() %>% filter(n > 1) %>% # Select rows where there is a mismatch between stockEff calculation of CATCH_NO or CATCH_WT compared to these calculations
        arrange(YEAR,SEASON,STRATUM)
      strata_diff <- check_strata %>% 
        group_by(COMMON_NAME, SPECIES_ITIS, STOCK_ABBREV, PURPOSE_CODE, YEAR, SEASON, CRUISE6, STRATUM, STATIONS, SEX_TYPE) %>% 
        dplyr::reframe(spread_WT = range(CATCH_WT)[2]-range(CATCH_WT)[1],
                       spread_NO = range(CATCH_NO)[2]-range(CATCH_NO)[1])
    }
    
  } # End strata mean check for IndexType == "Default
  
  
  
  ###### Index calculations ######
  # Mimic processing used to generate STOCKEFF.V_SV_STRAT_IND_O view which is used to populate the final strat_mean.csv product available through the web interface
  indices <- strata_means %>% 
    #filter(YEAR == 1982, SEASON == "FALL") %>% 
    mutate(CATCH_WT_EXP = CATCH_WT*STRATUM_AREA,
           CATCH_NO_EXP = CATCH_NO*STRATUM_AREA,
           CATCH_WT_EXP_VAR = STRATUM_AREA^2*CATCH_WT_VAR, # NA if CATCH_WT_VAR or CATCH_NO_VAR not calculated because only 1 station sampled
           CATCH_NO_EXP_VAR = STRATUM_AREA^2*CATCH_NO_VAR) %>%
    group_by(COMMON_NAME, SPECIES_ITIS,  STOCK_ABBREV, YEAR, SEASON, PURPOSE_CODE, CRUISE6) %>%
    dplyr::reframe(TOTAL_SAMP_AREA = sum(STRATUM_AREA), # stratum 18 not sampled in spring 1981, 1990 so total area smaller in these years
                   STRAT_MEAN_WT = sum(CATCH_WT_EXP)/TOTAL_SAMP_AREA,
                   STRAT_MEAN_WT_VAR = sum(CATCH_WT_EXP_VAR, na.rm = TRUE)/(TOTAL_SAMP_AREA^2),
                   STRAT_MEAN_WT_STDERROR = sqrt(STRAT_MEAN_WT_VAR),
                   STRAT_MEAN_WT_CV = STRAT_MEAN_WT_STDERROR/STRAT_MEAN_WT,
                   STRAT_MEAN_NO = sum(CATCH_NO_EXP)/TOTAL_SAMP_AREA,
                   STRAT_MEAN_NO_VAR = sum(CATCH_NO_EXP_VAR, na.rm = TRUE)/(TOTAL_SAMP_AREA^2),
                   STRAT_MEAN_NO_STDERROR = sqrt(STRAT_MEAN_NO_VAR),
                   STRAT_MEAN_NO_CV = STRAT_MEAN_NO_STDERROR/STRAT_MEAN_NO) 
  
  # Check if sampling complete (assumes all strata sampled in at least one year)
  indices <- indices %>% 
    group_by(PURPOSE_CODE) %>% mutate(max_SAMP_AREA = max(TOTAL_SAMP_AREA)) %>% # Group by PURPOSE_CODE (survey, e.g. 10 = NEFSC bottom trawl) to calculate total sampling area for that survey
    mutate(STRATA_SAMPLING_COMPLETE = case_when(TOTAL_SAMP_AREA == max_SAMP_AREA ~ "Y",
                                                .default = "N"))
  
  if(IndexType == "Default"){
    ###### Check index ######
    # Check if stratified mean index matches V_SV_STRAT_IND_O product
    stockEff_check <-  V_SV_STRAT_IND_O %>%
      DataExplorer::drop_columns("SEX_TYPE") %>% # Drop columns not needed for comparison check
      mutate(STRAT_MEAN_WT = round(STRAT_MEAN_WT,2),
             STRAT_MEAN_WT_VAR = round(STRAT_MEAN_WT_VAR,2),
             STRAT_MEAN_WT_STDERROR = round(STRAT_MEAN_WT_STDERROR,2),
             STRAT_MEAN_WT_CV = round(STRAT_MEAN_WT_CV,2),
             STRAT_MEAN_NO = round(STRAT_MEAN_NO,2),
             STRAT_MEAN_NO_VAR = round(STRAT_MEAN_NO_VAR,2),
             STRAT_MEAN_NO_STDERROR = round(STRAT_MEAN_NO_STDERROR,2),
             STRAT_MEAN_NO_CV = round(STRAT_MEAN_NO_CV,2))
    ref_dim_index <- dim(stockEff_check)
    
    # Compare to calculated indices
    index_check <- indices %>% ungroup() %>% 
      mutate(STRAT_MEAN_WT = round(STRAT_MEAN_WT,2),
             STRAT_MEAN_WT_VAR = round(STRAT_MEAN_WT_VAR,2),
             STRAT_MEAN_WT_STDERROR = round(STRAT_MEAN_WT_STDERROR,2),
             STRAT_MEAN_WT_CV = round(STRAT_MEAN_WT_CV,2),
             STRAT_MEAN_NO = round(STRAT_MEAN_NO,2),
             STRAT_MEAN_NO_VAR = round(STRAT_MEAN_NO_VAR,2),
             STRAT_MEAN_NO_STDERROR = round(STRAT_MEAN_NO_STDERROR,2),
             STRAT_MEAN_NO_CV = round(STRAT_MEAN_NO_CV,2)) %>%
      DataExplorer::drop_columns("TOTAL_SAMP_AREA")  # Drop columns not in stockEff backend table, not needed for comparison check
    
    check_dim_index <- full_join(index_check, stockEff_check, by = c("COMMON_NAME", "SPECIES_ITIS", "STOCK_ABBREV", "PURPOSE_CODE", "YEAR", "SEASON", "CRUISE6", "STRATA_SAMPLING_COMPLETE",
                                                                   "STRAT_MEAN_WT", "STRAT_MEAN_WT_VAR", "STRAT_MEAN_WT_STDERROR", "STRAT_MEAN_WT_CV", 
                                                                   "STRAT_MEAN_NO", "STRAT_MEAN_NO_VAR", "STRAT_MEAN_NO_STDERROR", "STRAT_MEAN_NO_CV")) %>%
      # group_by(YEAR,SEASON) %>%
      # add_tally() %>% filter(n > 1) %>% # Select rows where there is a mismatch between stockEff calculation of CATCH_NO or CATCH_WT compared to these calculations
      # arrange(YEAR,SEASON)
      dim()
    
    if(check_dim_index[1] == ref_dim_index[1]){ 
      pass_index_check <- TRUE
      check_index <- NA
      index_diff <- NA
    } else{
      pass_index_check <- FALSE
      check_index <- full_join(index_check, stockEff_check, by = c("COMMON_NAME", "SPECIES_ITIS", "STOCK_ABBREV", "PURPOSE_CODE", "YEAR", "SEASON", "CRUISE6", "STRATA_SAMPLING_COMPLETE",
                                                                 "STRAT_MEAN_WT", "STRAT_MEAN_WT_VAR", "STRAT_MEAN_WT_STDERROR", "STRAT_MEAN_WT_CV", 
                                                                 "STRAT_MEAN_NO", "STRAT_MEAN_NO_VAR", "STRAT_MEAN_NO_STDERROR", "STRAT_MEAN_NO_CV")) %>%
        group_by(YEAR,SEASON, PURPOSE_CODE, CRUISE6) %>%
        add_tally() %>% filter(n > 1) %>% # Select rows where there is a mismatch between stockEff calculation of CATCH_NO or CATCH_WT compared to these calculations
        arrange(YEAR,SEASON)
      index_diff <- check_index %>% 
        group_by(COMMON_NAME, SPECIES_ITIS, STOCK_ABBREV, PURPOSE_CODE, YEAR, SEASON, CRUISE6) %>% 
        dplyr::reframe(spread_MEAN_WT = range(STRAT_MEAN_WT)[2]-range(STRAT_MEAN_WT)[1],
                       spread_WT_VAR = range(STRAT_MEAN_WT_VAR)[2]-range(STRAT_MEAN_WT_VAR)[1],
                       spread_WT_STDERROR = range(STRAT_MEAN_WT_STDERROR)[2]-range(STRAT_MEAN_WT_STDERROR)[1],
                       spread_WT_CV = range(STRAT_MEAN_WT_CV)[2]-range(STRAT_MEAN_WT_CV)[1],
                       spread_MEAN_NO = range(STRAT_MEAN_NO)[2]-range(STRAT_MEAN_NO)[1],
                       spread_NO_VAR = range(STRAT_MEAN_NO_VAR)[2]-range(STRAT_MEAN_NO_VAR)[1],
                       spread_NO_STDERROR = range(STRAT_MEAN_NO_STDERROR)[2]-range(STRAT_MEAN_NO_STDERROR)[1],
                       spread_NO_CV = range(STRAT_MEAN_NO_CV)[2]-range(STRAT_MEAN_NO_CV)[1])
    }
  }  # End index check for IndexType == "Default"
  
  
  ##### Return #####
  returnList <- NULL
  returnList$indices <- indices # Stratified mean index calculated by weight (WT) and numbers (NO) for specified species, year and season
  
  if(is.null(summary_strata) == TRUE){
    returnList$strata_means_summary <- strata_means
  } else{ # If summary_strata provided, only save strata_means for specified strata to save on storage
    returnList$strata_means_summary <- strata_means %>% filter(STRATUM %in% summary_strata)
  } 
  
  if(IndexType == "Default"){
    # save all_tows as an output when default used so an example record of all tows used in the default calculation is provided which can be subset to populate a custom surveyTowData
    returnList$all_tows <- all_tows 
    
    # Store checks for strata means
    returnList$pass_strata_check = pass_strata_check
    returnList$check_strata = check_strata # NA if pass_strata_check == TRUE (i.e. no difference between these calculations and stockEff product)
    returnList$strata_diff = strata_diff # NA if pass_strata_check == TRUE (i.e. no difference between these calculations and stockEff product)
    
    # Store checks for index
    returnList$pass_index_check = pass_index_check
    returnList$check_index = check_index # NA if pass_index_check == TRUE (i.e. no difference between these calculations and stockEff product)
    returnList$index_diff = index_diff # NA if pass_index_check == TRUE (i.e. no difference between these calculations and stockEff product)
  }
  
  return(returnList)
}


