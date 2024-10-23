#' @title Fill OB specification template for stockEff
#' 
#' @param species_itis 6 digit ITIS identifying the species for which data should be pulled, no default.
#' @param stock_abbrev A string describing the stock abbreviation assigned by stock efficiency (this shows up in the web address for STOCKEFF products), no default.          
#' @param assessment_abbrev A string describing the assessment abbreviation, no default. Common options include "MT", "RT", "OA"
#' @param assessment_year The year in which the assessment will be reviewed, no default.
#' @param data_years The range of years for which discard data should be formatted for stockEff OB excel template, no default.
#' @param sex_type A string describing the sex type for which data should be pulled (this shows up in the web address for STOCKEFF products), default = "NONE". Common options include "NONE", "MALE", "FEMALE", "UNSEXED" 
#' 
#' @param tab_blocks A list providing the following information to populate the BLOCKS tab: (NOTE: LW_ID is populated by identifying the LW whose associated start and end months completely overlap with the block, otherwise an NA is returned)
#' \itemize{
#   \item{dropMarketYr - A table containing the nespp4 market category ("NESPP4" column) to "roll up" (i.e. drop) in the corrsponding year column("YEAR"), if provided landings are redistributed to other categories for that year/region (see "roll up" in Adding and Updating Stocks in StockEff documentation)}
#'   \item{block_opt - A string for the following options to specify blocks for each market category and year: 
#'           if = NULL (default), one block for every L-W relationship in tab_lw, 
#'           if = "MONTH", one block for each month (useful for checking minimum number of lengths exceeded for larger blocks), 
#'           if = "QUARTER", one block for each quarter,
#'           if = "SEMESTER", one block for each semester,
#'           if = "ANNUAL", one block per year for each market category
#'           if = "checkLENGTHS", Assumes monthly blocks were submitted to stockEff, checks the number of monthly length samples from biosamp_summary.csv to determine smallest block supported by data for each market category and year. Must also provide minLenSample argument if this option is used.}
#'   \item{minLenSample - A number specifying the minimum number of length samples required to support a given block for a year and gear, only required if tab_blocks$block_opt == "checkLENGTHS", if used then code checks for all data_years}
#'   \item{autofillLW - A boolean, if TRUE fills LW_ID based on complete overlap with LW months and years, if FALSE leaves NAs for blocks without a perfect match to LW month and year definition, default = FALSE}
#'   \item{check_missing - A boolean, if TRUE check for years in data_years with missing regions}
#' }
#' @param tab_prorate A list providing the following information to populate the PRORATE tab:
#' \itemize{
#'   \item{turnOff - A vector of years for which unclassified landings should NOT be prorated, default prorates for all data_years.}
#' }
#' @param tab_regions A list providing the following information to populate the REGIONS tab: (NOTE: code is less well tested with >1 region)
#' \itemize{
#'   \item{stat_areas - A list of stat area vectors by region, no default. Example: list(c(510, 511), c(512, 513)) assumes 2 regions each containing 2 stat areas}
#'   \item{area_fished - A list of area fished by stat area region, no default. Example: list(c("GOMGB"), c("SNE)) assumes 2 areas fished for the regions defined in stat_areas example}
#'   \item{region_id !!! tells you what region id(s) assigned to each area_fished}
#'   \item{gear_area_fished !!! tells you what NESPP4_FLT gear codes are associated with each region_id for each area fished}
#'   \item{survey_strata - A list of survey strata by region, no default. Used to connect area_fished to survey strata for SVAGE_DEFINITION tab Example: list(c(01130, 01140), c(01380, 01390, 01400))} assumes 2 regions with different survey strata assigned to each
#'   \item{region_custom - A optional custom matrix containing "REGION_ID", "AREA", and "YEAR" columns, not required if stat_areas object provided !!! does not currently work but could be built out in future}
#' }
#' @param tab_lw A matrix or data.frame containing the following named columns of information for each LW data source. Currently only supports blocks with types, start and end months that match LW_PARAMS and are consistent across all years
#' \itemize{
#'   \item{ALPHA - Alpha parameter for each LW relationship}
#'   \item{BETA - Beta parameter for each LW relationship}
#'   \item{SOURCE - Description of LW parameter and link to source document}
#'   \item{LW_TYPE - Type of LW relationship, options can include "SEMESTER", "ANNUAL", "CUSTOM"}
#'   \item{LW_ID - ID for each row of the table}
#'   \item{MONTH_START - First month that LW_ID row may be used, processed in tab_BLOCKS and ultimately dropped from tab_LW_PARAMS}
#'   \item{MONTH_END - Last month that LW_ID row may be used, processed in tab_BLOCKS and ultimately dropped from tab_LW_PARAMS}
#'   \item{YEAR_START - First year that LW_ID row may be used, processed in tab_BLOCKS and ultimately dropped from tab_LW_PARAMS}
#'   \item{YEAR_END - Last year that LW_ID row may be used, processed in tab_BLOCKS and ultimately dropped from tab_LW_PARAMS}
#'   \item{BLOCK_ID - Block ID associated with each LW equation, ultimately dropped from tab_LW_PARAMS (e.g. seasonal SPRING LW = block 1, FALL LW = 2, but an annual LW with only one time block = 1)}
#' }
#' @param tab_sex A list providing the following information to populate the SEX_TYPE tab:
#' \itemize{
#'   \item{start_yr - The start year for OB data, default = first year in data_years.}
#'   \item{end_yr - The end year for OB data, default = last year in data_years.}
#' }
#' @param tab_nespp4_flt A named list of "pre_CAMS" and "post_CAMS" settings providing lists of the following information to populate the NESPP4_FLT tab for each pre/post CAMS area:
#' \itemize{
#'   \item{nespp4_FLT - A vector of market categories corresponding to fleets (as done in BIOSTAT) to include in block and region tabs. No default.}
#'   \item{gear_group - A list of gear groups (stockEff or CAMS) included in each fleet definition (if multiple gear group codes included in a single fleet definition then these should be grouped into a vector and will have a separate line in NESPP4_FLT tab tieing it into the group). E.g. list(058, c(050, 0_trawlbot, 057)) would reflect 2 fleets with 3 gear groups combined in the second fleet}
#'   \item{NEGEAR_group - A list of NEGEAR codes that are included in each of the nespp4_FLT defintions (only required if tab_blocks$block_opt == "checkLENGTHS" to query OBDBS lengths). E.g. for the above gear_group example, the corresponding NEGEAR_group argument should be: list(058, c(050, 353, 057, 051, 054, 055, 056, 059, 150, 350, 351)) based on CAMS gear groups}
#'   \item{meshsize_abbrev - A vector or mesh size abbreviations corresponding to the gear group (if not a trawl gear set to "ALL"), common abbreviation are "LG", "SM", "ALL" (i.e. each unique combination of gear_group and meshsize_abbrev represents a unique gear. nespp4_FLT and nespp4_FLT_DESC are labels assigned to gear for accounting purposes as in BIOSTAT)}
#'   \item{oc - Open/closed trip designation for scallop trips, default = "ALL" (other fleets would be considered open which is included in "ALL")}
#'   \item{trp - Trip access type for scallop trips, options include "GEN", "LIM" and "ALL", default = "ALL"}
#'   \item{start_year - Start year for calculations, no default. "post_CAMS" can start in 2018 but may start later if discards manually processed during CAMS transition}
#'   \item{end_year - End year for calculations no default. Generally the end year for "post_CAMS" data is the terminal year of the assessment}
#'   \item{nespp4_FLT_DESC - A vector of fleet descriptions corresponding to nespp4_FLT}
#' }
#' @param tab_age_data_source A list providing the following information to populate the AGE_DATA_SOURCE tab:
#' \itemize{
#'   \item{YEAR - Vector of years for which age data source provided, likely match data_years argument}
#'   \item{AGE_DATA_SOURCE - A vector of age data source corresponding to YEAR, options include "SVDBS" for survey ages (used for pre-CAMS years), "CFAGEAA" for commercial AA tables, "CAMSAGE" to pull ages directly from CAMS}
#' }
#' @param outfile A string for the final exported file name, does NOT need to include .xlsx extention. No default.
#' 
#' NOTE: tab_blocks$block_opt = checkLENGTHS will NOT work unless connected to the VPN
#' NOTE: ALK_HOLES, EXCLUSIONS, and LENGTH_IMPUTATIONS tabs are not populated by this function but placeholders are put in the resulting output file so that they can be filled externally as needed.
#' NOTE: If you receive the following error, then the .xlsx file by the provided name already exists and can't be overwritten (generally happens when the file is open): [ERROR] workbook_close(): Error creating 'outfile.xlsx'. System error = Permission denied Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.'
#' NOTE: Code assumes that all gears fish in all areas when setting up the NESPP4_FLT and SVAGE_DEFINITION tabs, if this is not the case then invalid area/gear combinations can be filtered out of the final table and re-saved manually (REGIONS, NESPP4_FLT, SVAGE_DEFINITION tabs would need to be manually adjusted)
#' NOTE: Code assumes one region for each area fished, if this is not the case then region can be manually updated in the following tabs: BLOCKS, REGIONS, AGE_DATA_SOURCE
#' NOTE: The following warning may indicate different lengths for nespp4_flt$nespp4_FLT, nespp4_flt$gear_group, nespp4_flt$NEGEAR_group, nespp4_flt$meshsize_abbrev, or nespp4_flt$nespp4_FLT_DESC objects: 
#'       Warning message: In cbind(SPECIES_ITIS = as.character(species_itis), STOCK_ABBREV = stock_abbrev,  :
#'       number of rows of result is not a multiple of vector length

#' NOTE: If you find NAs in the REGION_ID for the BLOCKS tab, then there was a stat area in the stockeff_pre_prod.mv_cf_stock_data_length_o product that was not provided in the tab_regions$stat_areas argument.
#'
#'
#'
#' @return A list containing the following:
#' \itemize{
#'   \item{regions_missing - Table of AREAs that appear in oracle data but not in tab_regions$stat_area argument for this function (may be missing blocking)}
#' }

library(tidyverse)
library(DataExplorer)
library(writexl)
library(DBI)



fillOB <- function(species_itis = NULL,
                   stock_abbrev = NULL, 
                   assessment_abbrev = NULL,
                   assessment_year = NULL,
                   data_years = NULL,
                   sex_type = "NONE",
                   tab_blocks = list(autofillLW = FALSE,
                                     check_missing = FALSE),
                   tab_prorate = NULL,
                   tab_regions = NULL,
                   tab_lw = NULL,
                   tab_sex = NULL,
                   tab_nespp4_flt = list(pre_CAMS = NULL,
                                         post_CAMS = NULL),
                   tab_svage_def = NULL,
                   tab_age_data_source = NULL,
                   outfile = NULL){
  
  
  
  # Set default list arguments that are not NULL 
  if(is.null(tab_blocks$autofillLW)){ # If setting is not provided, ensure that the default is FALSE
    tab_blocks$autofillLW = FALSE
  }
  if(is.null(tab_blocks$check_missing) == TRUE){ # If setting is not provided, ensure that the default is FALSE
    tab_blocks$check_missing = FALSE
  }
  
  # PRORATE tab
  prorate <- rep("Y", length(data_years)) %>% cbind(data_years)
  colnames(prorate) <- c("PRORATE_COMBINED", "YEAR")
  if(is.null(tab_prorate$turnOff) == FALSE){
    prorate[which(prorate[,"YEAR"] %in% tab_prorate$turnOff),"PRORATE_COMBINED"] <- "N" # Don't prorate specified years
  }
  
  tab_PRORATE <- cbind(SPECIES_ITIS = species_itis,
                       STOCK_ABBREV = stock_abbrev,
                       SEX_TYPE = sex_type,
                       ASSESSMENT_ABBREV = assessment_abbrev,
                       SA_YEAR = assessment_year, 
                       YEAR = prorate[,"YEAR"],
                       PRORATE_COMBINED = prorate[,"PRORATE_COMBINED"])
  
  # NESPP4_FLT tab
  tab_NESPP4_FLT <- NULL
  for(icams in 1:2){ # Loop over pre- and post-CAMS gear definitions
    if(is.null(tab_nespp4_flt[[icams]]$oc)){ # Set defaults if not provided
      tab_nespp4_flt[[icams]]$oc = "ALL"
    }
    if(is.null(tab_nespp4_flt[[icams]]$trp)){
      tab_nespp4_flt[[icams]]$trp = "ALL"
    }
    
    for(ifleet in 1:length(tab_nespp4_flt[[icams]]$gear_group)){ # Loop over each gear group
      temp <- cbind(SPECIES_ITIS = as.character(species_itis),
                    STOCK_ABBREV = stock_abbrev,
                    SEX_TYPE = sex_type,
                    ASSESSMENT_ABBREV = assessment_abbrev,
                    SA_YEAR = assessment_year,
                    NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT[[ifleet]],
                    GEAR_GROUP = tab_nespp4_flt[[icams]]$gear_group[[ifleet]],
                    MESHSIZE_ABBREV = tab_nespp4_flt[[icams]]$meshsize_abbrev[[ifleet]],
                    OC = tab_nespp4_flt[[icams]]$oc,
                    TRP = tab_nespp4_flt[[icams]]$trp,
                    START_YEAR = tab_nespp4_flt[[icams]]$start_year,
                    END_YEAR = tab_nespp4_flt[[icams]]$end_year,
                    NESPP4_FLT_DESC = tab_nespp4_flt[[icams]]$nespp4_FLT_DESC[[ifleet]]) %>% as.data.frame() %>%
        full_join(., as.data.frame(expand.grid(SPECIES_ITIS = as.character(species_itis), YEAR = (tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year), AREA_FISHED = unique(unlist(tab_regions$area_fished)))), relationship = "many-to-many") %>% # Assumes all gears fish in all areas
        arrange(YEAR) %>% 
        relocate(YEAR, .before = NESPP4_FLT) %>%
        relocate(AREA_FISHED, .before = GEAR_GROUP)
      
      tab_NESPP4_FLT <- rbind(tab_NESPP4_FLT, temp) # Append to tab
    } # End loop over fleet gear groupings
    
  } # End loop over pre/post-CAMS periods
  
  
  ##### REGIONS tab #####
  if(is.null(tab_regions$stat_area) == FALSE){ # Stat areas grouped by region (default option)
    AREA <- unlist(tab_regions$stat_areas)
    YEAR <- data_years
    area_fished <- tab_regions$area_fished
    
    # Tie stat area to area fished
    store_area <- NULL
    for(iregion in 1:length(tab_regions$stat_areas)){ # Set unique region ID tied to different stat_areas in each region and distinct area_fished
      temp <- cbind(AREA = tab_regions$stat_areas[[iregion]], AREA_FISHED = rep(tab_regions$area_fished[iregion], length(tab_regions$stat_areas[iregion])))
      store_area <- rbind(store_area, temp)
    }
    store_area <- store_area %>% as.data.frame() %>% mutate(AREA_FISHED = unlist(AREA_FISHED))
    
    # Tie region to area fished and NESPP4_FLT
    store_region <- NULL
    for(ifish in 1:length(tab_regions$area_fished)){ # Loop over all area fished
      for(idef in 1:length(tab_regions$region_id[[ifish]])){ # loop over all IDs assigned to a given area fished
        temp <- expand.grid(NESPP4_FLT = unlist(tab_regions$gear_area_fished[[ifish]][idef]), REGION_ID = tab_regions$region_id[[ifish]][idef], AREA_FISHED = rep(tab_regions$area_fished[[ifish]]))
        store_region <- rbind(store_region, temp)
      }
    }
    
    store_region <- store_region %>% as.data.frame()
    region_table <- full_join(store_area, store_region, by = "AREA_FISHED", relationship = "many-to-many") 
    
    #region_table <- cbind(AREA, REGION_ID, AREA_FISHED) %>% as.data.frame()
    
    tab_REGIONS <- NULL
    for(icams in 1:2){ # Loop over pre- and post-CAMS gear definitions
      temp_region <- expand.grid(YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year, 
                                 REGION_ID = unique(region_table$REGION_ID)) %>% 
        full_join(., region_table, by = "REGION_ID", relationship = "many-to-many") %>%
        cbind(SPECIES_ITIS = species_itis,
              STOCK_ABBREV = stock_abbrev,
              SEX_TYPE = sex_type,
              ASSESSMENT_ABBREV = assessment_abbrev,
              SA_YEAR = assessment_year) %>%
          as.data.frame() %>% 
        relocate(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, ASSESSMENT_ABBREV, SA_YEAR, YEAR, NESPP4_FLT,AREA, AREA_FISHED, REGION_ID) %>% #!!! need to test with multiple regions, code works but not sure if all region 1 should be together followed by all region 2 for subsequent processing
        arrange(YEAR,NESPP4_FLT)
        
      tab_REGIONS <- rbind(tab_REGIONS, temp_region) # Append region to tab
    } # End loop over pre/post CAMS period
    
  } #else if(is.null(tab_regions$region_custom) == FALSE){ # Custom region ID, stat area, year grouping provided - does not currently work
  #   tab_REGIONS <- cbind(SPECIES_ITIS = species_itis,
  #                        STOCK_ABBREV = stock_abbrev,
  #                        SEX_TYPE = sex_type,
  #                        ASSESSMENT_ABBREV = assessment_abbrev,
  #                        SA_YEAR = assessment_year, 
  #                        YEAR = tab_regions$region_custom[,"YEAR"]
  #                        NESPP4_FLT = tab_nespp4_flt$nes
  #                        REGION_ID = tab_regions$region_custom[,"REGION_ID"],
  #                        AREA = tab_regions$region_custom[,"AREA"],
  #                        )
  # }
  tab_REGIONS$AREA <- as.character(tab_REGIONS$AREA) # So data type match when setting up blocks
  
  
  ##### LW_PARAMS tab #####
  tab_LW_PARAMS <- cbind(SPECIES_ITIS = species_itis,
                         STOCK_ABBREV = stock_abbrev,
                         SEX_TYPE = sex_type,
                         ASSESSMENT_ABBREV = assessment_abbrev,
                         SA_YEAR = assessment_year, 
                         ALPHA = tab_lw[,"ALPHA"],
                         BETA = tab_lw[,"BETA"],
                         SOURCE = tab_lw[,"SOURCE"],
                         LW_TYPE = tab_lw[,"LW_TYPE"],
                         LW_ID = tab_lw[,"LW_ID"],
                         MONTH_START = tab_lw[,"MONTH_START"],
                         MONTH_END = tab_lw[,"MONTH_END"],
                         YEAR_START = tab_lw[,"YEAR_START"],
                         YEAR_END = tab_lw[,"YEAR_END"]) %>% 
    as.data.frame() 
  
  ##### BLOCKS tab #####
  store_tempBlock <- NULL # Used if block_opt == "checkLENGTHS"
  for(icams in 1:2){ # Loop over pre- and post-CAMS gear definitions for blocking
  if(tab_blocks$block_opt == "MONTH") { # Implement monthly blocks for each market category and year
    tempBlock <- expand.grid(YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year,
                             NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT,
                             #REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "CUSTOM",
                             MONTH_START = as.numeric(1:12)) %>% 
      mutate(MONTH_END = MONTH_START) %>% 
      left_join(., {tab_REGIONS %>% select(YEAR,NESPP4_FLT,REGION_ID) %>% distinct()}, by = c("NESPP4_FLT", "YEAR"), relationship = "many-to-many") %>%
      relocate(YEAR, NESPP4_FLT, REGION_ID, BLOCK_TYPE, MONTH_START, MONTH_END)
    store_tempBlock <- rbind(store_tempBlock, tempBlock)
  } else if(tab_blocks$block_opt == "QUARTER"){ # Implement quarterly blocks for each market category and year
    tempBlock <- expand.grid(YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year,
                             NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT,
                             #REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "QUARTER",
                             MONTH_START = c(1,4,7,10)) %>% 
      mutate(MONTH_END = MONTH_START+2) %>% 
      left_join(., {tab_REGIONS %>% select(YEAR,NESPP4_FLT,REGION_ID) %>% distinct()}, by = c("NESPP4_FLT", "YEAR"), relationship = "many-to-many") %>%
      relocate(YEAR, NESPP4_FLT, REGION_ID, BLOCK_TYPE, MONTH_START, MONTH_END)
    store_tempBlock <- rbind(store_tempBlock, tempBlock)
  } else if(tab_blocks$block_opt == "SEMESTER"){ # Implement semester blocks for each market category and year
    tempBlock <- expand.grid(YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year,
                             NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT,
                             #REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "SEMESTER",
                             MONTH_START = c(1,7)) %>% 
      mutate(MONTH_END = MONTH_START+5) %>% 
      left_join(., {tab_REGIONS %>% select(YEAR,NESPP4_FLT,REGION_ID) %>% distinct()}, by = c("NESPP4_FLT", "YEAR"), relationship = "many-to-many") %>%
      relocate(YEAR, NESPP4_FLT, REGION_ID, BLOCK_TYPE, MONTH_START, MONTH_END)
    store_tempBlock <- rbind(store_tempBlock, tempBlock)
  } else if(tab_blocks$block_opt == "ANNUAL"){ # Implement a single annual block for each market category and year
    tempBlock <- expand.grid(YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year,
                             NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT,
                             #REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "ANNUAL",
                             MONTH_START = c(1),
                             MONTH_END = c(12)) %>% 
      left_join(., {tab_REGIONS %>% select(YEAR,NESPP4_FLT,REGION_ID) %>% distinct()}, by = c("NESPP4_FLT", "YEAR"), relationship = "many-to-many") %>%
      relocate(YEAR, NESPP4_FLT, REGION_ID, BLOCK_TYPE, MONTH_START, MONTH_END) 
    store_tempBlock <- rbind(store_tempBlock, tempBlock)
  } else if(tab_blocks$block_opt == "checkLENGTHS" | tab_blocks$check_missing == TRUE){ 
    ## Read in ages and lengths if 1) blocking should be done based on length availability, or 2) code should check for missing market categories in data_years
    
    # Connect to oracle !!!!!!!!!!!!!! here to line 354 needs to be updated to pull from observer data rather than CF landings data
    if(exists("connection") == FALSE){ # Set up connection object if it doesn't otherwise exist in environment, prevents need to provide credentials multiple times as loop over pre-/post-CAMS periods
    connection <- dbConnect(drv = dbDriver("Oracle"),
                            username = rstudioapi::askForPassword("Oracle user name"), 
                            password = rstudioapi::askForPassword("Oracle password"),
                            dbname = rstudioapi::askForPassword("Oracle database name")) #!!! Maybe this doesn't work after changing oracle pw?
    }
    
    ## Pull lengths by species, stock, and year 
    # Pull combined observer and ASM data broken down by NESPP4_FLT
    checkLengths <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM STOCKEFF_PRE_PROD.MV_OB_STOCK_DATA_LENGTH_O WHERE species_itis = ", species_itis, " AND stock_abbrev = '", stock_abbrev,"'", " AND year >= ", data_years[1], " AND year <= ", data_years[length(data_years)]))
    
    checkLengths <- checkLengths %>% 
      filter(YEAR %in% tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year) %>%
      mutate(QTR = case_when(MONTH %in% c("01", "02", "03")~1,
                                                            MONTH %in% c("04", "05", "06")~2,
                                                            MONTH %in% c("07", "08", "09")~3,
                                                            MONTH %in% c("10", "11", "12")~4),
                                            SEM = case_when(QTR %in% c(1,2) ~ 1,
                                                            QTR %in% c(3,4) ~ 2)) %>% 
      mutate(QTR = as.numeric(QTR)) 
    checkLengths <- tab_REGIONS %>% select(YEAR, NESPP4_FLT, AREA_FISHED, REGION_ID) %>% distinct() %>% mutate(NESPP4_FLT = as.character(NESPP4_FLT)) %>%
      right_join(., checkLengths, by = c("YEAR", "NESPP4_FLT", "AREA_FISHED"))
    # # Pull observer data
    # nespp4_species <- tab_nespp4_flt$post_CAMS$nespp4_FLT[1] %>% substr(., 1,3) # Extract species nespp4 code from post-CAMS nespp4_flt names (i.e. first 3 numbers in code)
    # OBLengths <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM OBDBS.OBLEN WHERE NESPP4 LIKE '", nespp4_species, "%' AND YEAR >= ", data_years[1], " AND YEAR <= ", data_years[length(data_years)])) %>%
    #   #filter(NESPP4 %in% tab_blocks$nespp4) %>% # Filter to only selected market categories
    #   mutate(YEAR = as.numeric(YEAR), AREA = as.character(AREA)) %>% 
    #   left_join(.,dplyr::distinct(tab_REGIONS[,c("AREA", "REGION_ID", "YEAR")]), by = c("AREA", "YEAR")) %>% # Join data_years = 1989:2023lengths with region ID by area and year
    #   mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
    #                          QTR %in% c(3,4) ~ 2))
    # 
    # # Pull at sea monitoring (ASM) data
    # ASMLengths <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM OBDBS.ASMLEN WHERE NESPP4 LIKE '", nespp4_species, "%' AND YEAR >= ", data_years[1], " AND YEAR <= ", data_years[length(data_years)])) %>%
    #   mutate(YEAR = as.numeric(YEAR), AREA = as.character(AREA)) %>% 
    #   left_join(.,dplyr::distinct(tab_REGIONS[,c("AREA", "REGION_ID", "YEAR")]), by = c("AREA", "YEAR")) %>% # Join lengths with region ID by area and year
    #   mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
    #                          QTR %in% c(3,4) ~ 2))
    # 
    # checkLengths <- rbind(OBLengths, ASMLengths) # Use observer and at sea monitoring lengths
    # 
    # # CATDISP = 0s are discards, 1s are kept, 9 = unknown - need to filter by 0s
    # # LINK3 = link 1 + 4 digit/character haul number
    # # Mesh can change by haul number, need to pull mesh table and merge by link3 at haul level (only use for trawl gear and gillnet)
    # # Link1 = trip identifier, program, trip id, ect.
    # 
    # ## Merge mesh with checkLengths
    # # ID unique link 3 
    # uniq.link3 <- distinct(OBLengths, LINK3) %>% 
    #   pull(LINK3)
    # 
    # obs_gillnet_mesh <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM OBDBS.OBGGGH")) %>%
    #   select(YEAR, NEGEAR, LINK3, TIEDNUSD, MSWGTAVG) %>% # tiedowns, weighted avg of mesh size in inches
    #   filter(LINK3 %in% uniq.link3)
    # asm_gillnet_mesh <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM OBDBS.ASMGGGH")) %>%
    #   select(YEAR, NEGEAR, LINK3, TIEDNUSD, MSWGTAVG) %>%
    #   filter(LINK3 %in% uniq.link3) 
    # 
    # gillnet.mesh <- rbind(obs_gillnet_mesh, asm_gillnet_mesh) %>%
    #   mutate(Mesh = round(MSWGTAVG,1)) %>% select(YEAR, NEGEAR, LINK3, Mesh)
    # 
    # obs_ot_mesh <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM OBDBS.OBOTGH")) %>%
    #   select(YEAR, NEGEAR, LINK3, CODMSIZE, LINERMSIZE, CODLINERUSD) %>%
    #   filter(LINK3 %in% uniq.link3)
    # asm_ot_mesh <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM OBDBS.ASMOTGH")) %>%
    #   select(YEAR, NEGEAR, LINK3, CODMSIZE, LINERMSIZE, CODLINERUSD) %>%
    #   filter(LINK3 %in% uniq.link3)
    # 
    # trawl.mesh <- rbind(obs_ot_mesh, asm_ot_mesh) %>%
    #   mutate(TrawlMesh = if_else(is.na(LINERMSIZE)==FALSE & LINERMSIZE>0 & ( LINERMSIZE<CODMSIZE | is.na(CODMSIZE)==TRUE ), 
    #                              LINERMSIZE, # If liner in codend use it as long as it has a smaller mesh than the codend
    #                              CODMSIZE), # otherwise use the codend mesh size
    #          Mesh = round(TrawlMesh/25.4, 1)) %>% select(YEAR, NEGEAR, LINK3, Mesh)
    # 
    # 
    # all.mesh <- rbind(trawl.mesh, gillnet.mesh) %>% mutate(YEAR = as.numeric(YEAR)) 
    # 
    # # LINK3 = link 1 + 4 digit/character haul number
    # # Mesh can change by haul number, need to pull mesh table and merge by link3 at haul level (only use for trawl gear and gillnet)
    # 
    # # Link1 = trip identifier, program, trip id, ect.
    # 
    # # Finish merging information to group gears for checkLengths
    # checkLengths <- left_join(checkLengths, all.mesh, by = c("YEAR", "NEGEAR", "LINK3")) %>% # Merge all mesh size with checkLengths
    #   filter(CATDISP == 0) # CATDISP = 0s are discards, 1s are kept, 9 = unknown - need to filter by 0s
    #   
    

    
    # if(tab_blocks$check_missing == TRUE){ # Checks if region is missing for any years
    #   regions_missing <- checkLengths %>% filter(is.na(REGION_ID) == TRUE) # %>% select(YEAR, MONTH, QTR, AREA, LENGTH, NO_AT_LENGTH, TRIPID, HAULNUM, LINK1, LINK3, PROGRAM)
    #   # if(nrow(regions_missing) == 0){ # If no regions missing, also check for missing market categories !!! I don't think we need to check if a gear is missing in a given year???
    #   #   mkt_missing_yr <- expand.grid(YEAR = data_years,
    #   #                                 QTR = as.character(1:4),
    #   #                                 NESPP4 = as.character(tab_blocks$nespp4)) %>%
    #   #     #NESPP4 = as.character(paste0("0",tab_blocks$nespp4))) %>% 
    #   #     mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
    #   #                            QTR %in% c(3,4) ~ 2)) %>%
    #   #     left_join({checkLengths %>% group_by(YEAR, NESPP4) %>% summarise(LENGTHS_ANN = sum(NO_AT_LENGTH, na.rm = TRUE))}) %>% # NAs in LENGTHS_ANN when no lengths for that year/NESPP4 grouping
    #   #     group_by(YEAR, NESPP4) %>% 
    #   #     mutate(mktComplete = ifelse(any(is.na(LENGTHS_ANN) == TRUE), FALSE, TRUE)) %>% 
    #   #     filter(mktComplete == FALSE) %>% # Pull out market categories that are missing by year
    #   #     distinct(YEAR,NESPP4,LENGTHS_ANN,mktComplete) %>%
    #   #     arrange(YEAR)
    #   # } else{ # If region missing, return a warning and do not check for missing market categories
    #   #   mkt_missing_yr <- "Not calculated due to missing regions, check that tab_regions$stat_area input is complete."
    #   # }
    #   
    # } else{
    #   regions_missing <- "Not calculated since check_missing == FALSE"
    # }
    
    if(tab_blocks$block_opt == "checkLENGTHS"){
      
      # # Set up long form NEGEAR codes to merge with tab_NESPP4_FLT !!! need to add in the cutoff for small/large mesh to these definitions so can add check
      # NEGEAR_long <- NULL
      # for(icams in 1:2){
      #   for(igear in 1:length(tab_nespp4_flt[[icams]]$NEGEAR_group)){
      #     long_gear <- expand.grid(NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT[[igear]], NEGEAR = tab_nespp4_flt[[icams]]$NEGEAR_group[[igear]], YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year) %>% as.data.frame()
      #     NEGEAR_long <- rbind(NEGEAR_long, long_gear) # Append to storage
      #   } # End loop over gears
      # } # End loop over pre/post CAMS
      # 
      # # Combine checkLengths with NESPP4_FLT by gear code so grouping individual gears to match NESPP4_FLT definition
      # checkLengths <- checkLengths %>% 
      #   left_join(., NEGEAR_long, by = c("NEGEAR", "YEAR"), relationship = "many-to-many")
      #   
      # #   tab_NESPP4_FLT %>% 
      # #   select(SPECIES_ITIS, YEAR, NESPP4_FLT, MESHSIZE_ABBREV, OC, TRP) %>% dplyr::distinct() %>% #filter(YEAR >= 1989) %>% head() %>% # Will only have matches for years where both data sets available (e.g. 1989 onwards)
      # # right_join(., checkLengths, by = c("YEAR", "NEGEAR"), relationship = "many-to-many") # Only keep NESPP4_FLT gear information if this gear observed in a given year (e.g. if full_join gear 1242/058 has no information in 1999, also gaps prior to 1989 for all gears since no discard data prior to that year)
      # 
      # Check lengths
      defineBlocks <- expand.grid(YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year,
                                  QTR = as.numeric(1:4)) %>%
        # NESPP4 = paste0("0",tab_blocks$nespp4)) %>% 
        mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
                               QTR %in% c(3,4) ~ 2),
               QTR = as.numeric(QTR)) %>%
        left_join(., {tab_REGIONS %>% select(YEAR,NESPP4_FLT,AREA_FISHED,REGION_ID) %>% distinct()}, by = "YEAR", relationship = "many-to-many") %>%
        # Sum lengths by quarter for each gear
        left_join(.,{checkLengths %>% group_by(YEAR, NESPP4_FLT, QTR) %>% mutate(NESPP4_FLT = as.numeric(NESPP4_FLT)) %>% summarise(LENGTHS_QTR = sum(NO_AT_LENGTH))}, by = c("YEAR", "NESPP4_FLT", "QTR")) %>% # Can add AREA_FISHED here to grouping if we want to check lengths by quarter/gear/region and area fished otherwise check by quarter/gear/region, alternatively can change gear_area_fished and region_id specification so each area fished is its own region
        left_join({checkLengths %>% group_by(YEAR, NESPP4_FLT, SEM) %>% mutate(NESPP4_FLT = as.numeric(NESPP4_FLT)) %>% summarise(LENGTHS_SEM = sum(NO_AT_LENGTH))}) %>%
        left_join({checkLengths %>% group_by(YEAR, NESPP4_FLT) %>% mutate(NESPP4_FLT = as.numeric(NESPP4_FLT)) %>% summarise(LENGTHS_ANN = sum(NO_AT_LENGTH))}) %>%
        
        # Replace NAs with 0s used in checking complete market sampling
        mutate(LENGTHS_QTR = ifelse(is.na(LENGTHS_QTR), 0, LENGTHS_QTR),
               LENGTHS_SEM = ifelse(is.na(LENGTHS_SEM), 0, LENGTHS_SEM),
               LENGTHS_ANN = ifelse(is.na(LENGTHS_ANN), 0, LENGTHS_ANN)) %>%
      # defineBlocks <- tab_REGIONS %>% 
      #   filter(YEAR %in% tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year) %>% 
      #   select(YEAR, NESPP4_FLT, AREA_FISHED, REGION_ID) %>% 
      #   distinct() %>% 
      #   mutate(NESPP4_FLT = as.numeric(NESPP4_FLT)) %>%
      #   right_join(., defineBlocks, by = c("YEAR", "NESPP4_FLT", "AREA_FISHED")) %>%
      #   # group_by(YEAR) %>% mutate(mktComplete = ifelse(any(LENGTHS_ANN == 0), FALSE, TRUE)) %>% # Don't need since processing discards
      #   group_by(YEAR, NESPP4_FLT, SEM, QTR, REGION_ID) %>%
      #   distinct() %>%
      #   #dplyr::summarise(lengths_QTR = unique(LENGTHS_QTR), lengths_SEM = unique(LENGTHS_SEM), lengths_ANN = unique(LENGTHS_ANN), REGION_ID = unique(REGION_ID)) %>% # Collapse duplicates by month
        group_by(YEAR, NESPP4_FLT, REGION_ID) %>%
        summarise(diff_QTR = LENGTHS_QTR - tab_blocks$minLenSample, # Any difference >= 0 represents lengths above the minimum threshold
               diff_SEM = LENGTHS_SEM - tab_blocks$minLenSample,
               diff_ANN = LENGTHS_ANN - tab_blocks$minLenSample,
               QTR = QTR) %>%
        distinct() %>%
        group_by(YEAR,NESPP4_FLT,REGION_ID) %>%
        mutate(QTR_check = diff_QTR>= 0,
               SEM_check = diff_SEM >= 0,
               ANN_check = diff_ANN >= 0) %>%
        group_by(YEAR,NESPP4_FLT,REGION_ID) %>%
        summarise(BLOCK_TYPE = case_when(length(which(QTR_check == TRUE)) == 4 ~ "QUARTER",
                                         length(which(SEM_check == TRUE)) == 4 ~ "SEMESTER", # Check if both semesters greater than threshold, but since quarters there are 4 values instead of 2 (with 2 duplicates in each semester)
                                         TRUE %in% ANN_check ~ "ANNUAL", # If any individual quarter/semester has greater than the threshold then scale up to annual 
                                         .default = "Insufficient_Samples"))
        # summarise(test_block <- case_when((length(diff_QTR) >= 0) == 4 ~ "QUARTER"))
        #          length(unique(LENGTHS_QTR - tab_blocks$minLenSample) >= 0) %>%
        # # check if 
        # summarise(BLOCK_TYPE = case_when((sum(LENGTHS_QTR >= tab_blocks$minLenSample) == 4) ~ "QUARTER", # Assign block !!! TOO hard coded (won't work if more than one area fished)
        #                                  (sum(LENGTHS_SEM >= tab_blocks$minLenSample) == 4) ~ "SEMESTER", #!!1 doesn't work, lengths_SEM needs to check if sufficient lengths in both semester, currently if <4 quarters sampled then won't do annual blocking even if # of lengths is correct, need to add expand.grid at beginning like Charles' code did in order to guarantee 4 months of info
        #                                  (sum(LENGTHS_ANN >= tab_blocks$minLenSample) == 4) ~ "ANNUAL",
        #                                  .default = "Insufficient_Samples"), # Return "Insufficient_Samples if market category missing or annual number of samples < minLenSample
        #           REGION_ID = unique(REGION_ID)) # Retain for final tab_BLOCKS formatting
        #           LENGTHS_QTR = LENGTHS_QTR, # Can add in to see counts by year and block
        #           LENGTHS_SEM = LENGTHS_SEM,
        #           LENGTHS_ANN = LENGTHS_ANN) %>%
        # mutate(block_LENGTHS = case_when(BLOCK_TYPE == "QUARTER" ~ LENGTHS_QTR,
        #                                  BLOCK_TYPE == "SEMESTER" ~ LENGTHS_SEM,
        #                                  BLOCK_TYPE == "ANNUAL" ~ LENGTHS_ANN,
        #                                  BLOCK_TYPE == "Insufficient_Samples" ~ LENGTHS_ANN))
      
      quarters <- expand.grid(BLOCK_TYPE = "QUARTER",
                              MONTH_range = c("01_03", "04_06", "07_09", "10_12")) # Month range for each quarter
      semesters <- expand.grid(BLOCK_TYPE = "SEMESTER",
                               MONTH_range = c("01_06", "07_12")) # Month range for each semester
      annual <- expand.grid(BLOCK_TYPE = "ANNUAL",
                            MONTH_range = c("1_12"))
      months <- rbind(quarters, semesters, annual)
      
      # Expand starting and end month for each block type
      tempBlock <- left_join(defineBlocks, months, by = "BLOCK_TYPE", relationship = "many-to-many") %>% 
        separate_wider_delim(MONTH_range, delim = "_", names = c("MONTH_START", "MONTH_END")) %>% distinct() # %>% mutate(Cams = icams) #!!! can add optional Cams block here to make sure years treated by pre/post cams appropriately
      
    } # End statement to define checkLengths blocks
    
  } # End tab_blocks$opt and tab_blocks$check_missing statements
  
    if(tab_blocks$block_opt == "checkLENGTHS"){ # Done earlier for other options
      store_tempBlock <-  rbind(store_tempBlock, tempBlock) # need to keep only years in tempBlock that correspond to icams years
    }

} # End loop over pre- and post-CAMS gear definitions
  
  # Rename stored blocking so CAMS loop will run and subsequent code does not need changes (this step is not needed if block_opt != "checkLENGTHS")
  tempBlock <- NULL
  tempBlock <- store_tempBlock
  
  
  # Fill LW_ID using month and year range provide with each L-W relationship(if no option satisfied then return NA and must revisit by hand), if not-autofilled, only exact matches returned
  tempBlock$LW_ID <- rep(NA, nrow(tempBlock))
  
  noAnnual <- tab_LW_PARAMS %>% filter(LW_TYPE != "ANNUAL") # Handle separately, otherwise risk using ANNUAL L-W for all blocks
  if(nrow(noAnnual) !=0){
    
    # Auto-fill LW_ID using month and year range provide with each L-W relationship(if no option satisfied then return NA and must revisit by hand)
    if(tab_blocks$autofillLW == TRUE){
      # First fill exact matches
      for(iLW in 1:nrow(noAnnual)){
        tempBlock$LW_ID[which(as.numeric(tempBlock$MONTH_START) == as.numeric(noAnnual$MONTH_START[iLW]) &
                                as.numeric(tempBlock$MONTH_END) == as.numeric(noAnnual$MONTH_END[iLW]) &
                                as.numeric(tempBlock$YEAR) %in% noAnnual$YEAR_START[iLW]:noAnnual$YEAR_END[iLW])] <- noAnnual$LW_ID[iLW]
      }
      # Fill based on complete overlap with LW months, if no LW_IDs meet these conditions then the ID remains an NA (e.g. when insufficient samples)
      for(iLW in 1:nrow(noAnnual)){
        tempBlock$LW_ID[which(is.na(tempBlock$LW_ID) & # Of remaining rows with no LW_ID assigned, check the following
                                as.numeric(tempBlock$MONTH_START) >= as.numeric(noAnnual[iLW]) & # Check if months fully overlap with a LW definition: 
                                as.numeric(tempBlock$MONTH_START) <= as.numeric(noAnnual$MONTH_END[iLW]) & 
                                as.numeric(tempBlock$MONTH_END) <= as.numeric(noAnnual$MONTH_END[iLW]) &
                                as.numeric(tempBlock$YEAR) %in% noAnnual$YEAR_START[iLW]:noAnnual$YEAR_END[iLW])] <- noAnnual$LW_ID[iLW] # Check if year falls within LW definition, if yes assign that LW_ID
      }
    } else{ # if tab_blocks$autofillLW == FALSE, only return exact matches
      for(iLW in 1:nrow(noAnnual)){
        tempBlock$LW_ID[which(as.numeric(tempBlock$MONTH_START) == as.numeric(noAnnual$MONTH_START[iLW]) &
                                as.numeric(tempBlock$MONTH_END) == as.numeric(noAnnual$MONTH_END[iLW]) &
                                as.numeric(tempBlock$YEAR) %in% noAnnual$YEAR_START[iLW]:noAnnual$YEAR_END[iLW])] <- noAnnual$LW_ID[iLW]
      }
    }
  } else{ # End fill for LW_TYPE != "ANNUAL"
    warning("Only Annual LW parameters provided")
  }
  
  
  # Fill annual LW if available
  if("ANNUAL" %in% tab_LW_PARAMS$LW_TYPE){
    tempBlock$LW_ID[which(tempBlock$MONTH_START == "01" & tempBlock$MONTH_END == "12")] <- tab_LW_PARAMS$LW_ID[which(tab_LW_PARAMS$LW_TYPE == "ANNUAL")]
  }
  
  # Finish block formatting
  tab_BLOCKS <- tab_lw %>% 
    select(LW_ID, BLOCK_ID) %>% 
    mutate(LW_ID = as.character(LW_ID)) %>%  
    right_join(., tempBlock, by = "LW_ID", relationship = "many-to-many") %>% # Join with tempBlock to populate BLOCK_ID 
    #group_by(YEAR, NESPP4_FLT, REGION_ID, LW_ID) %>% 
    #mutate(BLOCK_ID = seq_along(YEAR)) %>% # Add the block ID numbers
    cbind(SPECIES_ITIS = species_itis,
          STOCK_ABBREV = stock_abbrev,
          SEX_TYPE = sex_type,
          ASSESSMENT_ABBREV = assessment_abbrev,
          SA_YEAR = assessment_year)  %>% as.data.frame() %>%
    relocate(c(YEAR, NESPP4_FLT, REGION_ID, BLOCK_ID, BLOCK_TYPE, MONTH_START, MONTH_END, LW_ID), .after = SA_YEAR) %>%
    arrange(YEAR,NESPP4_FLT) #!!! think this works but need to 1) add column for number of obs so info available, and 2) change region so not 1 per area fished (would need to change how area fished included in expand.grid for blocking)
    #group_by(YEAR, NESPP4_FLT,REGION_ID)

  

  # if(tab_blocks$check_missing == FALSE){ 
  #   regions_missing <- "Not calculated since check_missing == FALSE"
  # }
  
  ##### AGE_DATA_SOURCE tab #####
  tab_AGE_DATA_SOURCE <- tab_BLOCKS %>% select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, ASSESSMENT_ABBREV, SA_YEAR, YEAR, NESPP4_FLT, REGION_ID, BLOCK_ID) %>% 
    #tab_BLOCKS %>%
    as.data.frame() %>% 
    full_join(.,as.data.frame(tab_age_data_source), by = "YEAR") 
  
  ##### SEX_TYPE tab #####
  start <- head(data_years, n=1)
  end <- tail(data_years, n=1)
  if(is.null(tab_sex$start_yr) == FALSE){
    start <- tab_sex$start_yr
  }
  if(is.null(tab_sex$end_yr) == FALSE){
    end <- tab_sex$end_yr
  }
  
  tab_SEX_TYPE <- cbind(SPECIES_ITIS = species_itis,
                        STOCK_ABBREV = stock_abbrev,
                        ASSESSMENT_ABBREV = assessment_abbrev,
                        SA_YEAR = assessment_year,
                        OB_START_YEAR = start,
                        OB_END_YEAR = end,
                        SEX_TYPE = sex_type)
  
  
  ##### SVAGE_DEFINITION tab #####
  # Pull survey strata associated by each region and turn into table
  strata_table <- NULL
  for(iregion in 1:length(tab_regions$survey_strata)){ # Set area_fished for survey_strata
    strata_table <- rbind(strata_table, cbind(AREA_FISHED = rep(tab_regions$area_fished[[iregion]], length(tab_regions$survey_strata[[iregion]])), STRATUM = tab_regions$survey_strata[[iregion]]))
  }
  strata_table <- strata_table %>% as.data.frame()
  
  # Connect to oracle 
  if(exists("connection") == FALSE){
    connection <- dbConnect(drv = dbDriver("Oracle"),
                            username = rstudioapi::askForPassword("Oracle user name"), 
                            password = rstudioapi::askForPassword("Oracle password"),
                            dbname = rstudioapi::askForPassword("Oracle database name"))
  }
  
  
  # Pull age information by species, stock, and year
  svage_info <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM stockeff_pre_prod.I_SV_AGE_O WHERE species_itis = ", species_itis, " AND year >= ", data_years[1]))
  
  tab_SVAGE_DEFINITION <- NULL
  for(icams in 1:2){ # Loop over pre- and post-CAMS gear definitions
    svage <- expand.grid(PURPOSE_CODE = unique(svage_info$PURPOSE_CODE), SEASON = unique(svage_info$SEASON), STRATUM = unique(svage_info$STRATUM), 
                         YEAR = tab_nespp4_flt[[icams]]$start_year:tab_nespp4_flt[[icams]]$end_year, NESPP4_FLT = tab_nespp4_flt[[icams]]$nespp4_FLT) %>% # NESPP4 fleet definitions can vary between pre/post CAMS years, still assume that all fleets operate in all areas
      as.data.frame() %>%
      mutate(SPECIES_ITIS = species_itis, STOCK_ABBREV = stock_abbrev, SEX_TYPE = sex_type, ASSESSMENT_ABBREV = assessment_abbrev, SA_YEAR = assessment_year,
             MONTH = case_when(SEASON == "SPRING" ~ "01",
                               SEASON == "FALL" ~ "07",
                               .default = "NA")) # If season isn't spring/fall then no default month provided
    
    temp_svage <- full_join(svage, strata_table, by = "STRATUM", relationship = "many-to-many") %>%
      relocate(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, ASSESSMENT_ABBREV, SA_YEAR, YEAR, NESPP4_FLT, AREA_FISHED, PURPOSE_CODE, STRATUM, SEASON, MONTH)
    
    # Append rows for icams years to final tab
    tab_SVAGE_DEFINITION <- rbind(tab_SVAGE_DEFINITION, temp_svage)
  } # End loop over pre/post CAMS periods
  
  
  # Disconnect from database after pre-/post-CAMS loop so don't have to provide connection credentials multiple times
  dbDisconnect(connection)
  
  ##### ALK_HOLES tab - optional, can use stockEff auto-fill #####
  tab_ALK_HOLES <- data.frame(matrix(vector(), 0, 15, dimnames = list(c(), c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", "NESPP4_FLT", "REGION_ID", "BLOCK_ID", "LENGTH_UOM", "LENGTH", "AGE_UOM",
                                                                             "AGE", "NO_AT_AGE", "SOURCE"))),
                              stringsAsFactors = FALSE)
  
  ##### EXCLUSIONS tab - optional if some data should be excluded #####
  tab_EXCLUSIONS <- data.frame(matrix(vector(), 0, 22, dimnames = list(c(), c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", "NESPP4_OUTPUT", "AREA",
                                                                              "AREA_FISHED", "MONTH", "DAY", "FISHDISP", "CATDISP", "SEX", "LINK", "BIOSAMP_SOURCE", "LENGTH", "LENGTH_UOM",
                                                                              "AGESTRCT", "AGE", "AGE_UOM", "REASON"))),
                               stringsAsFactors = FALSE)
  
  ##### LENGTH_IMPUTATIONS tab - optional, provide imputations for market category/region/block/length if missing numbers at length #####
  tab_LENGTH_IMPUTATIONS <- data.frame(matrix(vector(), 0, 13, dimnames = list(c(), c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", "NESPP4_FLT", "REGION_ID",
                                                                                      "BLOCK_ID", "LENGTH_UOM", "LENGTH", "NO_AT_LENGTH", "SOURCE"))),
                                       stringsAsFactors = FALSE)
  
  
  
  ## Write tables to tabs of OB excel template
  
  # Remove temporary month and year start/end columns used to auto-assign LW relationships to blocks
  tab_LW_PARAMS <- tab_LW_PARAMS %>% as.data.frame() %>% drop_columns(c("MONTH_START", "MONTH_END", "YEAR_START", "YEAR_END"))  %>% as.data.frame()
  
  # Generate list of required tabs to output - unless otherwise denoted as optional placeholder, these are populated by this function
  write_list <- list("BLOCKS" = as.data.frame(tab_BLOCKS),
                     "PRORATE" = as.data.frame(tab_PRORATE),
                     "REGIONS" = as.data.frame(tab_REGIONS),
                     "LW_PARAMS" = tab_LW_PARAMS,
                     "SEX_TYPE" = as.data.frame(tab_SEX_TYPE),
                     "ALK_HOLES" = as.data.frame(tab_ALK_HOLES), # Placeholder tab that can be filled externally to this script as needed
                     "EXCLUSIONS" = as.data.frame(tab_EXCLUSIONS), # Placeholder tab that can be filled externally to this script as needed
                     "LENGTH_IMPUTATIONS" = as.data.frame(tab_LENGTH_IMPUTATIONS), # Placeholder tab that can be filled externally to this script as needed
                     "NESPP4_FLT" = as.data.frame(tab_NESPP4_FLT), 
                     "SVAGE_DEFINITION" = as.data.frame(tab_SVAGE_DEFINITION), 
                     "AGE_DATA_SOURCE" = as.data.frame(tab_AGE_DATA_SOURCE)) # Placeholder tab that can be filled externally to this script as needed
  
  # Actually write to template
  library(writexl)
  write_xlsx(write_list, paste0(outfile, ".xlsx")) # Drop information used in setting up tab_BLOCKS but not required in tab_LW_PARAMS before writing to template
  
  # Return
  # return_list <- list(regions_missing = regions_missing) # Table of AREAs that appear in oracle data but not in tab_regions$stat_area argument for this function (may be missing blocking)
  
  #return(return_list) 
}


