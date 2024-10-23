#' @title Fill CF specification template for stockEff
#' 
#' @param species_itis 6 digit ITIS identifying the species for which data should be pulled, no default.
#' @param stock_abbrev A string describing the stock abbreviation assigned by stock efficiency (this shows up in the web address for STOCKEFF products), no default.          
#' @param assessment_abbrev A string describing the assessment abbreviation, no default. Common options include "MT", "RT", "OA"
#' @param assessment_year The year in which the assessment will be reviewed, no default.
#' @param data_years The range of years for which catch data should be formatted for stockEff CF excel template, no default.
#' @param sex_type A string describing the sex type for which data should be pulled (this shows up in the web address for STOCKEFF products), default = "NONE". Common options include "NONE", "MALE", "FEMALE", "UNSEXED" 
#' 
#' @param tab_blocks A list providing the following information to populate the BLOCKS tab: (NOTE: LW_ID is populated by identifying the LW whose associated start and end months completely overlap with the block, otherwise an NA is returned)
#' \itemize{
#'   \item{nespp4 - A named vector of market categories to include in block tab, where names correspond to the nespp4 codes. No default. Market categories included in the stock definition but excluded here are by default prorated (see tab_prorate for specifics)}
#'   \item{dropMarketYr - A table containing the nespp4 market category ("NESPP4" column) to "roll up" (i.e. drop) in the corrsponding year column("YEAR"), if provided landings are redistributed to other categories for that year/region (see "roll up" in Adding and Updating Stocks in StockEff documentation)}
#'   \item{block_opt - A string for the following options to specify blocks for each market category and year: 
#'           if = NULL (default), one block for every L-W relationship in tab_lw, 
#'           if = "MONTH", one block for each month (useful for checking minimum number of lengths exceeded for larger blocks), 
#'           if = "QUARTER", one block for each quarter,
#'           if = "SEMESTER", one block for each semester,
#'           if = "ANNUAL", one block per year for each market category
#'           if = "checkLENGTHS", Assumes monthly blocks were submitted to stockEff, checks the number of monthly length samples from biosamp_summary.csv to determine smallest block supported by data for each market category and year. Must also provide minLenSample argument if this option is used.}
#'   \item{minLenSample - A number specifying the minimum number of length sampels required to support a given block for a year and market category, only required if tab_blocks$block_opt == "checkLENGTHS"}
#'   \item{autofillLW - A boolean, if TRUE fills LW_ID based on complete overlap with LW months and years, if FALSE leaves NAs for blocks without a perfect match to LW month and year definition, default = FALSE}
#'   \item{mkt_missing - A string to govern years where 1+ market categories not length sampled, default = "keep":
#'            if = "drop" then these years dropped and NAA is not estimated
#'            if = "keep" then return NAs for BLOCK_TYPE in these years}
#'   \item{check_missing - A boolean, if TRUE check for missing market categories in data_years, also check if is.na(mkt_missing) == FALSE (i.e. if years with missing market categories will be dropped or further processed)}
#' }
#' @param tab_prorate A list providing the following information to populate the PRORATE tab:
#' \itemize{
#'   \item{turnOff - A vector of years for which unclassified landings should NOT be prorated, default prorates for all data_years.}
#' }
#' @param tab_regions A list providing the following information to populate the REGIONS tab: (NOTE: code is less well tested with >1 region)
#' \itemize{
#'   \item{stat_areas - A list of stat area vectors by region, no default. Example: list(c(510, 511), c(512, 513)) assumes 2 regions each containing 2 stat areas}
#'   \item{region_custom - A optional custom matrix containing "REGION_ID", "AREA", and "YEAR" columns, not required if stat_areas object provided}
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
#' }
#' @param tab_sex A list providing the following information to populate the SEX_TYPE tab:
#' \itemize{
#'   \item{start_yr - The start year for CF data, default = first year in data_years.}
#'   \item{end_yr - The end year for CF data, default = last year in data_years.}
#' }
#' @param outfile A string for the final exported file name, does NOT need to include .xlsx extention. No default.
#' 
#' NOTE: tab_blocks$block_opt = checkLENGTHS will NOT work unless connected to the VPN
#' NOTE: ALK_HOLES, EXCLUSIONS, and LENGTH_IMPUTATIONS tabs are not populated by this function but placeholders are put in the resulting output file so that they can be filled externally as needed.
#' NOTE: If you receive the following error, then the .xlsx file by the provided name already exists and can't be overwritten: [ERROR] workbook_close(): Error creating 'outfile.xlsx'. System error = Permission denied Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.'
#' NOTE: If you find NAs in the REGION_ID for the BLOCKS tab, then there was a stat area in the stockeff_pre_prod.mv_cf_stock_data_length_o product that was not provided in the tab_regions$stat_areas argument.
#'
#' @return A list containing the following:
#' \itemize{
#'   \item{mkt_missing_yr - A table of market categories years for which 1+ market categories were not sampled where 1+ market categories were not sampled}
#'   \item{regions_missing - Table of AREAs that appear in oracle data but not in tab_regions$stat_area argument for this function (may be missing blocking)}
#' }

# library(tidyverse)
# library(DataExplorer)
# library(writexl)
# library(DBI)

# GB cod example - this still uses the RT LW
# fillLW <- read_csv("Data_workflow/RT_code_fromCP/lw_coefficients_cod_final.csv", show_col_types = FALSE) %>%
#   filter(STOCK == "GB") %>%
#   mutate(LW_TYPE = case_when(TB %in% c("QTR1", "QTR2", "QTR3", "QTR4") ~ "QUARTER",
#                              TB %in% c("SEM1", "SEM2") ~ "SEMESTER",
#                              TB == "ANN" ~ "ANNUAL"),
#          MONTH_START = case_when(TB %in% c("QTR1", "SEM1", "ANN") ~ 1,
#                                  TB == "QTR2" ~ 4,
#                                  TB %in% c("QTR3", "SEM2") ~ 7,
#                                  TB == "QTR4" ~ 10),
#          MONTH_END = case_when(TB == "QTR1" ~ 3,
#                                TB %in% c("QTR2", "SEM1") ~ 6,
#                                TB == "QTR3" ~ 9,
#                                TB %in% c("QTR4", "SEM2", "ANN") ~ 12),
#          YEAR_START = 2019, YEAR_END = 2023,
#          SOURCE = paste0("RT calculated LW: lw_coefficients_cod_final.csv ", TB, " ", YEAR_START, "-",YEAR_END)) %>%
#   group_by(STOCK, TB, a, b) %>% mutate(LW_ID = cur_group_id())
# 
# fillCF(species_itis = 164712,
#        stock_abbrev = "GBK",
#        assessment_abbrev = "MT",
#        assessment_year = 2024,
#        data_years = 2019:2023, # I want an additional 3 years of data so I have overlap with the RT to check against
#        sex_type = "NONE",
#        tab_regions = list(stat_areas = list(c(464, 522, 523, 524, 525, 561, 562, 551, 552, 542, 543))), # From post-Hague line SAS code
#        tab_lw = data.frame(ALPHA = fillLW$a,
#                            BETA = fillLW$b,
#                            SOURCE = fillLW$SOURCE,
#                            LW_TYPE = fillLW$LW_TYPE,
#                            LW_ID = fillLW$LW_ID,
#                            MONTH_START = fillLW$MONTH_START,
#                            MONTH_END = fillLW$MONTH_END,
#                            YEAR_START = fillLW$YEAR_START,
#                            YEAR_END = fillLW$YEAR_END),
#        tab_blocks = list(nespp4 = c(LARGE = 811, Market = 813, Scrod = 814),# Available: c(Unknown = 0, Large = 811, Market = 813, Scrod = 814, Steaker = 816, "Undersized (MREM)" = 822) but only use 811, 813 and 814 (816 headless so no ages, 822 MREM undersized, 0 unknown)
#                        minLenSample = 10,
#                        block_opt = "checkLENGTHS",
#                        autofillLW = TRUE,
#                        mkt_missing = "drop"),
#        outfile = here::here("Data_workflow", "GB_cod_CFfill_3-22-24"))

#' # BSB SOUTH example
#' # First grab LW info as processed for RT
#' load(here::here("Data_workflow/BSBtest_Length_Weight_Data.RDATA")) # Load LW example for South BSB stock
#' stock = "SOUTH"
#' lm.obj <- get(paste("lm",tolower(stock),"sem.yrbin", sep='.'))
#' lm.param.sem <- vector('list',length(lm.obj))
#' names(lm.param.sem) <- names(lm.obj)
#' 
#' for (sem in names(lm.param.sem))
#' {
#'   # sem <- names(lm.param.sem)[1]
#'   x <- lm.obj[[sem]]
#'   lm.mat <- data.frame(do.call(rbind, 
#'                                lapply(x, function(y) {
#'                                  c( y$coefficients["log(LENGTH)"],
#'                                     y$coefficients["(Intercept)"]
#'                                  )
#'                                } ))
#'   )
#'   colnames(lm.mat) <- c('Log.length','Intercept')
#'   lm.mat$YrBin <- rownames(lm.mat)
#'   rownames(lm.mat) <- NULL
#'   lm.mat$Semester <- sem  
#'   lm.param.sem[[sem]] <- lm.mat
#' }
#' lm.params <- do.call(rbind, lm.param.sem)
#' rownames(lm.params) <- NULL
#' 
#' ### Create LW_Params table
#' lw.params <- tibble(lm.params) %>%
#'   mutate(
#'     ALPHA = exp(lm.params$Intercept), .before=1
#'   ) %>%
#'   select(-Intercept) %>%
#'   rename(BETA = Log.length) %>%
#'   unite("SOURCE", Semester:YrBin, remove=FALSE) %>%
#'   mutate(LW_TYPE = 'SEMESTER',
#'          LW_ID = 1:n(),
#'          .before = YrBin
#'   ) %>%
#'   mutate(YEAR_START = c(1989, 2002, 2012, 1989, 2002, 2012), # 1989 start years were calculated with data from 1992-2001 and used to fill 1989-1991 since no samples available for these early years
#'          YEAR_END = c(2001, 2011, 2021, 2001, 2011, 2021),
#'          MONTH_START = c(1,1,1,7,7,7),
#'          MONTH_END = c(6,6,6,12,12,12))
#' 
#' fillCF(species_itis = 167687,
#'  stock_abbrev = "SOUTH",
#'  assessment_abbrev = "RT",
#'  assessment_year = 2022,
#'  data_years = 1989:2021,
#'  sex_type = "NONE",
#'  tab_regions = list(stat_areas = list(c('614', '615', '621', '622', '623', '624', '625', '626', '627', '628', '629', '631', '632', '633', '634', '635', '636', '637', '638', '639', '640'))),
#'  tab_lw = data.frame(ALPHA = lw.params$ALPHA,
#' BETA = lw.params$BETA,
#' SOURCE = lw.params$SOURCE,
#' LW_TYPE = lw.params$LW_TYPE,
#' LW_ID = lw.params$LW_ID, 
#' MONTH_START = lw.params$MONTH_START,
#' MONTH_END = lw.params$MONTH_END,
#' YEAR_START = lw.params$YEAR_START,
#' YEAR_END = lw.params$YEAR_END),
#'  tab_blocks = list(nespp4 = c('3350', '3351', '3352', '3353', '3355'),
#'  block_opt = "SEMESTER",
#'  autofillLW = TRUE),
#' outfile =  here::here("test_SOUTH_BSB_CFfill"))
#' 
#' originalBLOCKS <- read.csv(here::here("Data_workflow", "BSBtest_CF_Specification.BLOCKS.167687.SOUTH.csv"))
#' originalBLOCKS$NESPP4 <- as.character(originalBLOCKS$NESPP4)
#' originalBLOCKS$REGION_ID <- as.character(originalBLOCKS$REGION_ID)
#' originalBLOCKS$LW_ID <- as.character(originalBLOCKS$LW_ID)
# #' library(readxl)
#' testBLOCKS <- read_excel(here::here("test_SOUTH_BSB_CFfill.xlsx"), sheet = "BLOCKS")
#' # testBLOCKS$REGION_ID[nrow(testBLOCKS)] <- "2" # If uncommented makes 1 row not match in testBLOCKS, shows up in tidylog below
# #' library(tidylog)
#' left_join(testBLOCKS, originalBLOCKS, by = colnames(originalBLOCKS))

fillCF <- function(species_itis = NULL,
                   stock_abbrev = NULL, 
                   assessment_abbrev = NULL,
                   assessment_year = NULL,
                   data_years = NULL,
                   sex_type = "NONE",
                   tab_blocks = list(autofillLW = FALSE,
                                     mkt_missing = "keep",
                                     check_missing = FALSE),
                   tab_prorate = NULL,
                   tab_regions = NULL,
                   tab_lw = NULL,
                   tab_sex = NULL,
                   outfile = NULL){
  
  
  
  # Set default list arguments that are not NULL 
  if(is.null(tab_blocks$autofillLW)){ # If setting is not provided, ensure that the default is FALSE
    tab_blocks$autofillLW = FALSE
  }
  if(is.null(tab_blocks$check_missing) == TRUE){ # If setting is not provided, ensure that the default is FALSE
    tab_blocks$check_missing = FALSE
  }
  if(is.null(tab_blocks$mkt_missing) == "drop"){ # If manipulating years with missing market categories, check_missing must be TRUE
    tab_blocks$check_missing == TRUE
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
  
  
  # REGIONS tab
  if(is.null(tab_regions$stat_area) == FALSE){ # Stat areas grouped by region (default option)
    AREA <- unlist(tab_regions$stat_areas)
    YEAR <- data_years
    
    REGION_ID <- NULL
    for(iregion in 1:length(tab_regions$stat_areas)){
      REGION_ID <- c(REGION_ID, rep(iregion, length(tab_regions$stat_areas[[iregion]])))
    }
    region_table <- cbind(AREA, REGION_ID) %>% as.data.frame()
    
    tab_REGIONS <- expand.grid(AREA = unlist(tab_regions$stat_areas), YEAR = data_years) %>% 
      full_join(., region_table, by = "AREA") %>%
      cbind(SPECIES_ITIS = species_itis,
            STOCK_ABBREV = stock_abbrev,
            SEX_TYPE = sex_type,
            ASSESSMENT_ABBREV = assessment_abbrev,
            SA_YEAR = assessment_year) %>%
      relocate(c(REGION_ID, AREA, YEAR), .after = SA_YEAR) #!!! need to test with multiple regions, code works but not sure if all region 1 should be together followed by all region 2 for subsequent processing
    
  } else if(is.null(tab_regions$region_custom) == FALSE){ # Custom region ID, stat area, year grouping provided
    tab_REGIONS <- cbind(SPECIES_ITIS = species_itis,
                         STOCK_ABBREV = stock_abbrev,
                         SEX_TYPE = sex_type,
                         ASSESSMENT_ABBREV = assessment_abbrev,
                         SA_YEAR = assessment_year, 
                         REGION_ID = tab_regions$region_custom[,"REGION_ID"],
                         AREA = tab_regions$region_custom[,"AREA"],
                         YEAR = tab_regions$region_custom[,"YEAR"])
  }
  tab_REGIONS$AREA <- as.character(tab_REGIONS$AREA) # So type match when setting up blocks
  
  
  # LW_PARAMS tab
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
  
  # BLOCKS tab
  if(tab_blocks$block_opt == "MONTH") { # Implement monthly blocks for each market category and year
    tempBlock <- expand.grid(YEAR = data_years,
                             NESPP4 = tab_blocks$nespp4,
                             REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "CUSTOM",
                             MONTH_START = as.numeric(1:12)) %>% 
      mutate(MONTH_END = MONTH_START)
    mkt_missing_yr <- "Not calculated since check_missing = FALSE"
    regions_missing <- "Not calculated since check_missing = FALSE"
  } else if(tab_blocks$block_opt == "QUARTER"){ # Implement quarterly blocks for each market category and year
    tempBlock <- expand.grid(YEAR = data_years,
                             NESPP4 = tab_blocks$nespp4,
                             REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "QUARTER",
                             MONTH_START = c(1,4,7,10)) %>% 
      mutate(MONTH_END = MONTH_START+2)
    mkt_missing_yr <- "Not calculated since check_missing = FALSE"
    regions_missing <- "Not calculated since check_missing = FALSE"
  } else if(tab_blocks$block_opt == "SEMESTER"){ # Implement semester blocks for each market category and year
    tempBlock <- expand.grid(YEAR = data_years,
                             NESPP4 = tab_blocks$nespp4,
                             REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "SEMESTER",
                             MONTH_START = c(1,7)) %>% 
      mutate(MONTH_END = MONTH_START+5)
    mkt_missing_yr <- "Not calculated since check_missing = FALSE"
    regions_missing <- "Not calculated since check_missing = FALSE"
  } else if(tab_blocks$block_opt == "ANNUAL"){ # Implement a single annual block for each market category and year
    tempBlock <- expand.grid(YEAR = data_years,
                             NESPP4 = tab_blocks$nespp4,
                             REGION_ID = unique(tab_REGIONS$REGION_ID),
                             BLOCK_TYPE = "ANNUAL",
                             MONTH_START = c(1),
                             MONTH_END = c(12)) 
    mkt_missing_yr <- "Not calculated since check_missing = FALSE"
    regions_missing <- "Not calculated since check_missing = FALSE"
  } else if(tab_blocks$block_opt == "checkLENGTHS" | tab_blocks$check_missing == TRUE){ 
    ## Read in ages and lengths if 1) blocking should be done based on length availability, or 2) code should check for missing market categories in data_years
    
    # Connect to oracle
    connection <- dbConnect(drv = dbDriver("Oracle"),
                            username = rstudioapi::askForPassword("Oracle user name"), 
                            password = rstudioapi::askForPassword("Oracle password"),
                            dbname = rstudioapi::askForPassword("Oracle database name"))
    
    # Pull age information by species, stock, and year
    checkAges <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM stockeff_pre_prod.mv_cf_stock_data_age_o WHERE species_itis = ", species_itis, " AND stock_abbrev = '", stock_abbrev,"'", " AND year >= ", data_years[1], " AND year <= ", data_years[length(data_years)]))
    
    # Pull lengths by species, stock, and year
    checkLengths <- ROracle::dbGetQuery(connection, statement = paste0("SELECT * FROM stockeff_pre_prod.mv_cf_stock_data_length_o WHERE species_itis = ", species_itis, " AND stock_abbrev = '", stock_abbrev,"'", " AND year >= ", data_years[1], " AND year <= ", data_years[length(data_years)])) %>%
      # filter(NESPP4 %in% paste0("0",tab_blocks$nespp4)) %>% # Add 0 at start of NESPP4 code (codes are 4 digits long) and filter only the selected market categories
      filter(NESPP4 %in% tab_blocks$nespp4) %>% # Filter to only selected market categories
      left_join(.,tab_REGIONS[,c("AREA", "REGION_ID", "YEAR")], by = c("AREA", "YEAR")) %>%
      mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
                             QTR %in% c(3,4) ~ 2))
    
    if(tab_blocks$check_missing == TRUE){
      regions_missing <- checkLengths %>% filter(is.na(REGION_ID) == TRUE) %>% select(YEAR, MONTH, AREA, NESPP4, LENGTH, NO_AT_LENGTH)
      if(nrow(regions_missing) == 0){ # If no regions missing, also check for missing market categories
        mkt_missing_yr <- expand.grid(YEAR = data_years,
                                      QTR = as.character(1:4),
                                      NESPP4 = as.character(tab_blocks$nespp4)) %>%
                                      #NESPP4 = as.character(paste0("0",tab_blocks$nespp4))) %>% 
          mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
                                 QTR %in% c(3,4) ~ 2)) %>%
          left_join({checkLengths %>% group_by(YEAR, NESPP4) %>% summarise(LENGTHS_ANN = sum(NO_AT_LENGTH, na.rm = TRUE))}) %>% # NAs in LENGTHS_ANN when no lengths for that year/NESPP4 grouping
          group_by(YEAR, NESPP4) %>% 
          mutate(mktComplete = ifelse(any(is.na(LENGTHS_ANN) == TRUE), FALSE, TRUE)) %>% 
          filter(mktComplete == FALSE) %>% # Pull out market categories that are missing by year
          distinct(YEAR,NESPP4,LENGTHS_ANN,mktComplete) %>%
          arrange(YEAR)
      } else{ # If region missing, return a warning and do not check for missing market categories
        mkt_missing_yr <- "Not calculated due to missing regions, check that tab_regions$stat_area input is complete."
      }

    } 
    
    # Disconnect from database
    dbDisconnect(connection)
    
    if(tab_blocks$block_opt == "checkLENGTHS"){
      # Check lengths
      defineBlocks <- expand.grid(YEAR = data_years,
                                  QTR = as.character(1:4),
                                  NESPP4 = tab_blocks$nespp4) %>%
                                  # NESPP4 = paste0("0",tab_blocks$nespp4)) %>% 
        mutate(SEM = case_when(QTR %in% c(1,2) ~ 1,
                               QTR %in% c(3,4) ~ 2)) %>%
        # Sum lengths by quarter
        left_join(.,{checkLengths %>% group_by(YEAR, NESPP4, QTR) %>% mutate(NESPP4 = as.numeric(NESPP4)) %>% summarise(LENGTHS_QTR = sum(NO_AT_LENGTH))}) %>%
        left_join({checkLengths %>% group_by(YEAR, NESPP4, SEM) %>% mutate(NESPP4 = as.numeric(NESPP4)) %>% summarise(LENGTHS_SEM = sum(NO_AT_LENGTH))}) %>%
        left_join({checkLengths %>% group_by(YEAR, NESPP4) %>% mutate(NESPP4 = as.numeric(NESPP4)) %>% summarise(LENGTHS_ANN = sum(NO_AT_LENGTH))}) %>%
        
        # Replace NAs with 0s used in checking complete market sampling
        mutate(LENGTHS_QTR = ifelse(is.na(LENGTHS_QTR), 0, LENGTHS_QTR),
               LENGTHS_SEM = ifelse(is.na(LENGTHS_SEM), 0, LENGTHS_SEM),
               LENGTHS_ANN = ifelse(is.na(LENGTHS_ANN), 0, LENGTHS_ANN)) %>%
        group_by(YEAR) %>% mutate(mktComplete = ifelse(any(LENGTHS_ANN == 0), FALSE, TRUE)) %>%
        group_by(YEAR, NESPP4, SEM, QTR) %>%
        dplyr::summarise(lengths_QTR = unique(LENGTHS_QTR), lengths_SEM = unique(LENGTHS_SEM), lengths_ANN = unique(LENGTHS_ANN), REGION_ID = unique(REGION_ID)) %>% # Collapse duplicates by month
        group_by(YEAR, NESPP4, REGION_ID) %>%
        summarise(BLOCK_TYPE = case_when((sum(lengths_QTR >= tab_blocks$minLenSample) == 4) ~ "QUARTER", # Assign block
                                         (sum(lengths_SEM >= tab_blocks$minLenSample) == 4) ~ "SEMESTER", #!!1 doesn't work, lengths_SEM needs to check if sufficient lengths in both semester, currently if <4 quarters sampled then won't do annual blocking even if # of lengths is correct, need to add expand.grid at beginning like Charles' code did in order to guarantee 4 months of info
                                         (sum(lengths_ANN >= tab_blocks$minLenSample) == 4) ~ "ANNUAL",
                                         .default = "Insufficient_Samples"), # Return "Insufficient_Samples if market category missing or annual number of samples < minLenSample
                  REGION_ID = unique(REGION_ID)) # Retain for final tab_BLOCKS formatting
      
      quarters <- expand.grid(BLOCK_TYPE = "QUARTER",
                              MONTH_range = c("1_3", "4_6", "7_9", "10_12")) # Month range for each quarter
      semesters <- expand.grid(BLOCK_TYPE = "SEMESTER",
                               MONTH_range = c("1_6", "7_12")) # Month range for each semester
      annual <- expand.grid(BLOCK_TYPE = "ANNUAL",
                            MONTH_range = c("1_12"))
      months <- rbind(quarters, semesters, annual)
      
      # Expand starting and end month for each block type
      tempBlock <- full_join(defineBlocks, months, by = "BLOCK_TYPE", relationship = "many-to-many") %>% 
        separate_wider_delim(MONTH_range, delim = "_", names = c("MONTH_START", "MONTH_END"))
      
    } # End statement to define checkLengths blocks
    
    } # End tab_blocks$opt and tab_blocks$check_missing statements
  
  # Handle incomplete market category
  if(tab_blocks$mkt_missing == "drop"){ # Drop years that didn't sample a market category
    tempBlock <- tempBlock %>% filter(YEAR %in% mkt_missing_yr$YEAR == FALSE)
  } 
  
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
    tempBlock$LW_ID[which(tempBlock$MONTH_START == 1 & tempBlock$MONTH_END == 12)] <- tab_LW_PARAMS$LW_ID[which(tab_LW_PARAMS$LW_TYPE == "ANNUAL")]
  }
  
  # Finish block formatting
  tab_BLOCKS <- tempBlock %>% group_by(YEAR, NESPP4, REGION_ID) %>% 
    mutate(BLOCK_ID = seq_along(YEAR)) %>% # Add the block ID numbers
    cbind(SPECIES_ITIS = species_itis,
          STOCK_ABBREV = stock_abbrev,
          SEX_TYPE = sex_type,
          ASSESSMENT_ABBREV = assessment_abbrev,
          SA_YEAR = assessment_year)  %>% as.data.frame() %>%
    relocate(c(YEAR, NESPP4, REGION_ID, BLOCK_ID, BLOCK_TYPE, MONTH_START, MONTH_END, LW_ID), .after = SA_YEAR)
  
  # If specification drops some market categories in some years then drop here
  if(is.null(tab_blocks$dropMarketYR) == FALSE){ 
    tab_BLOCKS <- tab_BLOCKS  %>% 
      full_join(., tab_blocks$dropMarketYR, by = c("NESPP4", "YEAR")) %>%
      replace_na(list(drop_market=FALSE)) %>%
      filter(drop_market == FALSE) %>% # Keep market categories in years where not dropped 
      DataExplorer::drop_columns("drop_market")
  }
  
  # SEX_TYPE tab
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
                        CF_START_YEAR = start,
                        CF_END_YEAR = end,
                        SEX_TYPE = sex_type)
  
  
  # ALK_HOLES tab - optional, can use stockEff auto-fill
  tab_ALK_HOLES <- data.frame(matrix(vector(), 0, 16, dimnames = list(c(), c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "YEAR", "REGION_ID", "BLOCK_ID", "LENGTH_UOM", "LENGTH", "LENGTH", "AGE_UOM",
                                                                             "AGE", "NO_AT_AGE", "NESPP4", "ASSESSMENT_ABBREV", "SA_YEAR", "SOURCE"))),
                              stringsAsFactors = FALSE)

  # EXCLUSIONS tab - optional if some data should be excluded
  tab_EXCLUSIONS <- data.frame(matrix(vector(), 0, 21, dimnames = list(c(), c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", "NESPP4", "AREA",
                                                                              "PORT", "MONTH", "DAY", "CATDISP", "SEX", "LINK", "BIOSAMP_SOURCE", "LENGTH", "LENGTH_UOM",
                                                                              "AGESTRCT", "AGE", "AGE_UOM", "REASON"))),
                               stringsAsFactors = FALSE)
  
  # LENGTH_IMPUTATIONS tab - optional, provide imputations for market category/region/block/length if missing numbers at length
  tab_LENGTH_IMPUTATIONS <- data.frame(matrix(vector(), 0, 12, dimnames = list(c(), c("STOCK_ABBREV", "SEX_TYPE", "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", "NESPP4", "REGION_ID",
                                                                                      "BLOCK_ID", "LENGTH_UOM", "LENGTH", "NO_AT_LENGTH", "SOURCE"))),
                                       stringsAsFactors = FALSE)
  
  ## Write tables to tabs of CF excel template
  
  # Remove temporary month and year start/end columns used to auto-assign LW relationships to blocks
  tab_LW_PARAMS <- tab_LW_PARAMS %>% as.data.frame() %>% drop_columns(c("MONTH_START", "MONTH_END", "YEAR_START", "YEAR_END"))  %>% as.data.frame()
  
  # Generate list of required tabs to output
  write_list <- list("BLOCKS" = as.data.frame(tab_BLOCKS),
                     "PRORATE" = as.data.frame(tab_PRORATE),
                     "REGIONS" = as.data.frame(tab_REGIONS),
                     "LW_PARAMS" = tab_LW_PARAMS,
                     "SEX_TYPE" = as.data.frame(tab_SEX_TYPE),
                     "ALK_HOLES" = as.data.frame(tab_ALK_HOLES), # Last 3 tabs are placeholders that can be filled externally to this script as needed
                     "EXCLUSIONS" = as.data.frame(tab_EXCLUSIONS),
                     "LENGTH_IMPUTATIONS" = as.data.frame(tab_LENGTH_IMPUTATIONS))
  
  # Actually write to template
#   library(writexl)
  write_xlsx(write_list, paste0(outfile, ".xlsx")) # Drop information used in setting up tab_BLOCKS but not required in tab_LW_PARAMS before writing to template
  
  # Return
  return_list <- list(mkt_missing_yr = mkt_missing_yr,   # Vector of years where 1+ market categories were not sampled
                      regions_missing = regions_missing) # Table of AREAs that appear in oracle data but not in tab_regions$stat_area argument for this function (may be missing blocking)
  
  return(return_list) 
}


