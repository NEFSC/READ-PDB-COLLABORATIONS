#' @title Fill CF specification template for STOCKEFF
#' 
#' @param species_itis 6 digit ITIS identifying the species for which data should be pulled, no default.
#' @param stock_abbrev A string describing the stock abbreviation assigned by stock efficiency (this shows up in the web address for STOCKEFF products), no default.          
#' @param assessment_abbrev A string describing the assessment abbreviation, no default. Common options include "MT", "RT", "OA"
#' @param assessment_year The year in which the assessment will be reviewed, no default.
#' @param data_years The vector of years for which catch data should be formatted for STOCKEFF CF excel template, no default.
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
#'           if = "checkLENGTHS", Assumes monthly blocks were submitted to STOCKEFF, checks the number of monthly length samples from biosamp_summary.csv to determine smallest block supported by data for each market category and year. Must also provide minLenSample argument if this option is used. }
#'   \item{minLenSample - A number specifying the minimum number of length sampels required to support a given block for a year and market category, used in length imputation check even if block_opt != checkLENGTHS, default = 1}
#'   \item{autofillLW - A boolean, if TRUE fills LW_ID based on complete overlap with LW months and years, if FALSE leaves NAs for blocks without a perfect match to LW month and year definition, default = FALSE}
#'   \item{mkt_missing - A string to govern years where 1+ market categories not length sampled, default = "keep":
#'            if = "drop" then these years dropped and NAA is not estimated
#'            if = "keep" then return NAs for BLOCK_TYPE in these years}
#'            # THIS ARGuMENT IS NO LONGER USED AND WAS REPLACED BY 
#'   \item{drop_blocking - A string specifying conditions or years for which blocking should be dropped:
#'            if = A vector of years, then those years are dropped from blocking and no age comps are processed.
#'            if = "missing_market", then years for which one or more market categories are not sampled are dropped from blocking and no age comps are processed.
#'            if = "X%", then years for which X percent of landings do not have length sampled are dropped from blocking and no age comps are processed (e.g. "80%" would drop years for which < 80 % of landings have length samples)
#'   } #!!! CHECK THAT THIS OPTION IS BUILT OUT
#'   \item{check_missing - A boolean, if TRUE check for missing market categories in data_years, also check if is.na(mkt_missing) == FALSE (i.e. if years with missing market categories will be dropped or further processed)}
#' }
#' @param tab_prorate A list providing the following information to populate the PRORATE tab:
#' \itemize{
#'   \item{turnOff - A vector of years for which unclassified landings should NOT be prorated, default prorates for all data_years.}
#' }
#' @param tab_regions A list providing the following information to populate the REGIONS tab:
#' \itemize{
#'   \item{stat_areas - A list of stat area vectors by region. Example: stat_areas = list("1" = c(511, 512), "2" = c(513, 514)}
#'   \item{region_custom - An optional data.frame/matrix for complex cases. 
#'         Example: data.frame(AREA = c(511, 512, 511), 
#'                            REGION_ID = c("1", "1", "2"), 
#'                            YEAR = c(2020, 2020, 2021))
#'         This is useful if an AREA changes REGION_ID over time.}
#' }
#' @param tab_lw A matrix or data.frame containing the following named columns of information for each LW data source. Currently only supports blocks with types, start and end months that match LW_PARAMS and are consistent across all years
#' \itemize{
#'   \item{ALPHA - Alpha parameter for each LW relationship}
#'   \item{BETA - Beta parameter for each LW relationship}
#'   \item{SOURCE - Description of LW parameter and link to source document}
#'   \item{LW_TYPE - Type of LW relationship, options can include "SEMESTER", "ANNUAL", "QUARTER"}
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
#' @param tab_impute A list providing rules for filling sampling gaps in the LENGTH_IMPUTATIONS tab, tab_blocks$minLenSample used to ID holes requiring imputation, if null then no imputations provided (default):
#' \itemize{
#'   \item{prorate_percent - A threshold between 1-100 corresponding to the maximum percentage of landings that must occur in a block, below which landings are prorated if no lengths are available for that block}
#'   \item{borrow_procedure - A vector specifying the type and order of borrowing logic to apply to provide length imputations, options include:
#            Check documentation for borrow_lengths for most up-to-date borrowing options
#'   }
#' }
#' @param tab_alk A list providing rules for filling gaps in the ALK_HOLES tab:
#' \itemize{
#'   \item{fill_alk - A boolean. If TRUE, identifies lengths present in landings that 
#'         lack corresponding age data and attempts to fill them using hierarchical borrowing. Default = FALSE.}
#'   \item{source - A vector of strings identifying data sources for age data gap filling,options are: "commercial", "survey". Default = "commercial", MUST contain "commercial" or ALK borrowing will trigger error} 
#'   \item{survey_strata - A vector of survey strata to include in borrowing, required if "survey" in source vector. No default.}
#'   \item{borrow_procedure - A string specifying the borrowing logic to apply to fill age gaps for ALK, options include:
#'            "seqCFSV_S_A" - Default, Borrow: 1) CF within semester, 2) CF within year, 3) SV within semester, 4) SV within year
#'            "seqSA_CF_SV" - Borrow: 1) CF within semester (i.e. pool mkt categories), 2) SV within semester, 3) CF within year, 4) SV within year
#'            Check documentation for borrow_ages function for most up-to-date borrowing options
#'   }
#'   \item{borrow_2020_option - A string specifying how 2020 borrowing is handled, options include: 
#'            Default uses borrow_procedure to fill 2020 gaps, in most cases this results in no gap filling because borrowing does not occur across years
#'            "none" = do not borrow to fill 2020 age gaps
#'            "combCFSV_1921" = Combine CF and SV data, fill 2020 using all observations from 2019 and 2021
#'            Check documentation for borrow_ages function for most up-to-date borrowing options
#'   } 
#'   \item{survey_region - Optional dataframe containing STRATUM and REGION_ID columns specifying the region 
#'        assignment for each survey stratum. Only required if multiple regions AND survey data used for 
#'        borrowing (i.e. "survey" in source list). Default assigns all survey strata to region 1}   
#' }
#' @param tab_exclusions An optional (default = NULL) list defining automated filters to remove non-representative biological records:
#' \itemize{
#'   \item{len_range - A numeric vector of length 2, e.g., c(10, 100). Any fish with a LENGTH 
#'         strictly less than the first value or strictly greater than the second value 
#'         will be added to the EXCLUSIONS tab.}
#'   \item{age_range - A numeric vector of length 2, e.g., c(1, 20). Any fish with an AGE 
#'         outside this range will be excluded. This is a powerful tool for removing 
#'         extreme outliers that may be age-reading or data-entry errors.}
#' }
#' @param schema Set to "STOCKEFF_PRE_PROD" if using pre production data for assembly and "STOCKEFF" for production data. Default is "STOCKEFF_PRE_PROD"
#' @param outfile A string for the final exported file name, does NOT need to include .xlsx extention. No default.
#' @param conn A DBI or ROracle connection object to the NEFSC Oracle database. 
#'        If NULL (default), the function will prompt for credentials via rstudioapi 
#'        and manage the connection/disconnection automatically. If a connection 
#'        is provided, the user is responsible for closing it after the function executes. !!! May want to change this to use keyring
#' @param optional_plots A boolean, if TRUE optional diagnostic plots (e.g. diagnostics for length imputation) are generated and saved in the same location as outfile. Default = FALSE. !!! Make sure these are added
#'         
#' \itemize{
#' \item{NOTE: tab_blocks$block_opt = checkLENGTHS will NOT work unless connected to the VPN}
#' \item{NOTE: ALK_HOLES, and EXCLUSIONS tabs are optionally populated by this function and may need to be filled externally if tab_alk and tab_exclusions arguments are not used to fill gaps.}
#' \item{NOTE: If you receive the following error, then the .xlsx file by the provided name already exists and can't be overwritten: [ERROR] workbook_close(): Error creating 'outfile.xlsx'. System error = Permission denied Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.'}
#' \item{NOTE: If you find NAs in the REGION_ID for the BLOCKS tab, then there was a stat area in the {schema}.mv_cf_stock_data_length_o product that was not provided in the tab_regions$stat_areas argument. Will also result in REGION_ID = NA in ALKHOLES tab.}
#' }
#' 
#' @return A populated .xlsx template and a list containing the following:
#' \itemize{
#'   \item{mkt_missing_yr - A table of market categories years for which 1+ market categories were not sampled where 1+ market categories were not sampled}
#'   \item{regions_missing - Table of AREAs that appear in oracle data but not in tab_regions$stat_area argument for this function (may be missing blocking)}
#' }
#' 
#'
#' @examples
#' \dontrun{
#' # Example call for GB Cod using spatial and temporal borrowing
#' fillCF(
#'   species_itis = 164712,
#'   stock_abbrev = "GBK",
#'   assessment_abbrev = "MT",
#'   assessment_year = 2024,
#'   data_years = 2019:2023,
#'   tab_regions <- list(
#'     stat_areas = list(
#'       "1" = c(464, 522, 523, 524, 525, 561, 562, 551, 552, 542, 543)
#'     ))
#' # for 2 regions
#' # tab_regions <- list(
#' #     stat_areas = list(
#' #       "1" = c(511, 512, 513),
#' #       "2" = c(521, 522, 525)
#' #       )
#' #     )
#'  tab_blocks <-  list(
#'   nespp4 = c(LARGE = 811, Market = 813, Scrod = 814),
#'   minLenSample = 100,
#'   block_opt = "checkLENGTHS",
#'   autofillLW = TRUE,
#'   mkt_missing = "keep",
#'   autofillLW = T
#'  ) 
#'   tab_impute = list(
#'     borrow_from_region = list("1" = "2"),
#     borrow_from_time = TRUE
#'   ),
#'   outfile = "GB_Cod_Specifications"
#' )
#' 
#' 
#' #mackerel example using exclusions
#' species_itis = 172414
#' stock_abbrev = "UNIT"
#' assessment_abbrev = "MT"
#' assessment_year = 2025
#' data_years = 2018:2019 # small subset for testing
#' sex_type = "NONE"
# Define tab_regions like this: 
#' tab_regions <- list(
#'   stat_areas = list(
#'     "1" = c(464,465,467,468,510,511,512,513,514,515,520,521,522,523,524,525,526,
#'             530,533,534,537,538,539,541,542,543,551,552,560,561,562,600,610,611,
#'             612,613,614,615,616,621,622,623,624,625,626,627,628,629,631,632,633,
#'             634,635,636,637,638,639,640,0
#'     )
#'   )
#' )
#' 
#' #extract lw eq. from STOCKEFF
#' lwQry <- glue::glue("select * from stockeff_pre_prod.i_cf_stock_lw_params_s where species_itis = {species_itis}
#'                     and stock_abbrev = '{stock_abbrev}' and assessment_abbrev = '{assessment_abbrev}'") 
#' fillLW <- ROracle::dbGetQuery(conn,lwQry)
#' tab_lw = data.frame(ALPHA = fillLW$ALPHA,
#'                     BETA = fillLW$BETA,
#'                     SOURCE = fillLW$SOURCE,
#'                     LW_TYPE = fillLW$LW_TYPE,
#'                     LW_ID = fillLW$LW_ID,
#'                     MONTH_START = c(1,7),
#'                     MONTH_END = c(6,12),
#'                     YEAR_START = rep(2000,2),
#'                     YEAR_END = rep(2024,2))

#' tab_blocks <-  list(nespp4 = c(MEDIUM = 2123, SMALL = 2122, LARGE = 2121, UNCLASSIFIED =2120),
#'                     minLenSample = 500,
#'                     block_opt = "checkLENGTHS",
#'                     autofillLW = TRUE,
#'                     mkt_missing = "keep")
#' 
#' tab_impute = list(
#'   #borrow_from_region = list("1" = "2", "2" = "1"), #only valid for stocks with more than one region!
#   borrow_from_time = TRUE # Flag to trigger the temporal fallback
#' )
#' 
#' tab_exclusions = list(len_range = c(10,35))
#' 
#' outfile = file.path("ExampleCFspecs")
#' if(!dir.exists("ExampleAutoCFspecs")) dir.create("ExampleAutoCFspecs")
#' outfile = file.path("ExampleAutoCFspecs",glue::glue("CF_specification_load_{species_itis}_{stock_abbrev}_{assessment_year}"))
#' 
#' tab_prorate = NULL
#' tab_sex = NULL
#' 
#' fillCF(species_itis = species_itis,
#'        stock_abbrev = stock_abbrev,
#'        assessment_abbrev = assessment_abbrev,
#'        assessment_year = assessment_year,
#'        data_years = data_years,
#'        sex_type = sex_type,
#'        tab_regions = tab_regions, 
#'        tab_lw = tab_lw,           
#'        tab_blocks = tab_blocks,
#'        outfile = outfile,
#'        tab_prorate = tab_prorate,
#'        tab_sex = tab_sex,
#'        tab_impute = tab_impute,
#'        tab_alk = tab_alk,
#'        tab_exclusions = tab_exclusions,
#'        conn = conn
#' )
#' 
#' }
#' 
#' 
#' @importFrom dplyr mutate filter select group_by summarise left_join bind_rows rename arrange desc case_when reframe n_distinct ungroup any_of all_of across relocate
#' @importFrom tidyr expand_grid unnest
#' @importFrom stringr str_pad str_trim
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_warning cli_alert_success cli_inform
#' @importFrom glue glue
#' @importFrom ROracle Oracle dbConnect dbGetQuery dbDisconnect
#' @importFrom writexl write_xlsx
#' @importFrom rstudioapi askForPassword
#' @importFrom purrr map_lgl imap_dfr
#' @importFrom tidyr replace_na
#' 
# @importFrom magrittr |>  # This function doesn't appear to be an export of magrittr but instead is a base R function so shouldn't need to be imported

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fillCF <- function(species_itis = NULL,
                   stock_abbrev = NULL, 
                   assessment_abbrev = NULL,
                   assessment_year = NULL,
                   data_years = NULL,
                   sex_type = "NONE",
                   tab_blocks = list(autofillLW = FALSE,
                                     mkt_missing = "keep",
                                     check_missing = FALSE,
                                     block_opt = "ANNUAL",
                                     minLenSample = 1),
                   tab_prorate = NULL,
                   tab_regions = NULL,
                   tab_lw = NULL,
                   tab_sex = NULL,
                   tab_impute = list(prorate_percent = NULL),
                   tab_alk = list(fill_alk = FALSE, 
                                  source = "both"), # Options: "both", "commercial", "survey"
                   tab_exclusions = list(len_range = NULL, 
                                         age_range = NULL),
                   schema = "STOCKEFF_PRE_PROD",
                   outfile = NULL,
                   conn = NULL,
                   optional_plots = FALSE){

  # R package Set-up  
  cli::cli_h1("STOCKEFF Specification Build {species_itis} {stock_abbrev} {assessment_abbrev} {assessment_year}")
  needed_pkgs <- c("tidyverse", "writexl", "DBI", "ROracle", 
                   "rstudioapi", "cli", "glue")
  missing_pkgs <- needed_pkgs[!purrr::map_lgl(needed_pkgs, 
                                              \(x) requireNamespace(x, quietly = TRUE))]
  
  if (length(missing_pkgs) > 0) {
    cli::cli_alert_danger("Required packages are missing: 
                          {paste(missing_pkgs, collapse = ', ')}")
    stop("Process halted.")
  }
  
  # ROracle connection in conn object  
  auto_disconnect <- FALSE
  if (is.null(conn)) {
    cli::cli_alert_info("Connecting to Oracle...")
    conn <- tryCatch({
      ROracle::dbConnect(ROracle::Oracle(), 
                         user = rstudioapi::askForPassword("Oracle Username"), 
                         password = rstudioapi::askForPassword("Oracle Password"), 
                         dbname = "NEFSC_pw_oraprod")
    }, error = function(e) {
      cli::cli_alert_danger("Connection failed: {e$message}")
      return(NULL)
    })
    if (is.null(conn)) stop("Database connection required.")
    auto_disconnect <- TRUE
  }
  
  if(class(try(ROracle::dbListResults(conn)))=="try-error") stop("Oracle connection is non functional - CHECK")

  # Set up defaults for lists  
  if(is.null(tab_blocks$autofillLW)) tab_blocks$autofillLW <- FALSE
  if(is.null(tab_blocks$check_missing)) tab_blocks$check_missing <- FALSE
  if(is.null(tab_blocks$mkt_missing)) tab_blocks$mkt_missing <- "keep"
  
  ##### SEX_TYPE tab #####
  tab_SEX_TYPE <- NULL
  tab_sex$start_yr <- ifelse(is.null(tab_sex$start_yr),min(data_years),tab_sex$start_yr )
  tab_sex$end_yr <- ifelse(is.null(tab_sex$end_yr),max(data_years),tab_sex$end_yr )
  
  tab_SEX_TYPE <- as.data.frame(tab_sex) |> 
    dplyr::mutate(SPECIES_ITIS = species_itis, 
                  STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type,
                  ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year
    ) |> 
    dplyr::rename(CF_START_YR = start_yr,
                  CF_END_YR = end_yr
    ) |> 
    dplyr::select(SPECIES_ITIS, STOCK_ABBREV,ASSESSMENT_ABBREV, SA_YEAR,
                  CF_START_YR,CF_END_YR,SEX_TYPE)
  
  ##### PRORATE tab #####
  
  tab_PRORATE <- data.frame(YEAR = data_years, 
                            PRORATE_COMBINED = rep("Y", length(data_years))) |>
    dplyr::mutate(PRORATE_COMBINED = ifelse(YEAR %in% tab_prorate$turnOff, "N", 
                                            PRORATE_COMBINED),
                  SPECIES_ITIS = species_itis, 
                  STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type,
                  ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year) |>
    dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, 
                  ASSESSMENT_ABBREV, SA_YEAR, YEAR, PRORATE_COMBINED)
  
  ##### REGIONS tab ##### 
  tab_REGIONS <- NULL
  
  if (!is.null(tab_regions$region_custom)) { # Use custom table to specify region by statistical area(s) and year
    cli::cli_alert_info("Using custom region mapping provided in 'region_custom'.")
    tab_REGIONS <- as.data.frame(tab_regions$region_custom)
    
    # Standardize common column naming variations
    if ("STAT_AREA" %in% names(tab_REGIONS)) {
      tab_REGIONS <- dplyr::rename(tab_REGIONS, AREA = STAT_AREA)
    }
    if ("area" %in% names(tab_REGIONS)) {
      tab_REGIONS <- dplyr::rename(tab_REGIONS, AREA = area)
    }
    
  } else if (!is.null(tab_regions$stat_areas)) { # Use time-invariant region definition by statistical area(s)
    region_mapping <- purrr::imap_dfr(tab_regions$stat_areas, 
                                      \(x, y) data.frame(AREA = as.character(x), 
                                                         REGION_ID = as.character(y))) # Standard area mapping to region(s) for use in all years
    
    tab_REGIONS <- tidyr::expand_grid(AREA = region_mapping$AREA, # Expand areas by years
                                      YEAR = data_years) |>
      dplyr::left_join(region_mapping, by = "AREA") # Tie areas to regions
  }
  
  if (is.null(tab_REGIONS)) {
    stop("No regional definition found. Please provide 'stat_areas' or 'region_custom'.")
  }
  
  # Finalize with mandatory STOCKEFF metadata
  tab_REGIONS <- tab_REGIONS |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("AREA", "REGION_ID")), as.character)) |>
    dplyr::mutate(SPECIES_ITIS = species_itis, 
                  STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type,
                  ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year) |> 
    dplyr::select(SPECIES_ITIS,STOCK_ABBREV,SEX_TYPE,ASSESSMENT_ABBREV,SA_YEAR,REGION_ID,AREA,YEAR)
  
  
  ### LW_PARAMS tab #####
  
  tab_LW_PARAMS <- as.data.frame(tab_lw) |>
    dplyr::mutate(SPECIES_ITIS = species_itis, 
                  STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type,
                  ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year) |> 
    dplyr::select(SPECIES_ITIS,STOCK_ABBREV,SEX_TYPE,ASSESSMENT_ABBREV,SA_YEAR
                  ,ALPHA,BETA,SOURCE,LW_TYPE,LW_ID,MONTH_START,MONTH_END,YEAR_START,YEAR_END)
  
  ### BLOCKS tab #####
  
  target_nespp4 <- stringr::str_pad(as.character(tab_blocks$nespp4), 
                                    width = 4, side = "left", pad = "0") # Treat as strings to ensure leading zeros in nespp4 codes are retained
  
  # Load length sampling with no imputations (e.g. those introduced in length expansion)
  raw_lens <- tryCatch({
    sql1 <- glue::glue("SELECT YEAR, NESPP4, AREA, QTR, LENGTH, NO_AT_LENGTH 
                         FROM {schema}.mv_cf_stock_data_length_o 
                         WHERE species_itis = '{species_itis}' 
                         AND year BETWEEN {min(data_years)} AND {max(data_years)}")
    ROracle::dbGetQuery(conn, sql1)
  }, error = function(e) { stop("Length query failed.") })

  # Format raw_lens and add regions
  raw_lens <- raw_lens |> 
    dplyr::filter(YEAR %in% data_years) |> # Only consider commercial data within data_years range of interest
    dplyr::mutate(NESPP4 = as.character(NESPP4), # Ensure same column name/type for later use
                  AREA = stringr::str_trim(as.character(AREA)),
                  YEAR = as.numeric(YEAR), 
                  LENGTH = as.numeric(LENGTH)) |>
    dplyr::filter(NESPP4 %in% target_nespp4) |> # Only keep lengths for desired nespp4 codes
    dplyr::left_join(tab_REGIONS |> dplyr::select(AREA, YEAR, REGION_ID), # Merge with region so lengths have region assigned
                     by = c("AREA", "YEAR"))
  
  if(tab_blocks$block_opt == "checkLENGTHS" | !is.null(tab_impute)){# Generate length sampling summary if checkLENGTHS for blocks OR providing length imputations
    sampling_summary <- raw_lens |>
      dplyr::group_by(YEAR, NESPP4, REGION_ID) |>
      dplyr::summarise(
        q_pass = all(purrr::map_lgl(1:4, 
                                    \(x) sum(NO_AT_LENGTH[QTR == x], na.rm = TRUE) >= tab_blocks$minLenSample)), # ID quarters with minLenSample
        s_pass = (sum(NO_AT_LENGTH[QTR %in% c(1,2)], na.rm = TRUE) >= tab_blocks$minLenSample & #ID semesters with minLenSample
                    sum(NO_AT_LENGTH[QTR %in% c(3,4)], na.rm = TRUE) >= tab_blocks$minLenSample),
        a_pass = (sum(NO_AT_LENGTH, na.rm = TRUE) >= tab_blocks$minLenSample), # ID years with minLenSample
        .groups = "drop"
      ) |>
      dplyr::mutate(BLOCK_TYPE = dplyr::case_when(
        is.na(REGION_ID) ~ "Insufficient_Samples", # If no region ID assigned then insufficient samples
        q_pass ~ "QUARTER", 
        s_pass ~ "SEMESTER", 
        a_pass ~ "ANNUAL", 
        .default = "Insufficient_Samples" # If quarter, semester, and annual blocks have less than minLenSample, label as insufficent samples
      ))
  } # End if statement for summarizing length sampling 

  
  if(tab_blocks$block_opt == "checkLENGTHS"){    
    tempBlock <- sampling_summary |>
      dplyr::filter(BLOCK_TYPE != "Insufficient_Samples") |> # Assign blocks with insufficient samples to annual block, choice to impute lengths or drop block is up to analyst 
      
      full_join(expand.grid(YEAR = unique(tab_REGIONS$YEAR), REGION_ID = unique(tab_REGIONS$REGION_ID), NESPP4 = target_nespp4)) |> # Join with all year/nespp4/region combinations to ensure (annual) blocks set up for market categories even if no lengths collected
      dplyr::mutate(BLOCK_TYPE = dplyr::case_when(is.na(BLOCK_TYPE) ~ "ANNUAL",
                                                  .default = BLOCK_TYPE)) |>
      dplyr::reframe(YEAR, NESPP4, REGION_ID, BLOCK_TYPE, 
                     MONTH_START = dplyr::case_when(
                       BLOCK_TYPE == "QUARTER" ~ list(c(1, 4, 7, 10)), 
                       BLOCK_TYPE == "SEMESTER" ~ list(c(1, 7)), 
                       BLOCK_TYPE == "ANNUAL" ~ list(1)
                     )) |>
      tidyr::unnest(MONTH_START) |>
      dplyr::mutate(MONTH_END = dplyr::case_when(
                      BLOCK_TYPE == "QUARTER" ~ MONTH_START + 2, 
                      BLOCK_TYPE == "SEMESTER" ~ MONTH_START + 5, 
                      BLOCK_TYPE == "ANNUAL" ~ 12
                    )) 
  } else { # if tab_blocks$block_opt != "checkLENGTHS" then set up specified blocking
    block_configs <- list("QUARTER" = list(type = "QUARTER", 
                                           start = c(1,4,7,10), 
                                           offset = 2), 
                          "SEMESTER" = list(type = "SEMESTER", 
                                            start = c(1,7), 
                                            offset = 5), 
                          "ANNUAL" = list(type = "ANNUAL", 
                                          start = 1, 
                                          offset = 11))
    conf <- block_configs[[tab_blocks$block_opt]]
    tempBlock <- tidyr::expand_grid(YEAR = data_years, 
                                    NESPP4 = as.character(tab_blocks$nespp4), 
                                    REGION_ID = as.character(unique(tab_REGIONS$REGION_ID)), 
                                    BLOCK_TYPE = conf$type, 
                                    MONTH_START = conf$start) |>
      dplyr::mutate(MONTH_END = MONTH_START + conf$offset)
  }
  
  tab_BLOCKS <- tempBlock |> 
    dplyr::group_by(YEAR, NESPP4, REGION_ID) |> 
    dplyr::mutate(BLOCK_ID = dplyr::row_number()) |> 
    dplyr::ungroup() |>
    dplyr::mutate(SPECIES_ITIS = species_itis, STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type, ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year, LW_ID = NA_character_)

  # If tab_blocks$autofillLW == TRUE then populate the blocking LW_ID using the month and year range specified in the tab_LW
  if (isTRUE(tab_blocks$autofillLW)) {
    
    # Treat months and years as numeric and arrange so checks occurr in the following order: annual, semester, quarter
    tab_LW_clean <- tab_LW_PARAMS |> 
      dplyr::mutate(dplyr::across(
        dplyr::any_of(c("MONTH_START", "MONTH_END", "YEAR_START", "YEAR_END")), # Ensure months and years are numeric
        as.numeric
      )) |> 
      dplyr::mutate(fill_order = factor(LW_TYPE, levels = c("ANNUAL", "SEMESTER", "QUARTER"))) |> 
      dplyr::arrange(fill_order) # Check for overlap with annual, semester, then quarterly LW
      
    
    # ID blocks with month and year ranges that completely overlap with LW month and year ranges
      # Loop over annual, semester, then quarterly LW so the finest temporal LW relationship used for block
      # If block temporal resolution doesn't overlap with LW then LW_ID = NA (most likely if block doesn't fall within the year range for any annual LW)
    for (i in seq_len(nrow(tab_LW_clean))) { # Loop over LW table organized by fill_order
      
      match_indices <- which(
        as.numeric(tab_BLOCKS$MONTH_START) %in% tab_LW_clean$MONTH_START[i]:tab_LW_clean$MONTH_END[i] &
          as.numeric(tab_BLOCKS$MONTH_END) %in% tab_LW_clean$MONTH_START[i]:tab_LW_clean$MONTH_END[i] &
          as.numeric(tab_BLOCKS$YEAR) %in% tab_LW_clean$YEAR_START[i]:tab_LW_clean$YEAR_END[i] 
      )
      
      # Update the LW_ID for all blocks meeting the overlap criteria (since loop over annual, semester, then quarter will update if finer resolution overlap is available, e.g. a block from month 2:3 is contained in an annual 1:12, semester 1:6, and quarter 1:3 LW but the final quarter option is used since it has the finest resolution)
      if (length(match_indices) > 0) {
        tab_BLOCKS$LW_ID[match_indices] <- as.character(tab_LW_clean$LW_ID[i]) 
      }
    } # End loop over LW relationships
  } # End autofillLW if statement
  
  tab_BLOCKS <- tab_BLOCKS |> 
    dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, 
                  ASSESSMENT_ABBREV, SA_YEAR, YEAR, NESPP4, REGION_ID, 
                  BLOCK_ID, BLOCK_TYPE, MONTH_START, MONTH_END, LW_ID) # LW_ID not populated by autofillLW = NA
  
  ##### LENGTH_IMPUTATIONS tab #####
  # Set up tab columns
  tab_LENGTH_IMPUTATIONS <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), 
                                     c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", 
                                       "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", 
                                       "NESPP4", "REGION_ID", "BLOCK_ID", 
                                       "LENGTH_UOM", "LENGTH", "NO_AT_LENGTH", "SOURCE"))
  
  if(!is.null(tab_impute)) {
    cli::cli_alert_info("Scanning for length sampling gaps to fill using selected borrowing procedure...")
    
    ###### Query landings to check for holes ######
    landings <- tryCatch({
      sql1 <- glue::glue("SELECT SPECIES_ITIS, YEAR, NESPP4_ORIG, NESPP4, AREA, MONTH, QTR, LANDINGS_KG_ORIG, LANDINGS_KG_ADJ
                         FROM {schema}.i_cf_stock_data_adj_land_o 
                         WHERE species_itis = '{species_itis}' 
                         AND year BETWEEN {min(data_years)} AND {max(data_years)}")
      ROracle::dbGetQuery(conn, sql1) |>
        as.data.frame() |>
        mutate(SPECIES_ITIS = as.numeric(SPECIES_ITIS))
      # group_by(YEAR, NESPP4, QTR) |>
      # dplyr::reframe(LANDINGS_MT_ADJ = sum(LANDINGS_KG_ADJ, na.rm = TRUE)/1000, # Summarize quarterly landings in mt
      #                SEM = case_when(QTR %in% c(1,2) ~ 1,
      #                                QTR %in% c(3,4) ~ 2)) 
    }, error = function(e) { stop("Landings query failed.") })
    
    ###### ID holes where landings exist but no length measurements are available ######
    # Calculate number of lengths across all possible blocking structures
    ann_lens <- raw_lens |> # Annual length summary
      group_by(YEAR, NESPP4, REGION_ID) |> 
      dplyr::reframe(NO_AT_LENGTH = sum(NO_AT_LENGTH, na.rm = TRUE), BLOCK_TYPE = "ANNUAL", BLOCK_ID = 1) 
    
    sem_lens <- raw_lens |> # Semester length summary
      mutate(BLOCK_ID = case_when(QTR %in% c(1,2) ~ 1,
                                  QTR %in% c(3,4) ~ 2)) %>%
      group_by(YEAR, NESPP4, REGION_ID, BLOCK_ID) |>
      dplyr::reframe(NO_AT_LENGTH = sum(NO_AT_LENGTH, na.rm = TRUE), BLOCK_TYPE = "SEMESTER")
    
    qtr_lens <- raw_lens |> # Quarterly length summary
      group_by(YEAR, NESPP4, REGION_ID, QTR) |>
      dplyr::reframe(NO_AT_LENGTH = sum(NO_AT_LENGTH, na.rm = TRUE), BLOCK_TYPE = "QUARTER", BLOCK_ID = as.numeric(QTR)) |> # BLOCK_ID causes duplicate rows since QTR is not reframed
      distinct()|>
      select(-QTR)
    # Combine lengths with blocking
    len_blocks <- bind_rows(ann_lens, sem_lens, qtr_lens) %>% 
      right_join(tab_BLOCKS) # Matches length summary to chosen blocking structure (allows different summaries for different years or NESPP4 codes)
    
    # Calculate total landings across all possible blocking structures
    ann_land <- landings |> # Annual total landings
      group_by(YEAR, NESPP4, REGION_ID) |>
      dplyr::reframe(LANDINGS_MT_ADJ = sum(LANDINGS_KG_ADJ, na.rm = TRUE)/1000, BLOCK_TYPE = "ANNUAL", BLOCK_ID = 1)
    
    sem_land <- landings |> # Semester total landings
      mutate(BLOCK_ID = case_when(QTR %in% c(1,2) ~ 1,
                                  QTR %in% c(3,4) ~ 2)) %>%
      group_by(YEAR, NESPP4, REGION_ID, BLOCK_ID) |>
      dplyr::reframe(LANDINGS_MT_ADJ = sum(LANDINGS_KG_ADJ, na.rm = TRUE)/1000, BLOCK_TYPE = "SEMESTER")
    
    qtr_land <- landings |> # Quarterly total landings
      group_by(YEAR, NESPP4, REGION_ID, QTR) |>
      dplyr::reframe(LANDINGS_MT_ADJ = sum(LANDINGS_KG_ADJ, na.rm = TRUE)/1000, BLOCK_TYPE = "QUARTER", BLOCK_ID = as.numeric(QTR)) |> # BLOCK_ID causes duplicate rows since QTR is not reframed
      distinct()|>
      select(-QTR)
    # Combine total landings with blocking
    land_blocks <- bind_rows(ann_land, sem_land, qtr_land) %>%
      right_join(tab_BLOCKS) # Matches total landings to chosen blocking structure (allows total landings to be calculated over different blocks for different years or NESPP4 codes)
    
    # # Development check consistency - pass for scup, all landings successfully joined to blocking
    # bind_rows(ann_land, sem_land, qtr_land) %>%
    #        right_join(tab_BLOCKS) %>% 
    #   #filter(YEAR == 2022, NESPP4 == 3293) %>% 
    #   select(LANDINGS_MT_ADJ) %>% sum(., na.rm = TRUE) 
    # 
    # landings %>% 
    #   filter(!is.na(REGION_ID)) |> # Small portion of landings don't have region assigned, if not removed, results in small difference between sum of source landings and sum of landings joined with blocks (since blocking doesn't include REGION_ID = NA)
    #   #filter(YEAR == 2022, NESPP4 == 3293) %>% 
    #   select(LANDINGS_KG_ADJ) %>% sum(., na.rm = TRUE)/1000 #!!! NO NESPP4 3293 block in 2022 but some landings, why?
    
    length_holes <- full_join(land_blocks, len_blocks) |> # Combine length summary and total landings by block
      group_by(YEAR) |>
      mutate(annual_landings = sum(LANDINGS_MT_ADJ, na.rm = TRUE),
             PROP.ANNUAL.COMLAND = LANDINGS_MT_ADJ/annual_landings) |> # Calculate proportion of total annual landings in each block
      select(-annual_landings) |>
      filter(is.na(NO_AT_LENGTH))  # ID holes for blocks where landings occur but no lengths are sampled
    holes <- length_holes # Rename so output matches output of optional prorating and ensures same name passed to borrow_lengths
    
    ###### Optional prorating ######
    # Add optional check where holes are prorated rather than filling with borrowing logic based on % of aggregate landings they represent
    if(!is.null(tab_impute$prorate_percent)){ # If a percentage was provided to tab_impute$prorate_percent then apply as a filter and prorate holes !!! need to add to documentation and ensure default = NULL once finalized
      holes <- length_holes %>% filter(PROP.ANNUAL.COMLAND > tab_impute$prorate_percent/100) # Retain holes that exceed % contribution to aggregate landings threshold and fill with borrowing
      prorate_holes <- length_holes %>% filter(PROP.ANNUAL.COMLAND <= tab_impute$prorate_percent/100) # ID blocks to prorate
      tab_BLOCKS <- anti_join(tab_BLOCKS, prorate_holes) # Drop all blocks below threshold that do not have length measurements so prorated
    }
    
    ###### length imputations to fill holes ######
    if(nrow(holes) > 0) {
      
      imputed_data_list <- borrow_lengths(missing_lengths = holes,
                                          borrow_procedure = tab_impute$borrow_procedure,
                                          borrowing_data = raw_lens)
      
      # If imputed values provided, append to length imputation tab
      if(length(imputed_data_list) > 0) {
        tab_LENGTH_IMPUTATIONS <- dplyr::bind_rows(imputed_data_list) |> 
          dplyr::select(dplyr::all_of(names(tab_LENGTH_IMPUTATIONS)))
      }
    } # End check if length holes exist
  } # End of fill for tab_impute
  
  ##### ALK_HOLES tab #####
  ## Set defaults
  # Determine the user's preferred data source for age data - only used if tab_alk$fill_alk == TRUE
  if(is.null(tab_alk$source)){
    alk_source_opt <- c("commercial")
  } else {
    alk_source_opt <- tab_alk$source
  }
  # Fill option
  if(is.null(tab_alk$fill_alk)){ # By default do not fill ALK holes
    tab_alk$fill_alk <- FALSE
  } else{ # If filling ALK holes, print type of source data used
    cli::cli_alert_info("Identifying ALK holes via Hybrid Year Logic (Source: {alk_source_opt})...") # Only print if actually using
  }
  # borrow_procedure default follow borrow_ages setting, only used if tab_alk$fill_alk == TRUE
  # survey_region default assigned below
  
  
  tab_ALK_HOLES <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                            c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "YEAR", 
                              "REGION_ID", "BLOCK_ID", "LENGTH_UOM", "LENGTH", 
                              "AGE_UOM", "AGE", "NO_AT_AGE", 
                              "NESPP4", "ASSESSMENT_ABBREV", "SA_YEAR", "SOURCE"))
  
  if(isTRUE(tab_alk$fill_alk)) {
    #search_yrs <- (min(data_years) - 1):(max(data_years) + 1)
    #Remove the +1 / -1 buffer. Only search what the user defined.
    # search_yrs <- data_years # use data_years in queries below so this line redundant
    
    ###### Conditional Database Query: Commercial Ages ######
    mv_cf_age <- NULL
    if("commercial" %in% alk_source_opt) {
      mv_cf_age <- tryCatch({
        q_parts <- list()
        
        if(any(data_years < 2020)) {
          y_leg <- data_years[data_years < 2020]
          q_parts$legacy <- glue::glue("
            SELECT 
              YEAR, 
              NESPP4, 
              AREA, 
              QTR, 
              LENGTH, 
              AGE, 
              NUMAGE AS NO_AT_AGE, 
              DOCN AS SOURCE,
              NULL AS PORT,               -- Aligned with CAMS
              NULL AS MONTH,              -- Aligned with CAMS
              NULL AS CATDISP,            -- Aligned with CAMS
              NULL AS SEX,                -- Aligned with CAMS
              NULL AS LINK,               -- Aligned with CAMS
              'LEGACY' AS BIOSAMP_SOURCE, -- Aligned with CAMS
              NULL AS AGESTRCT,           -- Aligned with CAMS
              'CM' AS LENGTH_UOM,         -- Aligned with CAMS
              'YEAR' AS AGE_UOM           -- Aligned with CAMS
            FROM {schema}.MV_CF_AGE_NEFSC 
            WHERE SPECIES_ITIS = '{species_itis}' 
              AND YEAR IN ({paste(y_leg, collapse = ',')})
              AND AREA IN ({paste(unique(tab_REGIONS$AREA), collapse = ',')})") # Ensure correct data pulled for multi-stock species
        }
        
        if(any(data_years >= 2020)) {
          y_cams <- data_years[data_years >= 2020]
          q_parts$cams <- glue::glue("
            SELECT 
              YEAR, 
              NESPP4, 
              AREA, 
              CEIL(MONTH/3) AS QTR,
              LENGTH, 
              AGE, 
              1 AS NO_AT_AGE,
              DATA_SOURCE AS SOURCE,
              PORT, 
              MONTH, 
              CATDISP, 
              SEX, 
              LINK, 
              DATA_SOURCE AS BIOSAMP_SOURCE, 
              AGESTRCT, 
              LENGTH_UOM, 
              AGE_UOM
            FROM {schema}.MV_CF_AGE_CAMS 
            WHERE SPECIES_ITIS = '{species_itis}' 
              AND YEAR IN ({paste(y_cams, collapse = ',')})
              AND AREA IN ({paste(unique(tab_REGIONS$AREA), collapse = ',')})") # Ensure correct data pulled for multi-stock species
        }
        
        raw_cf_res <- purrr::map_dfr(q_parts, \(x) dplyr::mutate(ROracle::dbGetQuery(conn, x),
                                                          QTR = as.numeric(QTR), # Make sure that variables of same type across legacy and cams data 
                                                          AGESTRCT = as.numeric(AGESTRCT))) 
        
        # Standardize headers to uppercase immediately to insulate from driver variance
        if (!is.null(raw_cf_res) && nrow(raw_cf_res) > 0) {
          names(raw_cf_res) <- toupper(names(raw_cf_res))
        }
        raw_cf_res
        
      }, error = function(e) { return(NULL) })
    }
    
    # Join commercial ages with regional definitions and calculate semesters
    if(!is.null(mv_cf_age) && nrow(mv_cf_age) > 0) {
      mv_cf_age <- mv_cf_age |> 
        dplyr::mutate(YEAR = as.numeric(YEAR), 
                      AREA = stringr::str_trim(as.character(AREA))) |>
        dplyr::left_join(tab_REGIONS |> dplyr::select(AREA, YEAR, REGION_ID), 
                         by = c("AREA", "YEAR")) |> # Introduces NA if AREA not in tab_REGIONS
        dplyr::mutate(SEM = dplyr::case_when(QTR %in% c(1,2) ~ 1, 
                                             QTR %in% c(3,4) ~ 2, 
                                             .default = NA))
    }
    
    ###### Conditional Database Query: Survey Ages ######
    # Query survey age database if option is 'both' or 'survey'
    mv_sv_age <- NULL
    if("survey" %in% alk_source_opt) {
      if(is.null(tab_alk$survey_strata)){stop("Error: tab_alk$survey_strata not provided")}
      mv_sv_age <- tryCatch({
        sql_sv <- glue::glue("SELECT YEAR, SEASON, LENGTH, AGE, AGE_NO as NO_AT_AGE, STRATUM,
                              CRUISE6 as SOURCE FROM {schema}.MV_SV_AGE 
                              WHERE SPECIES_ITIS = '{species_itis}' 
                              AND YEAR BETWEEN {min(data_years)} AND {max(data_years)}
                              AND STRATUM IN ({paste(tab_alk$survey_strata, collapse = ',')})") # Ensure correct data pulled for multi-stock species 
                              ###AND YEAR BETWEEN {min(data_years) - 1} AND {max(data_years) + 1}")
        ROracle::dbGetQuery(conn, sql_sv)
      }, error = function(e) { return(NULL) })
      
      if(is.null(tab_alk$survey_region)){ # By default assume all survey strata used for borrowing in region 1
        mv_sv_age <- mv_sv_age %>% 
          mutate(REGION_ID = "1") %>% 
          select(-STRATUM) # Stratum only used for region assignment 
      } else{ # Use strata region assignments provided in tab_alk$survey_region argument for borrowing
        mv_sv_age <- left_join(mv_sv_age, survey_region, by = "STRATUM") %>% # NA introduced if survey_region table doesn't assign REGION_ID for a given STRATUM
          select(-STRATUM) %>% # Stratum only used for region assignment 
          mutate(REGION_ID = as.character(REGION_ID))
      }
    }
    
    # Standardize survey seasons into semesters for alignment with commercial blocks
    if(!is.null(mv_sv_age) && nrow(mv_sv_age) > 0) {
      mv_sv_age <- mv_sv_age |> 
        dplyr::mutate(SEM = dplyr::case_when(SEASON == 'SPRING' ~ 1, 
                                             SEASON == 'FALL' ~ 2, 
                                             .default = NA)) |> 
        dplyr::filter(!is.na(SEM)) 
    }
    
    ###### Conditional Database Query: Observer Ages ######
    # For the time being this is a placeholder but built in a placeholder here so could add to tab_ALK$source options and placeholder added to function for borrowing logic
    mv_ob_age <- NULL
    
    ###### Regional and Temporal Data Standardization ######
    # Filter for NAs that fall strictly within our window of interest 
    unmapped_in_scope <- mv_cf_age |> 
      dplyr::filter(is.na(REGION_ID), YEAR %in% data_years) |> 
      dplyr::select(YEAR, AREA) |> 
      dplyr::distinct() |> 
      dplyr::arrange(YEAR, AREA)
    
    if (nrow(unmapped_in_scope) > 0) {
      cli::cli_alert_warning(paste0(
        "{.strong {.yellow regional mapping gap:}} Found biological samples in the following ",
        "AREA/YEARs that are within scope but missing from {.code tab_regions}. ",
        "These will remain as NAs in ALK_HOLES."
      ))
      
      # Display the specific gaps
      print(unmapped_in_scope)
      
      cli::cli_inform("{.info Suggestion: Update your stat_areas or region_custom to include these if they belong to this stock.}")
    }
    
    ###### Gap Analysis Logic ######
    # Check ALK gaps against expanded lengths since this is the product that feeds into the age expansion
    expanded_lens <- tryCatch({
      sql_expandLen <- glue::glue("SELECT YEAR, NESPP4, BLOCK_ID, REGION_ID,  LENGTH, NO_AT_LENGTH, STOCK_ABBREV 
                         FROM {schema}.mv_cf_wgt_and_no_at_length_j
                         WHERE species_itis = '{species_itis}' 
                         AND year BETWEEN {min(data_years)} AND {max(data_years)}
                         AND STOCK_ABBREV = '{stock_abbrev}'") # Ensure correct data pulled for multi-stock species
      ROracle::dbGetQuery(conn, sql_expandLen) 
    }, error = function(e) { stop("Length query failed.") })
    
    # Filter landings to find length classes that lack age samples in the chosen source(s)
    #expanded_lens <- expanded_lens %>% mutate(BLOCK_ID = substring(BLOCK_ID,2,2)) #!!! for scup testing
    alk_needs <- expanded_lens |> #expanded_lens pulls in blocking but the blocking doesn't have quarter or semester assignments so added separately so consistent downstream grouping in code even if blocking varies across applications
      # raw_lens |>
      # dplyr::mutate(SEM = dplyr::case_when(QTR %in% c(1,2) ~ 1, 
      #                                      QTR %in% c(3,4) ~ 2, 
      #                                      .default = NA)) |>
      group_by(YEAR) %>% 
      mutate(REGION_ID = as.character(REGION_ID),
             n_block = length(unique(BLOCK_ID)), 
             SEM = case_when(n_block == 1 ~ NA,               # No way to assign semester if annual blocking
                             n_block == 2 & BLOCK_ID == 1 ~ 1, 
                             n_block == 2 & BLOCK_ID == 2 ~ 2,
                             n_block == 4 & BLOCK_ID == 1 ~ 1,
                             n_block == 4 & BLOCK_ID == 2 ~ 1,
                             n_block == 4 & BLOCK_ID == 3 ~ 2,
                             n_block == 4 & BLOCK_ID == 4 ~ 2), 
             QTR = case_when(n_block == 1 ~ NA,
                             n_block == 2 & BLOCK_ID == 1 ~ NA, # No way to assign QTR to semester or annual blocking
                             n_block == 2 & BLOCK_ID == 2 ~ NA,
                             n_block == 4 & BLOCK_ID == 1 ~ 1,
                             n_block == 4 & BLOCK_ID == 2 ~ 2,
                             n_block == 4 & BLOCK_ID == 3 ~ 3,
                             n_block == 4 & BLOCK_ID == 4 ~ 4)) |> 
      dplyr::anti_join(dplyr::bind_rows(mv_cf_age) |> dplyr::filter(YEAR %in% data_years), # Don't anti_join to anything except CF so alk_needs shows year/region/semester/mkt category in the expanded lengths that do not have commercial ages (no age in CF) - borrowing procedure governs how these filled/what other data to include in ALK alongside CF (e.g. could use all SV data along with CF)
                       by = c("YEAR", "REGION_ID", "LENGTH", "NESPP4", "SEM", "QTR")) |> # this will break if "commercial" not in tab_alk$source
      dplyr::select(YEAR, NESPP4, REGION_ID, LENGTH, SEM, QTR) |> 
      dplyr::distinct()
    
    if(nrow(alk_needs) > 0) {
      borrowed_ages <- borrow_ages(missing_ages = alk_needs,
                               CF_ages = mv_cf_age,
                               SV_ages = mv_sv_age,
                               OB_ages = mv_ob_age,
                               borrow_procedure = borrow_procedure,
                               borrow_2020_option = borrow_2020_option)
      
      # Calculate the final set of missing holes
      alk_unfilled <- borrowed_ages$remaining_gaps
      
      # Final Result Consolidation
      
      # Fill gaps based on blocking
      blocked_borrowed <- borrowed_ages$all_fills |>
        left_join(select(tab_BLOCKS, YEAR, NESPP4, REGION_ID, BLOCK_TYPE), by = c("YEAR", "REGION_ID", "NESPP4"), relationship = "many-to-many") |> # Add block type for a given year, market category, and region_id 
        distinct()
      
      # Annual blocking
      annual_borrowed <- blocked_borrowed %>% 
        filter(BLOCK_TYPE == "ANNUAL") %>%
        mutate(BLOCK_ID = 1) |> # Add annual block ID
        dplyr::group_by(YEAR, NESPP4, REGION_ID, LENGTH, AGE, SOURCE, BLOCK_ID) %>% # annual group
        dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE), .groups = "drop")
      
      # Semester blocking
      if("SEM" %in% colnames(blocked_borrowed)){
        semester_borrowed <- blocked_borrowed %>% 
          filter(BLOCK_TYPE == "SEMESTER") %>%
          mutate(BLOCK_ID = as.numeric(SEM)) |> # Add semester block ID
          dplyr::group_by(YEAR, NESPP4, REGION_ID, LENGTH, AGE, SEM, SOURCE, BLOCK_ID) |> # semester group
          dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE), .groups = "drop") |> select(-SEM)
      } else{
        semester_borrowed <- NULL
      }
      
      # Quarter blocking
      if("QTR" %in% colnames(blocked_borrowed)){
        quarter_borrowed <- blocked_borrowed %>%
          filter(BLOCK_TYPE == "QUARTER") %>%
          mutate(BLOCK_ID = as.numeric(QTR)) %>% # add quarter block ID
          dplyr::group_by(YEAR, NESPP4, REGION_ID, LENGTH, AGE, QTR, SOURCE, BLOCK_ID) |> # quarter group
          dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE), .groups = "drop") |> select(-QTR)
      } else{
        quarter_borrowed <- NULL
      }
      
      
      tab_ALK_HOLES <- rbind(annual_borrowed, semester_borrowed, quarter_borrowed) %>%
        dplyr::mutate(SPECIES_ITIS = species_itis, 
                      STOCK_ABBREV = stock_abbrev, 
                      SEX_TYPE = sex_type, 
                      ASSESSMENT_ABBREV = assessment_abbrev, 
                      SA_YEAR = assessment_year, 
                      AGE_UOM = "YEAR", 
                      LENGTH_UOM = "CM") |>
        dplyr::filter(NO_AT_AGE > 0) |>
        dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, YEAR, REGION_ID, 
                      BLOCK_ID, LENGTH_UOM, LENGTH, AGE_UOM, AGE, 
                      NO_AT_AGE, NESPP4, ASSESSMENT_ABBREV, SA_YEAR, SOURCE) |>
        arrange(YEAR)
        
      
      # tab_ALK_HOLES <- borrowed_ages$all_fills |> 
      #   dplyr::group_by(YEAR, NESPP4, REGION_ID, LENGTH, AGE, SEM, SOURCE) |> 
      #   dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE), .groups = "drop") |>
      #   dplyr::mutate(SPECIES_ITIS = species_itis, 
      #                 STOCK_ABBREV = stock_abbrev, 
      #                 SEX_TYPE = sex_type, 
      #                 ASSESSMENT_ABBREV = assessment_abbrev, 
      #                 SA_YEAR = assessment_year, 
      #                 AGE_UOM = "YEAR", 
      #                 LENGTH_UOM = "CM") |>
      #   dplyr::filter(NO_AT_AGE > 0) |> 
      #   left_join(select(tab_BLOCKS, YEAR, NESPP4, REGION_ID, BLOCK_TYPE), by = c("YEAR", "REGION_ID", "NESPP4"), relationship = "many-to-many") |> # Add block type for a given year, market category, and region_id 
      #   mutate(BLOCK_ID = case_when(BLOCK_TYPE == "ANNUAL" ~ 1,
      #                               BLOCK_TYPE == "SEMESTER" ~ as.numeric(SEM),
      #                               BLOCK_TYPE == "QUARTER" ~ as.numeric(QTR))) |> # Add block id based on type
      #   dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, YEAR, REGION_ID, 
      #                 BLOCK_ID, LENGTH_UOM, LENGTH, AGE_UOM, AGE, 
      #                 NO_AT_AGE, NESPP4, ASSESSMENT_ABBREV, SA_YEAR, SOURCE) |>
      #   arrange(YEAR)
    }
  } else{
    alk_unfilled <- NA # Required for subsequent check, will not describe remaining holes unless fill_alk == TRUE
  }
  
  ### EXCLUSIONS tab
  
  tab_EXCLUSIONS <- setNames(data.frame(matrix(ncol = 21, nrow = 0)), 
                             c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", 
                               "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", "NESPP4", 
                               "AREA", "PORT", "MONTH", "DAY", "CATDISP", "SEX", 
                               "LINK", "BIOSAMP_SOURCE", "LENGTH", "LENGTH_UOM", 
                               "AGESTRCT", "AGE", "AGE_UOM", "REASON"))
  
  if(!is.null(tab_exclusions$len_range) || !is.null(tab_exclusions$age_range)) {
    c_l_low <- if(!is.null(tab_exclusions$len_range)) min(tab_exclusions$len_range) else -Inf
    c_l_high <- if(!is.null(tab_exclusions$len_range)) max(tab_exclusions$len_range) else Inf
    c_a_low <- if(!is.null(tab_exclusions$age_range)) min(tab_exclusions$age_range) else -Inf
    c_a_high <- if(!is.null(tab_exclusions$age_range)) max(tab_exclusions$age_range) else Inf
    
    if(nrow(tab_LENGTH_IMPUTATIONS) > 0) {
      r_l <- range(tab_LENGTH_IMPUTATIONS$LENGTH, na.rm = TRUE)
      if(r_l[1] < c_l_low || r_l[2] > c_l_high) {
        cli::cli_alert_danger(paste0("{.strong {.red Consistency Alert: EXCLUSION ", 
                                     "length range clips LENGTH_IMPUTATIONS.}}"))
      }
    }
    
    if(nrow(tab_ALK_HOLES) > 0) {
      r_alk_l <- range(tab_ALK_HOLES$LENGTH, na.rm = TRUE)
      r_alk_a <- range(tab_ALK_HOLES$AGE, na.rm = TRUE)
      if(r_alk_l[1] < c_l_low || r_alk_l[2] > c_l_high) {
        cli::cli_alert_danger(paste0("{.strong {.red Consistency Alert: EXCLUSION ", 
                                     "length range clips ALK_HOLES.}}"))
      }
      if(r_alk_a[1] < c_a_low || r_alk_a[2] > c_a_high) {
        cli::cli_alert_danger(paste0("{.strong {.red Consistency Alert: EXCLUSION ", 
                                     "age range clips ALK_HOLES.}}"))
      }
    }
    
    q_exc <- list()
    if(any(data_years < 2020)) {
      y_leg <- data_years[data_years < 2020]
      q_exc$legacy <- glue::glue("SELECT YEAR, NESPP4, AREA, PORT, MONTH, 
                                   CATDISP, SEX, LINK, DATA_SOURCE as BIOSAMP_SOURCE, 
                                   LENGTH, AGE, AGESTRCT, LENGTH_UOM, AGE_UOM 
                                   FROM {schema}.MV_CF_AGE_NEFSC 
                                   WHERE SPECIES_ITIS = '{species_itis}' 
                                   AND YEAR IN ({paste(y_leg, collapse = ',')})")
    }
    if(any(data_years >= 2020)) {
      y_cams <- data_years[data_years >= 2020]
      q_exc$cams <- glue::glue("SELECT YEAR, NESPP4, AREA, PORT, MONTH, 
                                 CATDISP, SEX, LINK, DATA_SOURCE as BIOSAMP_SOURCE, 
                                 LENGTH, AGE, AGESTRCT, LENGTH_UOM, AGE_UOM 
                                 FROM {schema}.MV_CF_AGE_CAMS 
                                 WHERE SPECIES_ITIS = '{species_itis}' 
                                 AND YEAR IN ({paste(y_cams, collapse = ',')})")
    }
    
    raw_bio <- purrr::map_dfr(q_exc, \(x) ROracle::dbGetQuery(conn, x))
    
    if(!is.null(raw_bio) && nrow(raw_bio) > 0) {
      tab_EXCLUSIONS <- raw_bio |> 
        dplyr::mutate(DAY = NA_real_, 
                      REASON = dplyr::case_when(
                        LENGTH < c_l_low ~ "Under Length", 
                        LENGTH > c_l_high ~ "Over Length", 
                        AGE < c_a_low ~ "Under Age", 
                        AGE > c_a_high ~ "Over Age", 
                        .default = NA_character_)) |> 
        dplyr::filter(!is.na(REASON)) |>
        dplyr::mutate(SPECIES_ITIS = species_itis, 
                      STOCK_ABBREV = stock_abbrev, 
                      SEX_TYPE = sex_type, 
                      ASSESSMENT_ABBREV = assessment_abbrev, 
                      SA_YEAR = assessment_year,
                      BIOSAMP_SOURCE = ifelse(grep("AGE",BIOSAMP_SOURCE),"AGE","LEN")
        ) |>
        dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, ASSESSMENT_ABBREV, 
                      SA_YEAR, YEAR, NESPP4, AREA, PORT, MONTH, DAY, CATDISP, 
                      SEX, LINK, BIOSAMP_SOURCE, LENGTH, LENGTH_UOM, AGESTRCT, 
                      AGE, AGE_UOM, REASON)
    }
  }
  
  
  ##### Checks #####
  cli::cli_h1("STOCKEFF Specification Summary")
  counts <- list("BLOCKS" = nrow(tab_BLOCKS), 
                 "IMPUTATIONS" = nrow(tab_LENGTH_IMPUTATIONS), 
                 "ALK_HOLES" = nrow(tab_ALK_HOLES), 
                 "EXCLUSIONS" = nrow(tab_EXCLUSIONS))
  
  purrr::iwalk(counts, \(x, y) {
    if(x > 0) cli::cli_alert_success("{y}: {x} records.") 
    else cli::cli_alert_info("{y}: Empty.")
  })
  
  if(nrow(tab_ALK_HOLES) > 0 || tab_alk$fill_alk == TRUE) { # Logic changed so always run summary if tab_ALK_HOLES was populated OR borrowing attempted (removes dependence on alk_unfilled which is only generated when borrowing attempted)
    
    # Summary of what WAS filled
    alk_summary <- tab_ALK_HOLES |> 
      dplyr::mutate(Source_Group = dplyr::case_when(
        stringr::str_detect(SOURCE, "^MATCH_EXACT")     ~ "Exact Commercial Match",
        stringr::str_detect(SOURCE, "^MKT_BORROW")      ~ "Cross-Market Commercial Fill", 
        stringr::str_detect(SOURCE, "^ANNUAL_BORROW")   ~ "Cross-Semester Commercial Fill",
        stringr::str_detect(SOURCE, "^SV_BORROW_SEM")   ~ "NEFSC Survey (Semester) Fill",
        stringr::str_detect(SOURCE, "^SV_BORROW_ANNUAL") ~ "NEFSC Survey (Annual) Fill",
        .default = "Other")) |> 
      dplyr::count(Source_Group, name = "Records")
    
    cli::cli_h2("ALK Resolution Summary")
    
    if(nrow(tab_ALK_HOLES) > 0) {
      cli::cli_alert_success("Resolved {sum(alk_summary$Records)} age-length records:")
      for(i in 1:nrow(alk_summary)) { 
        cli::cli_li("{alk_summary$Source_Group[i]}: {alk_summary$Records[i]} records") 
      }
    }
    
    # Report the failures
    if(tab_alk$fill_alk == TRUE & nrow(alk_unfilled) > 0) {
      # Count unique "holes" (Yr/Mkt/Reg/Len/Sem combinations)
      num_holes <- nrow(alk_unfilled)
      cli::cli_alert_danger("{.strong {num_holes}} length classes remain unfilled (no biological samples found in any tier).")
      
      # Optional: show the user where the biggest gaps are
      top_gaps <- alk_unfilled |> dplyr::count(YEAR, REGION_ID) |> dplyr::arrange(desc(n)) |> head(3)
      cli::cli_inform("{.info Most missing data found in: {paste(top_gaps$YEAR, 'Region', top_gaps$REGION_ID, collapse = ', ')}}")
    }
  }
  
  if(nrow(tab_EXCLUSIONS) > 0) {
    exc_summary <- tab_EXCLUSIONS |> dplyr::count(REASON)
    cli::cli_alert_warning("Biological Exclusions Flagged:")
    for(i in 1:nrow(exc_summary)) { 
      cli::cli_li("{exc_summary$REASON[i]}: {exc_summary$n[i]} records") 
    }
  }
  
  ##### Return #####
  final_list <- list("BLOCKS" = as.data.frame(tab_BLOCKS), 
                     "PRORATE" = as.data.frame(tab_PRORATE), 
                     "REGIONS" = as.data.frame(tab_REGIONS), 
                     "LW_PARAMS" = as.data.frame(tab_LW_PARAMS), 
                     "SEX_TYPE" = as.data.frame(tab_SEX_TYPE), 
                     "ALK_HOLES" = as.data.frame(tab_ALK_HOLES), 
                     "EXCLUSIONS" = as.data.frame(tab_EXCLUSIONS), 
                     "LENGTH_IMPUTATIONS" = as.data.frame(tab_LENGTH_IMPUTATIONS))
  
  writexl::write_xlsx(final_list, path = paste0(outfile, ".xlsx"))
  if (auto_disconnect) ROracle::dbDisconnect(conn)
  cli::cli_alert_success("File generated: {outfile}.xlsx")
  
  return(final_list)
} # End of function definition

?fillCF <- function() {
  if (!requireNamespace("docstring", quietly = TRUE)) {
    message("Please install 'docstring' to view help: install.packages('docstring')")
  } else {
    docstring::docstring(fillCF)
  }
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#to add from git repo
#devtools::install_github("NEFSC/READ-PDB-COLLABORATIONS", dependencies = TRUE)
#package is called as pdbCollaborations
