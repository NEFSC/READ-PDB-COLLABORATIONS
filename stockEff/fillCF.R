#' @title Fill CF specification template for STOCKEFF
#' 
#' @param species_itis 6 digit ITIS identifying the species for which data should be pulled, no default.
#' @param stock_abbrev A string describing the stock abbreviation assigned by stock efficiency (this shows up in the web address for STOCKEFF products), no default.          
#' @param assessment_abbrev A string describing the assessment abbreviation, no default. Common options include "MT", "RT", "OA"
#' @param assessment_year The year in which the assessment will be reviewed, no default.
#' @param data_years The range of years for which catch data should be formatted for STOCKEFF CF excel template, no default.
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
#'           if = "checkLENGTHS", Assumes monthly blocks were submitted to STOCKEFF, checks the number of monthly length samples from biosamp_summary.csv to determine smallest block supported by data for each market category and year. Must also provide minLenSample argument if this option is used.}
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
#' @param tab_regions A list providing the following information to populate the REGIONS tab:
#' \itemize{
#'   \item{stat_areas - A list of stat area vectors by region. Example: list("1" = c(511, 512))}
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
#' @param tab_impute A list providing rules for filling sampling gaps in the LENGTH_IMPUTATIONS tab:
#' \itemize{
#'   \item{borrow_from_region - A named list defining spatial donor relationships. 
#'         Example: list("5" = "6") means if Region 5 has a sampling hole, 
#'         borrow the length-frequency distribution from Region 6 for that same 
#'         Year and Market Category.}
#'   \item{borrow_from_time - A boolean. If TRUE, triggers the constrained temporal 
#'         hierarchy if spatial borrowing fails or is not defined. 
#'         The hierarchy follows: Same Semester (for Quarter holes) > 
#'         Same Year (for Semester holes) > Adjacent Years (for Year holes).}
#' }
#' @param tab_alk A list providing rules for filling gaps in the ALK_HOLES tab:
#' \itemize{
#'   \item{fill_alk - A boolean. If TRUE, identifies lengths present in landings that 
#'         lack corresponding age data and attempts to fill them using hierarchical borrowing.}
#'   \item{borrow_alk_years - A boolean. Default = FALSE. If TRUE, allows the ALK fill 
#'         logic to look at adjacent years (Year - 1 and Year + 1) as a final fallback 
#'         if no regional or survey ages are found for the current year.}
#'   \item{source - options are "commercial", "survey" or (default) "both". Provides the option of using only
#'        one of the available sources for age data in hole filling.}     
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
#' @param schema Set to "STOCKEFF_PRE_PROD" if using pre production data for assembly. Default is "STOCKEFF"
#' @param outfile A string for the final exported file name, does NOT need to include .xlsx extention. No default.
#' @param conn A DBI or ROracle connection object to the NEFSC Oracle database. 
#'        If NULL (default), the function will prompt for credentials via rstudioapi 
#'        and manage the connection/disconnection automatically. If a connection 
#'        is provided, the user is responsible for closing it after the function executes.
#'         
#' \itemize{
#' \item{NOTE: tab_blocks$block_opt = checkLENGTHS will NOT work unless connected to the VPN}
#' \item{NOTE: ALK_HOLES, and EXCLUSIONS tabs are optionally populated by this function and may need to be filled externally if tab_alk and tab_exclusions arguments are not used to fill gaps.}
#' \item{NOTE: If you receive the following error, then the .xlsx file by the provided name already exists and can't be overwritten: [ERROR] workbook_close(): Error creating 'outfile.xlsx'. System error = Permission denied Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.'}
#' \item{NOTE: If you find NAs in the REGION_ID for the BLOCKS tab, then there was a stat area in the {schema}.mv_cf_stock_data_length_o product that was not provided in the tab_regions$stat_areas argument.}
#' }
#' 
#'
#' @return A populated .xlsx template and a list containing the following:
#' \itemize{
#'   \item{mkt_missing_yr - A table of market categories years for which 1+ market categories were not sampled where 1+ market categories were not sampled}
#'   \item{regions_missing - Table of AREAs that appear in oracle data but not in tab_regions$stat_area argument for this function (may be missing blocking)}
#' }
#' 
#' @export
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
#'     borrow_from_time = TRUE
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
#'   borrow_from_time = TRUE # Flag to trigger the temporal fallback
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
                   tab_impute = NULL,
                   tab_alk = list(fill_alk = FALSE, 
                                  borrow_alk_years = FALSE,
                                  source = "both"), # Options: "both", "commercial", "survey"
                   tab_exclusions = list(len_range = NULL, 
                                         age_range = NULL),
                   schema = "STOCKEFF",
                   outfile = NULL,
                   conn = NULL){
  
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
  
  if(is.null(tab_blocks$autofillLW)) tab_blocks$autofillLW <- FALSE
  if(is.null(tab_blocks$check_missing)) tab_blocks$check_missing <- FALSE
  if(is.null(tab_blocks$mkt_missing)) tab_blocks$mkt_missing <- "keep"
  
  ### Tab SEX_TYPE
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
  
  ### Tab PRORATE
  
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
  
  ### REGIONS Tab 
  tab_REGIONS <- NULL
  
  if (!is.null(tab_regions$region_custom)) {
    cli::cli_alert_info("Using custom region mapping provided in 'region_custom'.")
    tab_REGIONS <- as.data.frame(tab_regions$region_custom)
    
    # Standardize common column naming variations
    if ("STAT_AREA" %in% names(tab_REGIONS)) {
      tab_REGIONS <- dplyr::rename(tab_REGIONS, AREA = STAT_AREA)
    }
    if ("area" %in% names(tab_REGIONS)) {
      tab_REGIONS <- dplyr::rename(tab_REGIONS, AREA = area)
    }
    
  } else if (!is.null(tab_regions$stat_areas)) {
    # Standard hierarchical mapping from list for simple static cases
    region_mapping <- purrr::imap_dfr(tab_regions$stat_areas, 
                                      \(x, y) data.frame(AREA = as.character(x), 
                                                         REGION_ID = as.character(y)))
    
    tab_REGIONS <- tidyr::expand_grid(AREA = region_mapping$AREA, 
                                      YEAR = data_years) |>
      dplyr::left_join(region_mapping, by = "AREA")
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
  
  
  ### LW_PARAMS tab
  
  tab_LW_PARAMS <- as.data.frame(tab_lw) |>
    dplyr::mutate(SPECIES_ITIS = species_itis, 
                  STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type,
                  ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year) |> 
    dplyr::select(SPECIES_ITIS,STOCK_ABBREV,SEX_TYPE,ASSESSMENT_ABBREV,SA_YEAR
                  ,ALPHA,BETA,SOURCE,LW_TYPE,LW_ID,MONTH_START,MONTH_END,YEAR_START,YEAR_END)
  
  ### BLOCKS tab start
  
  ##  if(tab_blocks$block_opt == "checkLENGTHS"){  ### Sam's suggested edit, moved this line
  #cli::cli_alert_info("Querying length-frequency data for {species_itis}...")
  target_nespp4 <- stringr::str_pad(as.character(tab_blocks$nespp4), 
                                    width = 4, side = "left", pad = "0")
  
  raw_lens <- tryCatch({
    sql1 <- glue::glue("SELECT YEAR, NESPP4, AREA, QTR, LENGTH, NO_AT_LENGTH 
                         FROM {schema}.mv_cf_stock_data_length_o 
                         WHERE species_itis = '{species_itis}' 
                         AND year BETWEEN {min(data_years)} AND {max(data_years)}")
    ROracle::dbGetQuery(conn, sql1)
  }, error = function(e) { stop("Length query failed.") })
  
  raw_lens <- raw_lens |> 
    dplyr::mutate(NESPP4 = as.character(NESPP4), 
                  AREA = stringr::str_trim(as.character(AREA)),
                  YEAR = as.numeric(YEAR), 
                  LENGTH = as.numeric(LENGTH)) |>
    dplyr::filter(NESPP4 %in% target_nespp4) |>
    dplyr::left_join(tab_REGIONS |> dplyr::select(AREA, YEAR, REGION_ID), 
                     by = c("AREA", "YEAR"))
  
  sampling_summary <- raw_lens |>
    dplyr::group_by(YEAR, NESPP4, REGION_ID) |>
    dplyr::summarise(
      q_pass = all(purrr::map_lgl(1:4, 
                                  \(x) sum(NO_AT_LENGTH[QTR == x], na.rm = TRUE) >= tab_blocks$minLenSample)),
      s_pass = (sum(NO_AT_LENGTH[QTR %in% c(1,2)], na.rm = TRUE) >= tab_blocks$minLenSample & 
                  sum(NO_AT_LENGTH[QTR %in% c(3,4)], na.rm = TRUE) >= tab_blocks$minLenSample),
      a_pass = (sum(NO_AT_LENGTH, na.rm = TRUE) >= tab_blocks$minLenSample), 
      .groups = "drop"
    ) |>
    dplyr::mutate(BLOCK_TYPE = dplyr::case_when(
      is.na(REGION_ID) ~ "Insufficient_Samples", 
      q_pass ~ "QUARTER", 
      s_pass ~ "SEMESTER", 
      a_pass ~ "ANNUAL", 
      .default = "Insufficient_Samples"
    ))
  
  if(tab_blocks$block_opt == "checkLENGTHS"){    
    tempBlock <- sampling_summary |>
      dplyr::filter(BLOCK_TYPE != "Insufficient_Samples") |>
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
  } else {
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
  
  ### LENGTH_IMPUTATIONS tab
  
  tab_LENGTH_IMPUTATIONS <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), 
                                     c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", 
                                       "ASSESSMENT_ABBREV", "SA_YEAR", "YEAR", 
                                       "NESPP4", "REGION_ID", "BLOCK_ID", 
                                       "LENGTH_UOM", "LENGTH", "NO_AT_LENGTH", "SOURCE"))
  
  if(!is.null(tab_impute)) {
    cli::cli_alert_info("Scanning for sampling gaps using hierarchical borrowing...")
    raw_lens <- raw_lens |> 
      dplyr::mutate(SEM = dplyr::case_when(QTR %in% c(1,2) ~ 1, 
                                           QTR %in% c(3,4) ~ 2, 
                                           .default = NA))
    holes <- sampling_summary |> 
      dplyr::filter(!is.na(REGION_ID), 
                    BLOCK_TYPE == "Insufficient_Samples") |> 
      dplyr::select(YEAR, NESPP4, REGION_ID) |> 
      dplyr::distinct()
    
    if(nrow(holes) > 0) {
      imputed_data_list <- list()
      for(i in 1:nrow(holes)) {
        h_yr <- holes$YEAR[i]; h_mkt <- holes$NESPP4[i]; h_reg <- holes$REGION_ID[i]
        donor_data <- NULL
        if(!is.null(tab_impute$borrow_from_region)) {
          s_reg <- tab_impute$borrow_from_region[[as.character(h_reg)]]
          if(!is.null(s_reg)) {
            donor_data <- raw_lens |> 
              dplyr::filter(YEAR == h_yr, NESPP4 == h_mkt, REGION_ID == s_reg) |> 
              dplyr::mutate(SOURCE = paste0("IMPUTE_REG_", s_reg))
          }
        }
        if(is.null(donor_data) || nrow(donor_data) == 0) {
          target_sem <- ifelse(any(tempBlock$MONTH_START[tempBlock$YEAR == h_yr] <= 6), 1, 2)
          donor_data <- raw_lens |> 
            dplyr::filter(YEAR == h_yr, NESPP4 == h_mkt, REGION_ID == h_reg, SEM == target_sem) |> 
            dplyr::mutate(SOURCE = paste0("IMPUTE_TIME_SEM_", target_sem))
          if(nrow(donor_data) == 0) {
            donor_data <- raw_lens |> 
              dplyr::filter(YEAR == h_yr, NESPP4 == h_mkt, REGION_ID == h_reg) |> 
              dplyr::mutate(SOURCE = "IMPUTE_TIME_ANNUAL")
          }
          if(nrow(donor_data) == 0) {
            donor_data <- raw_lens |> 
              dplyr::filter(YEAR %in% c(h_yr-1, h_yr+1), NESPP4 == h_mkt, REGION_ID == h_reg) |> 
              dplyr::mutate(SOURCE = "IMPUTE_TIME_ADJ_YR")
          }
        }
        if(!is.null(donor_data) && nrow(donor_data) > 0) {
          donor_agg <- donor_data |> 
            dplyr::group_by(LENGTH, SOURCE) |> 
            dplyr::summarise(NO_AT_LENGTH = sum(NO_AT_LENGTH, na.rm = TRUE), .groups = "drop") |>
            dplyr::mutate(YEAR = h_yr, NESPP4 = h_mkt, REGION_ID = h_reg, 
                          SPECIES_ITIS = species_itis, STOCK_ABBREV = stock_abbrev, 
                          SEX_TYPE = sex_type, ASSESSMENT_ABBREV = assessment_abbrev, 
                          SA_YEAR = assessment_year, LENGTH_UOM = "CM", BLOCK_ID = "1")
          imputed_data_list[[length(imputed_data_list) + 1]] <- donor_agg
        }
      }
      if(length(imputed_data_list) > 0) {
        tab_LENGTH_IMPUTATIONS <- dplyr::bind_rows(imputed_data_list) |> 
          dplyr::select(dplyr::all_of(names(tab_LENGTH_IMPUTATIONS)))
      }
    }
  }
  
  ### ALK_HOLES tab
  
  tab_ALK_HOLES <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                            c("SPECIES_ITIS", "STOCK_ABBREV", "SEX_TYPE", "YEAR", 
                              "REGION_ID", "BLOCK_ID", "LENGTH_UOM", "LENGTH", 
                              "AGE_UOM", "AGE", "NO_AT_AGE", 
                              "NESPP4", "ASSESSMENT_ABBREV", "SA_YEAR", "SOURCE"))
  
  if(isTRUE(tab_alk$fill_alk)) {
    
    ### Source Option Determination
    # Determine the user's preferred data source for age data
    alk_source_opt <- if(is.null(tab_alk$source)) "both" else tab_alk$source
    cli::cli_alert_info("Identifying ALK holes via Hybrid Year Logic (Source: {alk_source_opt})...")
    
    #search_yrs <- (min(data_years) - 1):(max(data_years) + 1)
    #Remove the +1 / -1 buffer. Only search what the user defined.
    search_yrs <- data_years 
    
    ### Conditional Database Query: Commercial Ages
    mv_cf_age <- NULL
    if(alk_source_opt %in% c("both", "commercial")) {
      mv_cf_age <- tryCatch({
        q_parts <- list()
        # Filter strictly for search_yrs
        if(any(search_yrs < 2020)) {
          y_leg <- search_yrs[search_yrs < 2020]
          q_parts$legacy <- glue::glue("SELECT YEAR, NESPP4, AREA, QTR, LENGTH, 
                                        AGE, NUMAGE as NO_AT_AGE, DOCN as SOURCE 
                                        FROM {schema}.MV_CF_AGE_NEFSC 
                                        WHERE SPECIES_ITIS = '{species_itis}' 
                                        AND YEAR IN ({paste(y_leg, collapse = ',')})")
        }
        if(any(search_yrs >= 2020)) {
          y_cams <- search_yrs[search_yrs >= 2020]
          q_parts$cams <- glue::glue("SELECT YEAR, NESPP4, AREA, PORT, MONTH, 
                                    CATDISP, SEX, LINK, DATA_SOURCE as BIOSAMP_SOURCE, 
                                    LENGTH, AGE, AGESTRCT, LENGTH_UOM, AGE_UOM 
                                    FROM {schema}.MV_CF_AGE_CAMS 
                                    WHERE SPECIES_ITIS = '{species_itis}' 
                                    AND YEAR IN ({paste(y_cams, collapse = ',')})")
        }
        purrr::map_dfr(q_parts, \(x) ROracle::dbGetQuery(conn, x))
      }, error = function(e) { return(NULL) })
    }
    
    ### Conditional Database Query: Survey Ages
    # Query survey age database if option is 'both' or 'survey'
    mv_sv_age <- NULL
    if(alk_source_opt %in% c("both", "survey")) {
      mv_sv_age <- tryCatch({
        sql_sv <- glue::glue("SELECT YEAR, SEASON, LENGTH, AGE, AGE_NO as NO_AT_AGE, 
                              CRUISE6 as SOURCE FROM {schema}.MV_SV_AGE 
                              WHERE SPECIES_ITIS = '{species_itis}' 
                              AND YEAR BETWEEN {min(data_years) - 1} AND {max(data_years) + 1}")
        ROracle::dbGetQuery(conn, sql_sv)
      }, error = function(e) { return(NULL) })
    }
    
    ### Regional and Temporal Data Standardization
    # Join commercial ages with regional definitions and calculate semesters
    if(!is.null(mv_cf_age) && nrow(mv_cf_age) > 0) {
      mv_cf_age <- mv_cf_age |> 
        dplyr::mutate(YEAR = as.numeric(YEAR), 
                      AREA = stringr::str_trim(as.character(AREA))) |>
        dplyr::left_join(tab_REGIONS |> dplyr::select(AREA, YEAR, REGION_ID), 
                         by = c("AREA", "YEAR")) |>
        dplyr::mutate(SEM = dplyr::case_when(QTR %in% c(1,2) ~ 1, 
                                             QTR %in% c(3,4) ~ 2, 
                                             .default = NA))
    }
    
    # Filter for NAs that fall strictly within our window of interest
    unmapped_in_scope <- mv_cf_age |> 
      dplyr::filter(is.na(REGION_ID), YEAR %in% search_yrs) |> 
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
    
    # Standardize survey seasons into semesters for alignment with commercial blocks
    if(!is.null(mv_sv_age) && nrow(mv_sv_age) > 0) {
      mv_sv_age <- mv_sv_age |> 
        dplyr::mutate(SEM = dplyr::case_when(SEASON == 'SPRING' ~ 1, 
                                             SEASON == 'FALL' ~ 2, 
                                             .default = NA)) |> 
        dplyr::filter(!is.na(SEM))
    }
    
    ### Gap Analysis Logic
    # Filter landings to find length classes that lack age samples in the chosen source(s)
    alk_needs <- raw_lens |>
      dplyr::mutate(SEM = dplyr::case_when(QTR %in% c(1,2) ~ 1, 
                                           QTR %in% c(3,4) ~ 2, 
                                           .default = NA)) |>
      dplyr::anti_join(dplyr::bind_rows(mv_cf_age, mv_sv_age) |> dplyr::filter(YEAR %in% data_years), 
                       by = c("YEAR", "REGION_ID", "LENGTH", "NESPP4", "SEM")) |>
      dplyr::select(YEAR, NESPP4, REGION_ID, LENGTH, SEM) |> 
      dplyr::distinct()
    
    if(nrow(alk_needs) > 0) {
      
      # Initialize tiers as empty data frames
      fill_t1 <- fill_t2 <- fill_t3 <- fill_t4 <- fill_t5 <- data.frame()
      
      donor_pool <- mv_cf_age |> 
        dplyr::filter(YEAR %in% data_years) |>
        dplyr::mutate(SOURCE_BASE = SOURCE)
      
      # --- TIER 1: Match Everything ---
      fill_t1 <- alk_needs |>
        dplyr::inner_join(donor_pool, 
                          by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"),
                          relationship = "many-to-many") |>
        dplyr::mutate(SOURCE = paste0("MATCH_EXACT_", SOURCE_BASE))
      
      # Track cumulative fills
      all_fills <- fill_t1
      
      # --- TIER 2: Drop NESPP4 ---
      # Remove everything in all_fills from alk_needs to get the next batch of holes
      still_needed <- dplyr::anti_join(alk_needs, all_fills, 
                                       by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))
      
      if(nrow(still_needed) > 0) {
        fill_t2 <- still_needed |>
          dplyr::inner_join(donor_pool |> dplyr::select(-NESPP4) |> dplyr::distinct(), 
                            by = c("YEAR", "LENGTH", "SEM", "REGION_ID"),
                            relationship = "many-to-many") |>
          dplyr::mutate(SOURCE = paste0("MKT_BORROW_", SOURCE_BASE))
        
        all_fills <- dplyr::bind_rows(all_fills, fill_t2)
      }
      
      # --- TIER 3: Drop SEMESTER ---
      still_needed <- dplyr::anti_join(alk_needs, all_fills, 
                                       by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))
      
      if(nrow(still_needed) > 0) {
        annual_donors <- donor_pool |> 
          dplyr::group_by(YEAR, REGION_ID, LENGTH, AGE) |> 
          dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE),
                           SOURCE_BASE = dplyr::first(SOURCE_BASE), .groups = "drop")
        
        fill_t3 <- still_needed |>
          dplyr::inner_join(annual_donors, 
                            by = c("YEAR", "LENGTH", "REGION_ID"),
                            relationship = "many-to-many") |>
          dplyr::mutate(SOURCE = paste0("ANNUAL_BORROW_", SOURCE_BASE))
        
        all_fills <- dplyr::bind_rows(all_fills, fill_t3)
      }
      
      # --- TIER 4: Survey (Semester) ---
      still_needed <- dplyr::anti_join(alk_needs, all_fills, 
                                       by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))
      
      if(!is.null(mv_sv_age) && nrow(still_needed) > 0) {
        fill_t4 <- still_needed |>
          dplyr::inner_join(mv_sv_age |> dplyr::filter(YEAR %in% data_years), 
                            by = c("YEAR", "LENGTH", "SEM"),
                            relationship = "many-to-many") |>
          dplyr::mutate(SOURCE = paste0("SV_BORROW_SEM_", SOURCE))
        
        all_fills <- dplyr::bind_rows(all_fills, fill_t4)
      }
      
      # --- TIER 5: Survey (Annual) ---
      still_needed <- dplyr::anti_join(alk_needs, all_fills, 
                                       by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))
      
      if(!is.null(mv_sv_age) && nrow(still_needed) > 0) {
        sv_annual <- mv_sv_age |>
          dplyr::filter(YEAR %in% data_years) |>
          dplyr::group_by(YEAR, LENGTH, AGE) |>
          dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE),
                           SOURCE = dplyr::first(SOURCE), .groups = "drop")
        
        fill_t5 <- still_needed |>
          dplyr::inner_join(sv_annual, 
                            by = c("YEAR", "LENGTH"),
                            relationship = "many-to-many") |>
          dplyr::mutate(SOURCE = paste0("SV_BORROW_ANNUAL_", SOURCE))
        
        all_fills <- dplyr::bind_rows(all_fills, fill_t5)
      }
      
      # Calculate the final set of missing holes
      alk_unfilled <- dplyr::anti_join(alk_needs, all_fills, 
                                       by = c("YEAR", "NESPP4", "REGION_ID", "LENGTH", "SEM"))
      
      # Final Result Consolidation
      tab_ALK_HOLES <- dplyr::bind_rows(fill_t1, fill_t2, fill_t3, fill_t4, fill_t5) |> 
        dplyr::group_by(YEAR, NESPP4, REGION_ID, LENGTH, AGE, SEM, SOURCE) |> 
        dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(SPECIES_ITIS = species_itis, 
                      STOCK_ABBREV = stock_abbrev, 
                      SEX_TYPE = sex_type, 
                      ASSESSMENT_ABBREV = assessment_abbrev, 
                      SA_YEAR = assessment_year, 
                      AGE_UOM = "YEAR", 
                      LENGTH_UOM = "CM", 
                      BLOCK_ID = "1") |>
        dplyr::filter(NO_AT_AGE > 0) |> 
        dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, YEAR, REGION_ID, 
                      BLOCK_ID, LENGTH_UOM, LENGTH, AGE_UOM, AGE, 
                      NO_AT_AGE, NESPP4, ASSESSMENT_ABBREV, SA_YEAR, SOURCE)
    }
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
  
  ### BLOCKS tab completion
  
  tab_BLOCKS <- tempBlock |> 
    dplyr::group_by(YEAR, NESPP4, REGION_ID) |> 
    dplyr::mutate(BLOCK_ID = dplyr::row_number()) |> 
    dplyr::ungroup() |>
    dplyr::mutate(SPECIES_ITIS = species_itis, STOCK_ABBREV = stock_abbrev, 
                  SEX_TYPE = sex_type, ASSESSMENT_ABBREV = assessment_abbrev, 
                  SA_YEAR = assessment_year, LW_ID = NA_character_)
  
  if (isTRUE(tab_blocks$autofillLW)) {
    
    # Calculate the terminal year of the current data range to fill gaps
    max_data_yr <- max(as.numeric(data_years), na.rm = TRUE)
    
    # Standardize types and fill missing year ceilings
    tab_LW_clean <- tab_LW_PARAMS |> 
      dplyr::mutate(dplyr::across(
        dplyr::any_of(c("MONTH_START", "MONTH_END", "YEAR_START", "YEAR_END")), 
        as.numeric
      )) |> 
      dplyr::mutate(YEAR_END = tidyr::replace_na(YEAR_END, max_data_yr)) |>
      dplyr::arrange(dplyr::desc(LW_TYPE == "ANNUAL"))
    
    ### Temporal Intersection Check
    # We loop through the standardized parameters and check for full containment
    # Using as.numeric on the blocks side ensures we don't have character mismatches
    for (i in seq_len(nrow(tab_LW_clean))) {
      
      match_indices <- which(
        as.numeric(tab_BLOCKS$MONTH_START) >= tab_LW_clean$MONTH_START[i] & 
          as.numeric(tab_BLOCKS$MONTH_END)   <= tab_LW_clean$MONTH_END[i]   & 
          as.numeric(tab_BLOCKS$YEAR)        >= tab_LW_clean$YEAR_START[i]  & 
          as.numeric(tab_BLOCKS$YEAR)        <= tab_LW_clean$YEAR_END[i]
      )
      
      # Update the LW_ID for all blocks meeting the overlap criteria
      if (length(match_indices) > 0) {
        tab_BLOCKS$LW_ID[match_indices] <- as.character(tab_LW_clean$LW_ID[i])
      }
    }
  }
  
  tab_BLOCKS <- tab_BLOCKS |> 
    dplyr::select(SPECIES_ITIS, STOCK_ABBREV, SEX_TYPE, 
                  ASSESSMENT_ABBREV, SA_YEAR, YEAR, NESPP4, REGION_ID, 
                  BLOCK_ID, BLOCK_TYPE, MONTH_START, MONTH_END, LW_ID)
  
  cli::cli_h1("STOCKEFF Specification Summary")
  counts <- list("BLOCKS" = nrow(tab_BLOCKS), 
                 "IMPUTATIONS" = nrow(tab_LENGTH_IMPUTATIONS), 
                 "ALK_HOLES" = nrow(tab_ALK_HOLES), 
                 "EXCLUSIONS" = nrow(tab_EXCLUSIONS))
  
  purrr::iwalk(counts, \(x, y) {
    if(x > 0) cli::cli_alert_success("{y}: {x} records.") 
    else cli::cli_alert_info("{y}: Empty.")
  })
  
  if(nrow(tab_ALK_HOLES) > 0 || nrow(alk_unfilled) > 0) {
    
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
    if(nrow(alk_unfilled) > 0) {
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
}

?fillCF <- function() {
  if (!requireNamespace("docstring", quietly = TRUE)) {
    message("Please install 'docstring' to view help: install.packages('docstring')")
  } else {
    docstring::docstring(fillCF)
  }
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
