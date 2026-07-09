#' @title calc_caa_waa
#' @description Calculate annual catch- and weight-at-age, using expanded lengths from stockEff and ALK with time blocking that matches stockEff blocking
#'
#' NOTE: If you receive the following error try setting connection = NULL and rerun function to re-establish oracle connection:
#'       Error: unable to find an inherited method for function ‘dbGetQuery’ for signature ‘conn = "NULL", statement = "character"’
#'
#' @param ALK A long-format table containing the following columns:
#' \itemize{
#'   \item{YEAR}
#'   \item{LENGTH - Length bin}
#'   \item{AGE - Age bin}
#'   \item{PROP - proportion-at-age for a given length and YEAR}
#'   \item{BLOCK_ID - Block ID used to match ALK to expanded lengths from stockEff}
#' }
#' @param stockEff_mode String specifying version of stockEff to query, options include: "test", "prod". Default = "prod"
#' @param stockEff_module A string indicating the module for which products will be pulled, no default. Options include:
#' \itemize{
#'   \item{"survey" - Correspond to SV tab in stockEff}
#'   \item{"commercial" - Correspond to CF tab in stockEff}
#'   \item{"observer" - Correspond to OB tab in stockEff}
#'   \item{Nothing yet available for MRIP tab (as of 3/6/24)}
#' }
#' @param species_itis Species itis code
#' @param connection A DBI or ROracle connection object to the NEFSC Oracle database.
#'        If NULL (default), the function will prompt for credentials via rstudioapi
#'        and manage the connection/disconnection automatically. If a connection
#'        is provided, the user is responsible for closing it after the function executes.

calc_caa_waa <- function(ALK = NULL,
                              stockEff_mode = "prod",
                              stockEff_module = NULL,
                              species_itis = NULL,
                              connection = NULL){

  # Setup
  if(stockEff_mode == "test"){
    mode_abbrev = "_pre_prod"
  } else{
    mode_abbrev = ""
  }

  if(exists("connection") == FALSE | is.null(connection) == TRUE){ # Establish Oracle connection if not already loaded into object called "connection"
    connection <- dbConnect(drv = dbDriver("Oracle"),
                            username = rstudioapi::askForPassword("Oracle user name"),
                            password = rstudioapi::askForPassword("Oracle password"),
                            dbname = rstudioapi::askForPassword("Oracle database name"))
   }

  # Pull lengths by year, NESPP4, region, and stockEff block, includes length-weight parameters associated with each block
  if(stockEff_module == "commercial"){ # Query commercial landings
    mv_noatlen <- ROracle::dbGetQuery(connection, statement = paste0("select * from stockeff", mode_abbrev, ".mv_cf_wgt_and_no_at_length_j where species_itis = '", species_itis, "'")) %>%
      mutate(YEAR = as.numeric(YEAR),
             LENGTH = as.numeric(LENGTH))
  } else if(stockEff_module == "observer"){ # Query commercial discards
    mv_noatlen <- ROracle::dbGetQuery(connection, statement = paste0("select * from stockeff", mode_abbrev, ".MV_OB_WTG_AND_NO_AT_LENGTH_J where species_itis = '", species_itis, "'")) %>%
      mutate(YEAR = as.numeric(YEAR),
             LENGTH = as.numeric(LENGTH))
  }

  ROracle::dbDisconnect(connection) # Disconnect from oracle once query complete


  # Calculate annual catch- and weight-at-age
  caa_waa <- left_join(mv_noatlen, ALK, by = c("YEAR", "BLOCK_ID", "LENGTH"), relationship = "many-to-many") %>%
                # many-to-many because prop-at-age provided for each length in long form so multiple ages for each length
                # NA in AGE means there is a gap in the provided ALK (observed length in catch but no age assigned)
                #
    mutate(NO_AT_LENGTH_AGE = NO_AT_LENGTH*PROP, # Age expansion by stockEff block
           WT_AT_LENGTH_AGE = NO_AT_LENGTH_AGE*IND_AVG_WT_KG) %>% # Calculate weight at length and age based on LW relationship
    group_by(YEAR, AGE) %>% # Calculate annual catch- and weight-at-age
    dplyr::reframe(TOT_NO_AT_AGE = sum(NO_AT_LENGTH_AGE), # Annual number of fish at AGE based on ALK
                   TOT_WT_AT_AGE = sum(WT_AT_LENGTH_AGE)) %>% # Annual weight of fish at AGE based on ALK
    mutate(AVG_WT_KG = TOT_WT_AT_AGE/TOT_NO_AT_AGE)

  # Return
  return(caa_waa)
}

