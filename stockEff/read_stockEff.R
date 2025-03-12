#' @title read_stockEff
#' @description Function reads stockEff products into R and saves as a time-stamped list. 
#' Options allow data to be pulled for different species, stocks, production status, modules, and sex types. 
#' This is essentially an R function wrapper around the "Connecting to StockEff via R" example user guide in confluence.
#' 
#' @param doLogin A boolean, if TRUE then function will ask for login credentials, if FALSE then function assumes connection has already been made to stockEff and code will use existing global "login" object to run (good option if nesting multiple calls within same R script). Default = TRUE
#' @param species_itis 6 digit ITIS identifying the species for which data should be pulled, no default.
#' @param stock_abbrev A string describing the stock abbreviation assigned by stock efficiency (this shows up in the web address for STOCKEFF products), no default.          
#' @param sex_type A string describing the sex type for which data should be pulled (this shows up in the web address for STOCKEFF products), default = "NONE". Common options include "NONE", "MALE", "FEMALE", "UNSEXED" 
#' @param mode A string indicating production "prod" or pre-production "test" modes, default = "prod"
#' @param module A string indicating the module for which products will be pulled, no default. Options include: 
#' \itemize{
#'   \item{"survey" - Correspond to SV tab in stockEff}
#'   \item{"commercial" - Correspond to CF tab in stockEff}
#'   \item{"observer" - Correspond to OB tab in stockEff}
#'   \item{Nothing yet available for MRIP tab (as of 3/6/24)}
#' }
#' @param product A vector of strings corresponding to the CSV files available in the SV modual of STOCKEFF, names should NOT need to include ".csv" extensions. No default.
#' @param outdir A directory where temporary files will be stored during processing (deleted when function finishes running), and final RDS object will be saved. Default to here::here(). 
#' @param saveout A boolean, if TRUE also provide outname argument for saved copy of output, otherwise results returned to R but not saved, default = TRUE.
#' @param outname A string to use as the output file name (will always be time stamped, do NOT add file extension to name), default = paste0("stockEff_",module) with timestamp.
#' 
#' @return An R list object where each item in the list corresponds to a product that was pulled from stockEff and an additional date_pulled object with the time stamp. If saveout = TRUE then this object is also saved as an RData file. When returned RData file is loaded into R it will automatically be named "stockEff_storage"
#' 
#' @examples
#' # GB cod survey pre-production survey example
#' read_stockEff(species_itis = 164712, stock_abbrev = "GBK", sex_type = "NONE", module = "survey", mode = "test", product = c("assessment_information", "strat_mean", "strat_mean_length"), outname = "example_GBcod_survey")
#' # Female spiny dogfish production survey example
#' read_stockEff(species_itis = 160617, stock_abbrev = "UNIT", sex_type = "FEMALE", module = "survey", mode = "prod", product = c("assessment_information", "strat_mean", "strat_mean_length"), outname = "example_FEMALEdogfish_survey")
#' # Butterfish production CF example
#' read_stockEff(doLogin = FALSE, species_itis = 172567, stock_abbrev = "UNIT", sex_type = "NONE", module = "commercial", mode = "prod", product = c("assessment_info", "landings_gear", "bio_data_summary"), outname = "example_butterfish_CF")


read_stockEff <- function(doLogin = TRUE, 
                          species_itis = NULL,
         stock_abbrev = NULL, 
         sex_type = "NONE",
         mode = "prod",
         module = NULL,
         product = NULL,
         outdir = here::here(),
         saveout = TRUE,
         outname = NULL){

  library(httr)

  if(doLogin == TRUE){ # Ask for credentials and establish connection, otherwise assumes connection already exists (e.g. if you don't want to type the credentials every time you call this function within the same script)
    # Log in to stockEff - prompts user to enter username and password
    login <- list(
      username = rstudioapi::askForPassword("Enter stockEff user name"),
      password = rstudioapi::askForPassword("Enter password"),
      submit = "true"
    )
    # POST("https://nefsctest.nmfs.local/stockeff/html/pub/index.php", body = login, encode = "form", verbose())
    POST("https://internal.nefsc.noaa.gov/stockeff/pub/index.php", body = login, encode = "form", verbose())
  }


  # Set up storage
  stockEff_storage <- NULL


  # Pull selected products from stock eff CSV files
  for(iproduct in 1:length(product)){
    # Pull data as binary file & write to temporary csv
    # res = GET(paste0("https://nefsctest.nmfs.local/stockeff/html/pub/index.php?c=products&product=", product[iproduct], "&module=", module, "&species_itis=",species_itis,"&stock_abbrev=",stock_abbrev,"&sex_type=",sex_type,"&mode=",mode,"&source=all&type=csv"))
    res = GET(paste0("https://internal.nefsc.noaa.gov/stockeff/pub/index.php?c=products&product=", product[iproduct], "&module=", module, "&species_itis=",species_itis,"&stock_abbrev=",stock_abbrev,"&sex_type=",sex_type,"&mode=",mode,"&source=all&type=csv"))
    bin <- content(res, "raw")
    writeBin(bin, paste0(outdir,"/temp_stockEff.csv")) ## Name your file something meaningful here if you want to reference outside of R.

    # Read temporary csv into storage object
    stockEff_storage[[iproduct]] <- read.csv(paste0(outdir,"/temp_stockEff.csv"))

    # Remove temporary file
    file.remove(paste0(outdir,"/temp_stockEff.csv"))

  }

  # Name storage objects in order they were added
  names(stockEff_storage) <- product

  # Add date data was stored
  stockEff_storage$pull_date <- Sys.time() %>% str_split_i(., " ", i=1)

  # Save final storage object as RDS
  if(saveout ==TRUE){
    if(is.null(outname) == TRUE){
      outname <- paste0("stockEff_", module, "_", stockEff_storage$pull_date, ".RData")
    } else{
      outname <- paste0(outname, "_", stockEff_storage$pull_date, ".RData")
    }
    save(stockEff_storage, file = paste(outdir, outname, sep="/"))
  }


  # Return storage object so it can also be assigned to an R object and used immediately
  return(stockEff_storage)
}
