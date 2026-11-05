#' @title borrow_lengths
#' @description Define borrowing logic for length imputations
#'
#' @param missing_lengths A table containing the following information on lengths that require imputations:
#' \itemize{
#'   \item{}
#' }
#' @param borrow_procedure A string specifying the borrowing logic to apply to provide length imputations, options include:
#' \itemize{
#'   \item{"default" - Default, Borrow: }  
#' }

borrow_lengths <- function(missing_lengths,
                           borrow_procedure,
                           
                           borrow_from_region,
                           raw_lens,
                           tempBlock, 
                           
                           ...){
  # For testing
  missing_lengths = holes
  
  
  if(borrow_procedure == "default"){
    imputed_data_list <- list()
    for(ihole in 1:nrow(missing_lengths)) {
      h_yr <- missing_lengths$YEAR[ihole] 
      h_mkt <- missing_lengths$NESPP4[ihole] 
      h_reg <- missing_lengths$REGION_ID[ihole]
      donor_data <- NULL
      if(!is.null(tab_impute$borrow_from_region)) {
        s_reg <- tab_impute$borrow_from_region[[as.character(h_reg)]]
        if(!is.null(s_reg)) {
          donor_data <- raw_lens |> 
            dplyr::filter(YEAR == h_yr, NESPP4 == h_mkt, REGION_ID == s_reg) |> 
            dplyr::mutate(SOURCE = paste0("IMPUTE_REG_", s_reg))
        }
      }
      if(is.null(donor_data) || nrow(donor_data) == 0) { # If donor_data NOT established using borrow_from_region logic
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
      
      if(!is.null(donor_data) && nrow(donor_data) > 0) { # Format donor_data
        donor_agg <- donor_data |> 
          dplyr::group_by(LENGTH, SOURCE) |> 
          dplyr::summarise(NO_AT_LENGTH = sum(NO_AT_LENGTH, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(YEAR = h_yr, NESPP4 = h_mkt, REGION_ID = h_reg, 
                        SPECIES_ITIS = species_itis, STOCK_ABBREV = stock_abbrev, 
                        SEX_TYPE = sex_type, ASSESSMENT_ABBREV = assessment_abbrev, 
                        SA_YEAR = assessment_year, LENGTH_UOM = "CM", BLOCK_ID = "1")
        imputed_data_list[[length(imputed_data_list) + 1]] <- donor_agg #!!! START HERE, make sure that returned table is in correct format
      }
    } # End loop over holes
    
  } else if(borrow_procedure == "mackerel"){
    
  }
  
  
}
