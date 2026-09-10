#' @title borrow_ages
#'
#' @param missing_ages A table containing length-age pairs that lack age information to fill via borrowing_procedure !!! probably want to describe columns/content better
#' @param CF_ages A table of commercial ages to use in borrowing !!! describe what stockEff products used to generate these and other ages table arguments
#' @param SV_ages A table of survey ages to use in borrowing
#' @param OB_ages PLACEHOLDER FOR NOW: A table of observer ages to use in borrowing
#' @param borrow_procedure A string specifying the borrowing logic to apply to fill age gaps for ALK, options include:
#' \itemize{
#'   \item{"seqCFSV_S_A" - Default, Borrow: 1) CF within semester, 2) CF within year, 3) SV within semester, 4) SV within year} 
#'   \item{"seqSA_CF_SV" - Borrow: 1) CF within semester (i.e. pool mkt categories), 2) SV within semester, 3) CF within year, 4) SV within year} 
#' }
#' @param borrow_2020_option  A string specifying how 2020 borrowing is handled, options include: 
#' \itemize{
#'   \item{Default uses borrow_procedure to fill 2020 gaps, in most cases this results in no gap filling because borrowing does not occur across years}
#'   \item{"none" = do not borrow to fill 2020 age gaps}
#'   \item{"combCFSV_1921" = Combine CF and SV data, fill 2020 using all observations from 2019 and 2021}
#' }
#'            
#' @return A list containing:
#' \itemize{
#'   \item{all_fills - A table containing borrowed data used to fill gaps for missing_ages}
#'   \item{remaining_gaps - A table of missing_ages for which gaps could not be filled using borrow_procedure logic}
#' }            
#'

borrow_ages <- function(missing_ages,
                        CF_ages,
                        SV_ages,
                        OB_ages,
                        borrow_procedure = "seqCFSV_S_A",
                        borrow_2020_option = FALSE){

  ##### seqCFSV_S_A #####
  # Sequentially borrow: 1) Check no match available, 2) borrow CF within semester, 3) CF within year, 4) SV within semester, 5) SV within year 
  if(borrow_procedure == "seqCFSV_S_A"){
    # Check data exist for borrowing option
    if(is.null(CF_ages)){error("No commercial data, provide CF_ages argument or check that 'commercial' %in% tab_alk$source fillCF argument")}
    if(is.null(SV_ages)){error("No survey data, provide SV_ages argument or check that 'survey' %in% tab_alk$source fillCF argument")}
    
    # Initialize tiers as empty data frames
    fill_t1 <- fill_t2 <- fill_t3 <- fill_t4 <- fill_t5 <- data.frame()

    donor_pool <- CF_ages |>
      dplyr::mutate(SOURCE_BASE = SOURCE)
    
    # Drop quarter so borrowing only joins on semester or year
    missing_ages <- missing_ages |> select(-QTR) |> distinct()

    # --- TIER 1: CF Match Everything ---
    fill_t1 <- missing_ages |>
      dplyr::inner_join(donor_pool,
                        by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"),
                        relationship = "many-to-many") |>
      dplyr::mutate(SOURCE = paste0("MATCH_EXACT_", SOURCE_BASE))

    # Track cumulative fills
    all_fills <- fill_t1

    # --- TIER 2: CF Drop NESPP4 ---
    # Remove everything in all_fills from missing_ages to get the next batch of holes
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))

    if(nrow(still_needed) > 0) {
      fill_t2 <- still_needed |>
        dplyr::inner_join(donor_pool |> dplyr::select(-NESPP4) |> dplyr::distinct(),
                          by = c("YEAR", "LENGTH", "SEM", "REGION_ID"),
                          relationship = "many-to-many") |>
        dplyr::mutate(SOURCE = paste0("CF_BORROW_MKTSEM_", SOURCE_BASE))

      all_fills <- dplyr::bind_rows(all_fills, fill_t2)
    }

    # --- TIER 3: CF Drop SEMESTER ---
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
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
        dplyr::mutate(SOURCE = paste0("CF_BORROW_ANNUAL_", SOURCE_BASE))

      all_fills <- dplyr::bind_rows(all_fills, fill_t3)
    }

    # --- TIER 4: SV (Semester) ---
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))

    if(!is.null(SV_ages) && nrow(still_needed) > 0) {
      fill_t4 <- still_needed |>
        dplyr::inner_join(SV_ages,
                          by = c("YEAR", "LENGTH", "SEM"),
                          relationship = "many-to-many") |>
        dplyr::mutate(SOURCE = paste0("SV_BORROW_SEM_", SOURCE))

      all_fills <- dplyr::bind_rows(all_fills, fill_t4)
    }

    # --- TIER 5: SV (Annual) ---
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "NESPP4", "SEM", "REGION_ID"))

    if(!is.null(SV_ages) && nrow(still_needed) > 0) {
      sv_annual <- SV_ages |>
        dplyr::group_by(YEAR, LENGTH, AGE) |>
        dplyr::summarise(NO_AT_AGE = sum(NO_AT_AGE, na.rm = TRUE),
                         SOURCE = dplyr::first(SOURCE), .groups = "drop")

      fill_t5 <- still_needed |>
        dplyr::inner_join(sv_annual,
                          by = c("YEAR", "LENGTH"),
                          relationship = "many-to-many") |>
        dplyr::mutate(SOURCE = paste0("SV_BORROW_ANNUAL_", SOURCE))

      all_fills <- dplyr::bind_rows(all_fills, fill_t5)
      #!!! double check that all_fills equivalent to: dplyr::bind_rows(fill_t1, fill_t2, fill_t3, fill_t4, fill_t5)
    }
  
    ##### seqSA_CF_SV #####
  } else if(borrow_procedure == "seqSA_CF_SV"){
    # Borrow 1) CF within semester (i.e. pool mkt categories), 2) SV within semester, 3) CF within year, 4) SV within year
    
    # Check data exist for borrowing option
    if(is.null(CF_ages)){error("No commercial data, provide CF_ages argument or check that 'commercial' %in% tab_alk$source fillCF argument")}
    if(is.null(SV_ages)){error("No survey data, provide SV_ages argument or check that 'survey' %in% tab_alk$source fillCF argument")}
    
    # Initialize tiers as empty data frames
    fill_t1 <- fill_t2 <- fill_t3 <- fill_t4 <- fill_t5 <- all_fills <- data.frame()
    
    #0) Add source base
    donor_pool_CF <- CF_ages |>
      dplyr::mutate(SOURCE_BASE = SOURCE)
    donor_pool_SV <- SV_ages |>
      dplyr::mutate(SOURCE_BASE = SOURCE)
    
    #1) Pool commercial length-age data across market categories and regions to borrow within semester
    fill_t1 <- missing_ages %>% dplyr::select(-NESPP4, -REGION_ID, -QTR) %>% dplyr::distinct() %>% # Need to drop variables not used in borrowing and select distinct lengths with missing age or else borrowing will duplicate fills across market categories, regions, and quarters
      dplyr::inner_join(donor_pool_CF |> dplyr::select(-NESPP4, -REGION_ID, -QTR) |> dplyr::distinct(), # Don't borrow based on NESPP4, REGION_ID, or QTR
                        by = c("YEAR", "LENGTH", "SEM"),
                        relationship = "many-to-many") |>
      dplyr::mutate(SOURCE = paste0("CF_BORROW_MKTSEM_", SOURCE_BASE))
    
    all_fills <- dplyr::bind_rows(all_fills, fill_t1)
    
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "SEM")) # Look for remaining gaps by semester and year
    
    #2) Borrow survey data within year and semester to fill gaps
    fill_t2 <- still_needed %>% dplyr::select(-NESPP4, -REGION_ID, -QTR) %>% dplyr::distinct() %>%
      dplyr::inner_join(donor_pool_SV |> dplyr::select(-REGION_ID) |> dplyr::distinct(), # Don't borrow based on REGION_ID
                        by = c("YEAR", "LENGTH", "SEM"),
                        relationship = "many-to-many") |>
      dplyr::mutate(SOURCE = paste0("SV_BORROW_SEM_", SOURCE_BASE))
    
    all_fills <- dplyr::bind_rows(all_fills, fill_t2)
    
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "SEM")) # Look for remaining gaps by semester and year
   
    #3) Borrow commercial data within year to fill gaps
    fill_t3 <- still_needed %>% dplyr::select(-NESPP4, -REGION_ID, -QTR) %>% dplyr::distinct() %>% # Retain semester so semester gaps filled using annual data
      dplyr::inner_join(donor_pool_CF |> dplyr::select(-NESPP4, -REGION_ID, -QTR, -SEM) |> dplyr::distinct(), # Don't borrow based on REGION_ID
                        by = c("YEAR", "LENGTH"),
                        relationship = "many-to-many") |>
      dplyr::mutate(SOURCE = paste0("CF_BORROW_ANNUAL_", SOURCE_BASE))
    
    all_fills <- dplyr::bind_rows(all_fills, fill_t3)
    
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "SEM")) # Look for remaining gaps by year and semester
    
    #4) Borrow survey data within year to fill gaps
    fill_t4 <- still_needed %>% dplyr::select(-NESPP4, -REGION_ID, -QTR) %>% dplyr::distinct() %>% # Retain semester so semester gaps filled using annual data
      dplyr::inner_join(donor_pool_SV |> dplyr::select(-REGION_ID, -SEM) |> dplyr::distinct(), # Don't borrow based on REGION_ID or SEMESTER
                        by = c("YEAR", "LENGTH"),
                        relationship = "many-to-many") |>
      dplyr::mutate(SOURCE = paste0("SV_BORROW_ANNUAL_", SOURCE_BASE))
    
    all_fills <- dplyr::bind_rows(all_fills, fill_t4)
    
    still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                     by = c("YEAR", "LENGTH", "SEM")) # Look for remaining gaps by year and semester
    
  } else{
    print("ERROR: no borrowing procedure specified, check spelling for tab_ALK$borrow_procedure argument")
  }

  
  ##### 2020 Borrowing #####
  if(borrow_2020_option == "none"){ # If no 2020 borrowing, drop default borrowing based on borrow_procedure logic
    all_fills <- filter(all_fills, YEAR != 2020)
  } else if(borrow_2020_option == "combCFSV_all1921"){ # Combine CF and SV data, fill 2020 using all observations from 2019 and 2021
    fills_2020 <- CF_ages |>
      select(YEAR, REGION_ID, SEM, LENGTH, AGE, NO_AT_AGE, SOURCE) |>
      rbind(select(SV_ages,YEAR, REGION_ID, SEM, LENGTH, AGE, NO_AT_AGE, SOURCE)) |>  
      filter(YEAR %in% c(2019, 2021)) %>%
      mutate(SOURCE_BASE = SOURCE,
             SOURCE = paste0("BORROW_adjacent_", YEAR),
             YEAR = 2020, # Relable year so used in 2020, source year included in source column
             NESPP4 = "NA", 
             QTR = "NA")
    all_fills <- filter(all_fills, YEAR != 2020) |> # Drop default borrow_procedure fills for 2020
      rbind(select(fills_2020, colnames(all_fills))) # Select columns in all_fills (will drop QTR if not used in borrowing)
  } else if(borrow_2020_option == "combCFSV_fill1921"){
    # Combine CF and SV data and use both data sets to fill remaining 2020 gaps by borrowing by 1) semester and 2) annually
      donor_pool_2020 <- CF_ages |>
        select(YEAR, REGION_ID, SEM, LENGTH, AGE, NO_AT_AGE, SOURCE) |> # pools commercial data across market categories
        rbind(select(SV_ages,YEAR, REGION_ID, SEM, LENGTH, AGE, NO_AT_AGE, SOURCE)) |>  
        filter(YEAR %in% c(2019, 2021)) %>%
        mutate(SOURCE_BASE = SOURCE)
      
      # 1) Borrow within semester across CF and SV in neighboring years
      fill_sem <- still_needed %>% filter(YEAR == 2020) %>% 
        dplyr::select(-NESPP4, -REGION_ID, -QTR, -YEAR) %>% dplyr::distinct() %>%
        dplyr::inner_join(donor_pool_2020 |> dplyr::select(-REGION_ID, -YEAR) |> dplyr::distinct(), # Don't borrow based on REGION_ID or YEAR since allow borrowing across neighboring years
                          by = c("LENGTH", "SEM"),
                          relationship = "many-to-many") |>
        dplyr::mutate(SOURCE = paste0("CFSV_BORROW_SEM2020_", SOURCE_BASE),
                      YEAR = 2020) # Use borrowed data for 2020
      
      all_fills <- dplyr::bind_rows(all_fills, fill_sem)
      
      still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                       by = c("YEAR", "LENGTH", "SEM")) # Look for remaining gaps by year and semester
      
      #2) Borrow across neighboring years (all seasons) and CF and SV data (returns no new fill rows if all 2020 gaps already filled)
      fill_ann <- still_needed %>% filter(YEAR == 2020) %>% 
        dplyr::select(-NESPP4, -REGION_ID, -QTR, -YEAR, -SEM) %>% dplyr::distinct() %>%
        dplyr::inner_join(donor_pool_2020 |> dplyr::select(-REGION_ID, -YEAR, -SEM) |> dplyr::distinct(), # Don't borrow based on REGION_ID SEM, or YEAR since allow borrowing across neighboring years
                          by = c("LENGTH"), # Pool all data by length
                          relationship = "many-to-many") |>
        dplyr::mutate(SOURCE = paste0("CFSV_BORROW_ANN2020_", SOURCE_BASE),
                      YEAR = 2020) # Use borrowed data for 2020
      
      all_fills <- dplyr::bind_rows(all_fills, fill_ann)
      
      still_needed <- dplyr::anti_join(missing_ages, all_fills,
                                       by = c("YEAR", "LENGTH", "SEM")) # Look for remaining gaps by year and semester
  }
  
  ##### Species-specific borrowing #####
  # Custom borrowing to resolve species-specific issues (e.g. how to handle low survey coverage in 2023)
  # Borrowing applied based on species_itis
  # if(species_itis == 172877){ # American plaice
  #   # Borrow spring survey ages from 2022 and 2024 to fill gaps in spring 2023 that occur due to low survey coverage
  #   fills_2023 <- SV_ages |>
  #     filter(YEAR %in% c(2022, 2024), SEASON == "SPRING") |>
  #     mutate(SOURCE_BASE = SOURCE,
  #            SOURCE = paste0("Borrow_adjacentSpring_", YEAR),
  #            YEAR = 2023, # Relable year so used in spring 2023
  #            NESPP4 = "NA",
  #            QTR = "NA")
  #   all_fills <- filter(all_fills, !(YEAR == 2023 & SEM == 1 & SOURCE_BASE != "CAMSAGE")) |> # Drop default survey borrowing for spring 2023 fills 
  #     rbind(select(fills_2023, colnames(all_fills))) # Select columns in all_fills (will drop QTR if not used in borrowing)
  # }
  
  
  # Final check for remaining gaps by semester
  still_needed_check <- dplyr::anti_join(missing_ages, all_fills, 
                                   by = c("YEAR","LENGTH", "SEM"))

  # Return
  return(list(all_fills = all_fills,
              remaining_gaps = still_needed))
}


