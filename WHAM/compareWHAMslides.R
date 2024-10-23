#' @title Auto-generate model comparison slides 
#' 
#' @description Compare diagnostics side by side for 2 WHAM models in pptx slides or 2+ in revealjs .html output. 
#' 
#' @param plotPaths A vector of file paths pointing to plots folders created by plot_wham_output for all models to be compared, must end in "plots_png".
#' @param format A string indicating the type of format to use ("pptx" or "revealjs"), default = "pptx"
#' @param slideOut A file path for the resulting quarto document that formats the slides (must end in .qmd extension), no default.
#' @param slideTitle A string used for the slide title page
#' @param fleetNames A vector of fleet names in the order that they are numbered in plotted results (assumes same fleets for all models, if this isn't the case use fleetGroup instead)
#' @param fleetGroup A named list containing a vector of fleet numbers to compare across all models (e.g. if number of fleets or order differs across models specify which fleets should be compared), if provided these names are used to label slides instead of fleetNames. Length of all vectors in list must equal length of plotPaths, provide 0 if fleet does not exist for a given model
#' @param indexNames A vector of index names in the order that they are numbered in plotted results (assumes same indices for all models, if this isn't the case use indexGroup instead)
#' @param indexGroup A named list containing a vector of index numbers to compare across all models (e.g. if index order differs or an index is split in 2 for one model), if provided these names are used to label slides instead of indexNames. Length of all vectors in list must equal length of plotPaths, provide 0 if index does not exist for a given model   
#' @param modelNames A vector of model names in the order that their plotted results are listed in plotPaths
#' @param AIC An optimal vector of AIC values for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param Gradient An optimal vector of gradient convergence values for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param Convergence An optimal vector of convergence values for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param MohnsRho_F An optimal vector of Mohn's rho F values for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param MohnsRho_SSB An optimal vector of Mohn's rho SSB values for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param MohnsRho_R An optimal vector of Mohn's rho R values for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param Overfished An optimal vector of overfished status for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param Overfishing An optimal vector of overfishing status for each model (order consistent with plotPaths) for inclusion in a summary table
#' @param Rho_adjust An optimal vector of rho adjustment checks for each model (order consistent with plotPaths) for inclusion in a summary table
#' 
#' @return A .qmd and output slides in the selected slideOut directory to compare the specified model runs
#'
#' @examples
#' # Generalized file paths and labels are purely for demonstration and would need to be updated to run on individual machines
#' compareWHAMslides(plotPaths = c(here::here("model1_folder/plots_png"),
#'                                 here::here("model2_folder/plots_png")),
#'               slideOut = here::here("compare_1_2_test.qmd"), slideTitle = "Testing title text",
#'               fleetNames = "US fleet", indexNames = c("DFO spring index", "NEFSC spring index ", "NEFSC fall index"), modelNames = c("model 1", "model 2"))
#' # Example with models where default index numbering does not correspond 
#' compareWHAMslides(plotPaths = c(here::here("model1_folder/plots_png"),
#'                                 here::here("model3_folder/plots_png")),
#'               slideOut = here::here("compare_split_run.qmd"), slideTitle = "Testing title text",
#'               indexGroups = list(DFO_spring = c(1,1), NEFSC_spring_Alb = c(2,2), NEFSC_spring_Big = c(2,3), NEFSC_fall_Alb = c(3,4), NEFSC_fall_Big = c(3,5), NEFSC_fall_BigOnly = c(0,5)))

compareWHAMslides <- function(plotPaths = NULL,
                          format = "pptx",
                          slideOut = NULL,
                          slideTitle = NULL,
                          fleetNames = NULL,
                          fleetGroups = NULL,
                          indexNames = NULL,
                          indexGroups = NULL,
                          modelNames = NULL,
                          AIC = NULL,
                          Gradient = NULL,
                          Convergence = NULL,
                          MohnsRho_F = NULL,
                          MohnsRho_SSB = NULL,
                          MohnsRho_R = NULL,
                          Overfished = NULL,
                          Overfishing = NULL, 
                          Rho_adjust = NULL){
  ##### Setup #####
  library(quarto)
  
  ncol <- length(plotPaths)
  widths <- rep(100/length(plotPaths), ncol)
  plotpath <- plotPaths
  if(is.null(modelNames)){
    label_runs <- paste0("Run ", c(1:ncol))
  } else{
    label_runs <- modelNames
  }
  
  # ID max number of default fleets and indices across models
  nfleet <- NULL
  nindex <- NULL
  for(imodel in 1:length(plotPaths)){
    nfleet[imodel] <- list.files(paste0(plotPaths[imodel], "/input_data"), pattern = "catch_age_comp") %>% length()
    nindex[imodel] <- list.files(paste0(plotPaths[imodel], "/results"), pattern = "index") %>% length()
  }
  
  # Fleet names
  if(is.null(fleetGroups) == TRUE){
    nfleet <- max(nfleet) # Loop over maximum unique fleets, assume number order corresponds across fleets
    
    if(is.null(fleetNames)){
      fleetNames <- paste0("fleet ", c(1:nfleet))
      fleetTitle <- paste0("## Goodness of fit to ", fleetNames)
    } else {
      fleetTitle <- paste0("## Goodness of fit to ", fleetNames)
    }
  } else{ # Otherwise use fleetGroups to define what fleets are compared
    nfleet <- length(fleetGroups)
    
    fleetNames <- names(fleetGroups)
    fleetTitle <- paste0("## Goodness of fit to ", fleetNames)
  }
  
  # Index names
  if(is.null(indexGroups) == TRUE){
    nindex <- max(nindex) # Loop over maximum unique indices, assume number order corresponds across indices
    
    if(is.null(indexNames)){
      indexNames <- paste0("index ", c(1:nindex))
      indexTitle <- paste0("## Goodness of fit to ", indexNames)
    } else{
      indexTitle <- paste0("## Goodness of fit to ", indexNames)
    }
  } else{ # Otherwise use indexGroups to define what indices are compared
    nindex <- length(indexGroups)
    
    indexNames <- names(indexGroups)
    indexTitle <- paste0("## Goodness of fit to ", indexNames) # Pull index slide names from indexGroups list since not following a default numbering
  }
  
  
  ##### Write quarto slide structure to file based on setup dimensions #####
  write("---", slideOut, append = FALSE)
  if(is.null(slideTitle)){
    write("title: 'Compare WHAM Model Results'", slideOut, append = TRUE)
  } else{
    write(paste0("title: '", slideTitle, "'"), slideOut, append = TRUE)
  }
  
  if(format == "revealjs"){
    write(paste0("format: "), slideOut, append = TRUE)
    write(paste0("  ", format,":"), slideOut, append = TRUE)
    write("    width: '150%'", slideOut, append = TRUE) # Increases size of plots, 
  } else if(format == "pptx"){
    write(paste0("format: ", format), slideOut, append = TRUE)
  }
  
  write("editor: visual", slideOut, append = TRUE)
  write("---", slideOut, append = TRUE)
  
  # Add fleet plots
  for(ifleet in 1:nfleet){
    
    write(fleetTitle[ifleet], slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(fleetGroups) == FALSE){ # If fleetGroups exist, iFleet is pulled from group instead of default numbering and nfleet (~ line 63) is based on the number of groups
        iFleet <- fleetGroups[ifleet][[1]][icol]
      } else{
        iFleet <- ifleet
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("Catch_4panel_fleet_",iFleet))) == 1){ # Include in column only if ifleet has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics",paste0("Catch_4panel_fleet_", iFleet, ".png"), sep="/"),"){width=150%}"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(fleetTitle[ifleet], " OSA residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(fleetGroups) == FALSE){ # If fleetGroups exist, iFleet is pulled from group instead of default numbering and nfleet (~ line 63) is based on the number of groups
        iFleet <- fleetGroups[ifleet][[1]][icol]
      } else{
        iFleet <- ifleet
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("OSA_resid_catch_4panel_fleet_",iFleet))) == 1){ # Include in column only if ifleet has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics", paste0("OSA_resid_catch_4panel_fleet_", iFleet,".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(fleetTitle[ifleet], " age comp OSA residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(fleetGroups) == FALSE){ # If fleetGroups exist, iFleet is pulled from group instead of default numbering and nfleet (~ line 63) is based on the number of groups
        iFleet <- fleetGroups[ifleet][[1]][icol]
      } else{
        iFleet <- ifleet
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("Catch_age_comp_osa_resids_fleet_",iFleet))) == 1){ # Include in column only if ifleet has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics", paste0("Catch_age_comp_osa_resids_fleet_", iFleet,".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(fleetTitle[ifleet], " age comp OSA residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(fleetGroups) == FALSE){ # If fleetGroups exist, iFleet is pulled from group instead of default numbering and nfleet (~ line 63) is based on the number of groups
        iFleet <- fleetGroups[ifleet][[1]][icol]
      } else{
        iFleet <- ifleet
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("OSA_resid_paa_6panel_fleet_",iFleet))) == 1){ # Include in column only if ifleet has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics",paste0("OSA_resid_paa_6panel_fleet_", iFleet, ".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(fleetTitle[ifleet], " age comp obs-pred residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(fleetGroups) == FALSE){ # If fleetGroups exist, iFleet is pulled from group instead of default numbering and nfleet (~ line 63) is based on the number of groups
        iFleet <- fleetGroups[ifleet][[1]][icol]
      } else{
        iFleet <- ifleet
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("Catch_age_comp_resids_fleet_",iFleet))) == 1){ # Include in column only if ifleet has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics", paste0("Catch_age_comp_resids_fleet_", iFleet,".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
  } # End loop over fleets 
  
  # Add index plots
  for(isurvey in 1:nindex){
    
    write(indexTitle[isurvey], slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(indexGroups) == FALSE){ # If indexGroups exist, isurvey is pulled from group instead of default numbering and nindex (~ line 58) is based on the number of groups
        iSurvey <- indexGroups[isurvey][[1]][icol]
      } else{
        iSurvey <- isurvey
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("Index_4panel_",iSurvey))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics",paste0("Index_4panel_", iSurvey, ".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(indexTitle[isurvey], " OSA residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(indexGroups) == FALSE){ # If indexGroups exist, isurvey is pulled from group instead of default numbering and nindex (~ line 58) is based on the number of groups
        iSurvey <- indexGroups[isurvey][[1]][icol]
      } else{
        iSurvey <- isurvey
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("OSA_resid_catch_4panel_index_",iSurvey))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics", paste0("OSA_resid_catch_4panel_index_", iSurvey, ".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(indexTitle[isurvey], " age comp OSA residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(indexGroups) == FALSE){ # If indexGroups exist, isurvey is pulled from group instead of default numbering and nindex (~ line 58) is based on the number of groups
        iSurvey <- indexGroups[isurvey][[1]][icol]
      } else{
        iSurvey <- isurvey
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("Catch_age_comp_osa_resids_index_",iSurvey))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics", paste0("Catch_age_comp_osa_resids_index_", iSurvey, ".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(indexTitle[isurvey], " age comp OSA residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(indexGroups) == FALSE){ # If indexGroups exist, isurvey is pulled from group instead of default numbering and nindex (~ line 58) is based on the number of groups
        iSurvey <- indexGroups[isurvey][[1]][icol]
      } else{
        iSurvey <- isurvey
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("OSA_resid_paa_6panel_index_",iSurvey))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics",paste0("OSA_resid_paa_6panel_index_", iSurvey,".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
    write(paste0(indexTitle[isurvey], " age comp obs-pred residuals"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(indexGroups) == FALSE){ # If indexGroups exist, isurvey is pulled from group instead of default numbering and nindex (~ line 58) is based on the number of groups
        iSurvey <- indexGroups[isurvey][[1]][icol]
      } else{
        iSurvey <- isurvey
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/diagnostics"), pattern = paste0("Catch_age_comp_resids_index_",iSurvey))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"diagnostics", paste0("Catch_age_comp_resids_index_", iSurvey, ".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
    
  } # End loop over indices

  # Other diagnostics
  write("## Goodness of fit: residual time series", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"diagnostics", "Residuals_time.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  # Results plots
  write("## Model results: CV for SSB, R, F", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "CV_SSB_Rec_F.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: F by fleet", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "F_byfleet.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: M-at-age", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "M_at_age.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: M-at-age Tile", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "MAA_tile.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("##  Numbers-at-age", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "Numbers_at_age.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: Numbers-at-age proportion", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "Numbers_at_age_proportion.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: Catchability", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "q_time_series.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: Selectivity-at-age", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "SelAA_tile.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  for(ifleet in 1:nfleet){
    write(paste0("## Model results: Fleet ",fleetNames[ifleet], " selectivity"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/results"), pattern = paste0("Selectivity_fleet",iFleet))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"results", paste0("Selectivity_fleet", iFleet,".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
  } # End loop over fleet selectivities
  
  for(isurvey in 1:nindex){
    write(paste0("## Model results: Index ", indexNames[isurvey], " selectivity"), slideOut, append = TRUE)
    write(":::: {.columns}", slideOut, append = TRUE)
    for(icol in 1:ncol){
      if(is.null(indexGroups) == FALSE){ # If indexGroups exist, isurvey is pulled from group instead of default numbering and nindex (~ line 58) is based on the number of groups
        iSurvey <- indexGroups[isurvey][[1]][icol]
      } else{
        iSurvey <- isurvey
      }
      write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
      write(label_runs[icol], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(length(list.files(paste0(plotpath[icol],"/results"), pattern = paste0("Selectivity_index",iSurvey))) == 1){ # Include in column only if isurvey has plot
        write(paste0("![](", paste(plotpath[icol],"results", paste0("Selectivity_index", iSurvey,".png"), sep="/"),")"), slideOut, append = TRUE)
      }
      write(":::", slideOut, append = TRUE)
    }
    write("::::", slideOut, append = TRUE)
  } # End loop over index selectivities
  
  write("## Model results: SSB-at-age", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "SSB_at_age.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: SSB & F trends", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "SSB_F_trend.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: SSB-R", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "SSB_Rec.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: logSSB-logR", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "SSB_Rec_loglog.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model results: SSB & R time series", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"results", "SSB_Rec_time.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  # Add Mohn's rho plots
  write("## Model consistency: Fbar Relative Mohn's Rho", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","Fbar_retro_relative.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: Fbar Mohn's Rho", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","Fbar_retro.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: SSB Relative Mohn's Rhos", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","SSB_retro_relative.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: SSB Mohn's Rhos", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","SSB_retro.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: R Relative Mohn's Rho", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","NAA_age1_retro_relative.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: R Mohn's Rho", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","NAA_age1_retro.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: NAA Relative Mohn's Rho", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","NAA_retro_relative.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model consistency: NAA Mohn's Rho", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"retro","NAA_retro.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model Results: Stock status ", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    write(paste0("![](", paste(plotpath[icol],"ref_points","Kobe_status.png", sep="/"),")"), slideOut, append = TRUE)
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
  
  write("## Model Results: Rho adjustment check ", slideOut, append = TRUE)
  write(":::: {.columns}", slideOut, append = TRUE)
  for(icol in 1:ncol){
    write(paste0("::: {.column width='",widths[icol],"%'}"), slideOut, append = TRUE)
    write(label_runs[icol], slideOut, append = TRUE)
    write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
    if(length(list.files(paste0(plotpath[icol]), pattern = "rho_adjust.png")) == 1){ # Include in column only if plot exists
      write(paste0("![](", paste(plotpath[icol],"rho_adjust.png", sep="/"),")"), slideOut, append = TRUE)
    }
    write(":::", slideOut, append = TRUE)
  }
  write("::::", slideOut, append = TRUE)
    
    write("## Model Summary", slideOut, append = TRUE)
    write(capture.output(cat("| Diagnostic | ", paste0(modelNames, " |"))), slideOut, append=TRUE)
    write(capture.output(cat("| ---------- | ", rep("----- |", ncol))), slideOut, append=TRUE)
    if(is.null(AIC) == TRUE){
      write(capture.output(cat("| AIC | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| AIC | ", paste0(AIC,"  |"))), slideOut, append=TRUE)
    }
    if(is.null(Gradient)==TRUE){
      write(capture.output(cat("| Max gradient | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Max gradient | ", paste0(Gradient, "  |"))), slideOut, append=TRUE)
    }
    if(is.null(Convergence) == TRUE){
      write(capture.output(cat("| Converged | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Converged | ", paste0(Convergence, "  |"))), slideOut, append=TRUE)
    }
    if(is.null(MohnsRho_F) == TRUE){
      write(capture.output(cat("| Mohn's Rho: F | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Mohn's Rho: F | ", paste0(MohnsRho_F, "  |"))), slideOut, append=TRUE)
    }
    if(is.null(MohnsRho_SSB) == TRUE){
      write(capture.output(cat("| Mohn's Rho: SSB | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Mohn's Rho: SSB | ", paste0(MohnsRho_SSB, "  |"))), slideOut, append=TRUE)
    }
    if(is.null(MohnsRho_R) == TRUE){
      write(capture.output(cat("| Mohn's Rho: R | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Mohn's Rho: R | ", paste0(MohnsRho_R, "  |"))), slideOut, append=TRUE)
    }
    write(capture.output(cat("| Status | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    if(is.null(Overfished) == TRUE){
      write(capture.output(cat("| Overfished | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Overfished | ", paste0(Overfished, "  |"))), slideOut, append=TRUE)
    }
    if(is.null(Overfishing) == TRUE){
      write(capture.output(cat("| Overfishing | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Overfishing | ", paste0(Overfishing, "  |"))), slideOut, append=TRUE)
    }
    if(is.null(Rho_adjust) == TRUE){
      write(capture.output(cat("| Rho adjustment needed? | ", paste0(rep("  |", ncol)))), slideOut, append=TRUE)
    } else{
      write(capture.output(cat("| Rho adjustment needed? | ", paste0(Rho_adjust, "  |"))), slideOut, append=TRUE)
    }
    
  
  
  
  ###### Source resulting file to render slides #####
  quarto:::quarto_render(slideOut)
}


