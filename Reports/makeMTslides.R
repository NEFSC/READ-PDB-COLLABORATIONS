#' @title Generate basic MT slides
#' @description Generate a standard set of MT slides given plots from a final WHAM model run
#' 
#' @param wham_model A final fitted wham model to use in generating MT slides, no default. NOTE: the loaded version of WHAM must match the wham_model (i.e. wham_model is a multi-wham model then multi-wham software must be loaded)
#' @param outdir A file path to store slides and wham plots in a folder titled "MTslides", no default.
#' @param slideName A string to use as the slide deck file name, default = "MTslides". 
#' @param stock_name A string for the stock name, no default. 
#' @param format A string indicating the type of format to use ("pptx" or "revealjs"), default = "pptx"
#' @param fleet_names A vector of fleet names in the order their data is provided to wham (also determines plot order), no default.
#' @param index_names A vector of index names in the order their data is provided to wham (also determines plot order), no default.

# # multi-wham test
# bridge12 <- readRDS(file = here::here("Bridge_runs", "12_bridge_updateWHAM", "12_bridge_model.rds"))
# makeMTslides(wham_model = bridge12,
#              outdir = here::here("Bridge_runs", "12_bridge_updateWHAM"),
#              slideName = "test_MTslides",
#              stock_name = "Georges Bank Atlantic Cod",
#              format = "pptx", 
#              fleet_names = "Combined US & Canadian",
#              index_names = c("DFO spring", "NEFSC spring", "NEFSC fall")
#              )
# 
# # single-wham test
# bridge14 <- readRDS(file = here::here("Bridge_runs", "14_bridge_MT", "14_bridge_model.rds"))
# makeMTslides(wham_model = bridge14,
#              outdir = here::here("Bridge_runs", "14_bridge_MT"),
#              slideName = "test_MTslides",
#              stock_name = "Georges Bank Atlantic Cod",
#              format = "pptx", 
#              fleet_names = "Combined US & Canadian",
#              index_names = c("DFO spring", "NEFSC spring", "NEFSC fall")
# )

makeMTslides <- function(wham_model = NULL,
                         outdir = NULL,
                         slideName = "MTslides",
                         stock_name = NULL,
                         format = "pptx",
                         fleet_names = NULL,
                         index_names = NULL){
  
  # Make storage directory for slides and plots
  dir.create(paste0(outdir, "/MTslides"))
  
  # Plot model results
  plot_wham_output(wham_model, dir.main = paste0(outdir, "/MTslides"))
  
  # Make slide file path
  slideOut = paste0(outdir, "/MTslides/", slideName, ".qmd")
  
  # Make plot path
  plotPath = paste0(outdir, "/MTslides/plots_png")
  
  # Indexing
  n_fleet = wham_model$input$data$n_fleets
  n_index = wham_model$input$data$n_indices
  
  if("n_regions" %in% names(wham_model$input$data)){ # If wham_model has n_regions input data then it is a multi-wham model object
    n_region = wham_model$input$data$n_regions
    n_stock = wham_model$input$data$n_stocks
    multi_wham = TRUE
  } else{
    n_region = 1 # Need this default for index plot reasons
    n_stock = 1
    multi_wham = FALSE
  }
  
  
  
  
  
  # Title slide
  write("---", slideOut, append = FALSE)
  write(paste0("title: '", format(Sys.Date(), "%Y"), " ", stock_name, " Management Track'"), slideOut, append = TRUE)

  # Set slide format
  if(format == "revealjs"){
    write(paste0("format: "), slideOut, append = TRUE)
    write(paste0("  ", format,":"), slideOut, append = TRUE)
    write("    width: '150%'", slideOut, append = TRUE) # Increases size of plots, 
  } else if(format == "pptx"){
    write(paste0("format: ", format), slideOut, append = TRUE)
  }
  
  write("editor: visual", slideOut, append = TRUE)
  write("---", slideOut, append = TRUE)
  
  # Background
  write("## Background", slideOut, append = TRUE)
  
  # TOR 1 slides
  write("## TOR 1: Estimate catch from all sources including landings and discards", slideOut, append = TRUE)
  
  write("## Total catch", slideOut, append = TRUE)
  
  # !!! embed plot of catch bars broken down by commercial, recreational, land/disc based on same summary file that is used for generating the report files
  
  # Catch-at-age bubble plot by fleet
  if(multi_wham == TRUE){
    for(ifleet in 1:n_fleet){
      for(iregion in 1:n_region){
        write(paste0("## Total catch-at-age for ", fleet_names[ifleet], " fleet"), slideOut, append = TRUE)
        write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
        write(paste0("![](", paste(plotPath,"input_data", paste0("catch_age_comp_fleet", ifleet,"_region_", iregion, ".png"), sep="/"),")"), slideOut, append = TRUE)
        write("", slideOut, append = TRUE)
      }
    }
  } else{ # single-wham plots
    for(ifleet in 1:n_fleet){
      write(paste0("## Total catch-at-age for ", fleet_names[ifleet], "fleet"), slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      write(paste0("![](", paste(plotPath,"input_data", paste0("catch_age_comp_fleet", ifleet,".png"), sep="/"),")"), slideOut, append = TRUE)
      write("", slideOut, append = TRUE)
    }
  }

 
  # TOR 2 slides
  write("## TOR 2: Evaluate indices used in the assessment", slideOut, append = TRUE)
  
  # Time series and age comp for each index
  time_series <- wham_model$input$data$agg_indices 
  time_series[which(time_series == -999)] <- NA # Replace -999 with NAs where data missing/unavailable
  for(iregion in 1:n_region){ # If multi_wham == FALSE n_region = 1 so only does this loop once
    for(index in 1:n_index){
      # Make a time series plot for the index
      tsPlot <- time_series[,index] %>% as.data.frame() %>%
        mutate(Year = wham_model$years) %>%
        ggplot() +
        geom_line(aes(x = Year, y = .)) +
        ylab(paste0(index_names[index], " Index")) +
        theme_classic()
      ggsave(filename = here::here(plotPath, "input_data", paste0("index_", index, "_region_", iregion, "_timeseries.png")), tsPlot, width = 10) # Always save with iregion so timeseries half of slides can pull plot names consistently
      
      # Format slide
      
      write(paste0("## ", index_names[index], " Index"), slideOut, append = TRUE) # write title
      write(":::: {.columns}", slideOut, append = TRUE) # Initialize columns
      
      # Time series column
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      write(paste0("![](", paste(plotPath,"input_data", paste0("index_", index, "_region_", iregion, "_timeseries.png"), sep="/"),")"), slideOut, append = TRUE) 
      write(":::", slideOut, append = TRUE)
      
      # Age comp column
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(multi_wham == TRUE){
        write(paste0("![](", paste(plotPath,"input_data", paste0("index_", index, "_region_", iregion, "_age_comp.png"), sep="/"),")"), slideOut, append = TRUE)
      } else{
        write(paste0("![](", paste(plotPath,"input_data", paste0("index", index, "_age_comp.png"), sep="/"),")"), slideOut, append = TRUE) #!!! confirm that this is the naming convention for single-wham models
      }
      write(":::", slideOut, append = TRUE)
      
      write("::::", slideOut, append = TRUE) # End columns
    } # End loop over indices
  } # End loop over regions

  
  # TOR 3 slides
  write("## TOR 3: ", slideOut, append = TRUE)
  write("Estimate annual fishing mortality, recruitment and stock biomass for the time series using the approved assessment method and estimate their uncertainty. Include retrospective analyses if possible (both historical and within-model) to allow a comparison with previous assessment results and projections, and to examine model fit. \n Include bridge runs from the previously accepted model to the updated model proposed for this peer review.", slideOut, append = TRUE)
  write("", slideOut, append = TRUE)
  
  # Bridge runs
  write("## Bridge runs")
  
  # Model fit to fleet 
  for(iregion in 1:n_region){ # If multi_wham == FALSE n_region = 1 so only does this loop once
    for(ifleet in 1:n_fleet){
      # Format slide
      write(paste0("## Aggregate fit to ", fleet_names[ifleet], " fleet"), slideOut, append = TRUE) # write title
      write(":::: {.columns}", slideOut, append = TRUE) # Initialize columns
      
      # Time series column
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(multi_wham == TRUE){
        write(paste0("![](", paste(plotPath,"diagnostics", paste0("Catch_4panel_fleet_fleet_", ifleet, "_region_", iregion, ".png"), sep="/"),")"), slideOut, append = TRUE) 
      } else{
        write(paste0("![](", paste(plotPath,"diagnostics", paste0("Catch_4panel_fleet_", ifleet, ".png"), sep="/"),")"), slideOut, append = TRUE) 
      }
      write(":::", slideOut, append = TRUE)
      
      # OSA column 
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      write(paste0("![](", paste(plotPath,"diagnostics", paste0("OSA_resid_catch_4panel_fleet_", ifleet, ".png"), sep="/"),")"), slideOut, append = TRUE)
      write(":::", slideOut, append = TRUE)
      
      write("::::", slideOut, append = TRUE) # End columns
    } # End loop over fleets
  } # End loop over regions
  
  # Model fit to fleet age comp
  for(iregion in 1:n_region){ # If multi_wham == FALSE n_region = 1 so only does this loop once
    for(ifleet in 1:n_fleet){
      # Format slide
      write(paste0("## Aggregate fit to ", fleet_names[ifleet], " fleet"), slideOut, append = TRUE) # write title
      write(":::: {.columns}", slideOut, append = TRUE) # Initialize columns
      
      # OSA bubble plot
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      write(paste0("![](", paste(plotPath,"diagnostics", paste0("Catch_age_comp_osa_resids_fleet_", ifleet, ".png"), sep="/"),")"), slideOut, append = TRUE) 
      write(":::", slideOut, append = TRUE)
      
      # OSA 6 panel plot 
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      write(paste0("![](", paste(plotPath,"diagnostics", paste0("OSA_resid_paa_6panel_fleet_", ifleet, ".png"), sep="/"),")"), slideOut, append = TRUE)
      write(":::", slideOut, append = TRUE)
      
      write("::::", slideOut, append = TRUE) # End columns
    } # End loop over fleets
  } # End loop over regions
  
  # Model fit to indices
  for(iregion in 1:n_region){ # If multi_wham == FALSE n_region = 1 so only does this loop once
    for(index in 1:n_index){
      # Format slide
      write(paste0("## Aggregate fit to ", index_names[index]," index"), slideOut, append = TRUE) # write title
      write(":::: {.columns}", slideOut, append = TRUE) # Initialize columns
      
      # Time series column
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(multi_wham == TRUE){
        write(paste0("![](", paste(plotPath,"diagnostics", paste0("Index_4panel_index_", index, "_region_", iregion, ".png"), sep="/"),")"), slideOut, append = TRUE) 
      } else{
        write(paste0("![](", paste(plotPath,"diagnostics", paste0("Index_4panel_", index, ".png"), sep="/"),")"), slideOut, append = TRUE) 
      }
      write(":::", slideOut, append = TRUE)
      
      # OSA column 
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      write(paste0("![](", paste(plotPath,"diagnostics", paste0("OSA_resid_catch_4panel_index_", index, ".png"), sep="/"),")"), slideOut, append = TRUE)
      write(":::", slideOut, append = TRUE)
      
      write("::::", slideOut, append = TRUE) # End columns
    } # End loop over indices
  } # End loop over regions
  
  # Retrospective plots: Fbar (don't iterate over stocks in multi-wham so labeled differently)
  for(iregion in 1:n_region){
    for(istock in 1:n_stock){
        rho = "Fbar"
        write(paste0("## Mohn's rho: ", rho), slideOut, append = TRUE) # write title
        write(":::: {.columns}", slideOut, append = TRUE) # Initialize columns
        # Relative Mohn's rho column column
        write("::: {.column width='50%'}", slideOut, append = TRUE)
        write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
        if(multi_wham == TRUE){
          write(paste0("![](", paste(plotPath,"retro", paste0("region_", iregion, "_", rho, "_retro_relative.png"), sep="/"),")"), slideOut, append = TRUE) 
        } else{
          write(paste0("![](", paste(plotPath,"retro", paste0(rho, "_retro_relative.png"), sep="/"),")"), slideOut, append = TRUE) 
        }
        write(":::", slideOut, append = TRUE)
        
        # Mohn's rho column 
        write("::: {.column width='50%'}", slideOut, append = TRUE)
        #write(index_names[index], slideOut, append = TRUE)
        write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
        if(multi_wham == TRUE){
          write(paste0("![](", paste(plotPath,"retro", paste0("region_", iregion, "_", rho, "_retro.png"), sep="/"),")"), slideOut, append = TRUE) 
        } else{
          write(paste0("![](", paste(plotPath,"retro", paste0(rho, "_retro.png"), sep="/"),")"), slideOut, append = TRUE) 
        }
        write(":::", slideOut, append = TRUE)
        
        write("::::", slideOut, append = TRUE) # End columns
    } # End loop over stocks
  } # End loop over regions
  
  # Retrospective plots: NAA, R, SSB
  for(iregion in 1:n_region){
    if(multi_wham == TRUE){
      rhoNames <- c("NAA_age_1", "NAA", "SSB")
    } else{
      rhoNames <- c("NAA_age1", "NAA", "SSB")
    }
    for(rho in rhoNames){
      write(paste0("## Mohn's rho: ", rho), slideOut, append = TRUE) # write title
      write(":::: {.columns}", slideOut, append = TRUE) # Initialize columns
      # Relative Mohn's rho column column
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(multi_wham == TRUE){
        write(paste0("![](", paste(plotPath,"retro", paste0("stock_", istock, "_region_", iregion, "_", rho, "_retro_relative.png"), sep="/"),")"), slideOut, append = TRUE) 
      } else{
        write(paste0("![](", paste(plotPath,"retro", paste0(rho, "_retro_relative.png"), sep="/"),")"), slideOut, append = TRUE) 
      }
      write(":::", slideOut, append = TRUE)
      
      # Mohn's rho column 
      write("::: {.column width='50%'}", slideOut, append = TRUE)
      #write(index_names[index], slideOut, append = TRUE)
      write("", slideOut, append = TRUE) # Must have empty space or plots not pulled into powerpoint
      if(multi_wham == TRUE){
        write(paste0("![](", paste(plotPath,"retro", paste0("stock_", istock, "_region_", iregion, "_", rho, "_retro.png"), sep="/"),")"), slideOut, append = TRUE) 
      } else{
        write(paste0("![](", paste(plotPath,"retro", paste0(rho, "_retro.png"), sep="/"),")"), slideOut, append = TRUE) 
      }
      write(":::", slideOut, append = TRUE)
      
      write("::::", slideOut, append = TRUE) # End columns
    } # End loop over retrospective plots
  } # End loop over regions
  
  # Fishing mortality estimates
  write("## Fishing mortality by fleet", slideOut, append = TRUE) # write title
  write(paste0("![](", paste(plotPath,"results", "F_byfleet.png", sep="/"),")"), slideOut, append = TRUE) 
  write("", slideOut, append = TRUE) 
  
  # SSB & F plot
  write("## SSB & F estimates", slideOut, append = TRUE) # write title
  write(paste0("![](", paste(plotPath,"results", "SSB_F_trend.png", sep="/"),")"), slideOut, append = TRUE) 
  write("", slideOut, append = TRUE)
  
  if(multi_wham == TRUE){
    # NAA deviation plot
    write("## NAA deviations", slideOut, append = TRUE) # write title
    write(paste0("![](", paste(plotPath,"results", "NAA_dev_tile.png", sep="/"),")"), slideOut, append = TRUE)
    write("", slideOut, append = TRUE)
  }
  
  
  # TOR 4 slides
  write("## TOR 4: Biological Reference Points", slideOut, append = TRUE)
  write("Re-estimate or update the BRPs as defined by the management track level and recommend stock status. Provide qualitative descriptions of stock status based on simple indicators/metrics.", slideOut, append = TRUE)
  write("", slideOut, append = TRUE)
  
  write("## TOR 4: Biological Reference Points", slideOut, append = TRUE)
  write("Prior/existing methods:", slideOut, append = TRUE)
  write("", slideOut, append = TRUE)
  
  write("## Stock status", slideOut, append = TRUE) # write title
  write("- Stock is/is not overfished (___% of SSB msy proxy)", slideOut, append = TRUE)
  write("- Overfishing is/is not occurring (___% of Fmsy proxy)", slideOut, append = TRUE)
  write(paste0("![](", paste(plotPath,"ref_points", "Kobe_status.png", sep="/"),")"), slideOut, append = TRUE) 
  write("", slideOut, append = TRUE)
  
  # TOR 5 slides
  write("## TOR 5: Conduct short-term projections", slideOut, append = TRUE)
  write("Prior/existing methods:", slideOut, append = TRUE)
  write("", slideOut, append = TRUE)
  
  # TOR 6 slides
  write("## TOR 6: Research ", slideOut, append = TRUE)
  write("Respond to any review panel comments or SSC concerns from the most recent prior assessment", slideOut, append = TRUE)
  write("", slideOut, append = TRUE)
  
  
  ###### Source resulting file to render slides #####
  quarto:::quarto_render(slideOut)
}

