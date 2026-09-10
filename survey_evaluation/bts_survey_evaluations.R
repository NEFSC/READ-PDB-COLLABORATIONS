# script to execute call to evaluate bottom trawl survey tows
# liz brooks
# last update: 21 august 2026


rm(list=ls(all=TRUE))      # Remove all variables, etc. from the session memory

library(tibble)            # Load required libraries
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(rstudioapi)
# get path for the R script you are working on
script_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(script_path))
library(here)              # Critical for relative file routing
library(ggforce)
library(devtools)

# ==============================================================================
# DYNAMIC DIRECTORY SETUP (Works on any computer)
# ==============================================================================
# The here() function automatically anchors paths to the root directory where 
# your DESCRIPTION file lives, regardless of who runs the script.

# 1. Load the function by sourcing it directly from the local R/ folder
# source(here("R", "fn_bts_survey_evaluations.R"))
# Load the local package framework interactively
devtools::load_all()

# Pull up the compiled help manual file
?survey_evaluation_plots


# 2. Establish data directory relative to the package root
data.dir <- here("data")

# 3. Establish output directory for saving plots and csv files
output.dir <- here("plots")

# If the plots output directory does not exist it will be created for you
if(!dir.exists(output.dir)) dir.create(output.dir, recursive=TRUE)



# ==============================================================================
# INPUT FILEPATHS
# USER: survey area file is already in /data; you need to download the 2 stockeff files for your stock
# ==============================================================================
# csv file with all strata and area
filepath.sv.area <- file.path(data.dir, "strata_area.csv")

# 2 stockeff files
## csv of stratified mean 
filepath.mean.index <- file.path(data.dir, "STOCKEFF_SV_172735_UNIT_NONE_strat_mean.csv")

## csv of individual tows
filepath.spp.tow.map <- file.path(data.dir, "STOCKEFF_SV_172735_UNIT_NONE_survey_dist_map_fixed.csv")

# ==============================================================================
# EXECUTE EVALUATION AND PLOTTING FUNCTION
# USER: yaxis.max allows you to control the maximum value for the yaxis on several plots
#       starting with NULL is best, so you can see if you need to truncate (also, the value may vary by N or Kg)
#       if you want plots for both units, you have to run the function twice
# ==============================================================================
survey_evaluation_plots(filepath.sv.area = filepath.sv.area,
                        filepath.spp.tow.map = filepath.spp.tow.map,
                        filepath.mean.index = filepath.mean.index,
                        od = output.dir,
                        sv.unit = 'N',       # options: 'N' or 'Kg'
                        plot.h = 6.5,         # plot height
                        plot.w = 8,           # plot width
                        plot.nrow.pp = 4,     # number of rows per page faceted by strata
                        plot.ncol.pp = 5,     # number of columns per page
                        yaxis.max = NULL,       # when NULL uses the max observed value;
                        plot.f = "png"        # output image format
)


# ==============================================================================
# GENERATE HTML FILE WITH ALL OF THE PLOTS
# USER: add your name as author_name and add your stock as stock_name
#       the document will summarize N or Kg plots (or both if you generated both)
# ==============================================================================

quarto::quarto_render(
  input = "survey_report.qmd",
  execute_params = list(
    author_name = "elmo",
    stock_name = "Summer Flounder"
  )
)


