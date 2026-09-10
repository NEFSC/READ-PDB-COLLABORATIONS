# SurveyEvalTools
Author: Liz Brooks
Latest revision: August 2026

This repository contains tools to process, evaluate, and summarize bottom trawl survey (BTS) tow data. It calculates stratum-specific means, tracks the impacts of incomplete annual sampling coverage, and generates a suite of diagnostic trend plots.
I generalized my code from spring 2026 management track for gb haddock, then asked gemini to add a header to my function, clean up the script that calls the function, and then to create a qmd file to generate the summary file "survey_report.html".

## Folder Structure

To run the evaluation, make sure your local directory is organized exactly like this:

```text
survey_evaluation/
├── DESCRIPTION
├── README.md
├── bts_survey_evaluations.R     <- Open and execute this script
├── R/
│   └── fn_bts_survey_evaluations.R
├── man/
│   └── survey_evaluation_plots.Rd
└── data/                                   <- Place your raw CSV datasets here
    ├── strata_area.csv
    ├── STOCKEFF_SV_172735_UNIT_NONE_strat_mean.csv
    └── STOCKEFF_SV_172735_UNIT_NONE_survey_dist_map_fixed.csv
```

## Setup & Prerequisites

Before running the script, ensure you have the required R packages installed. You can install them by running the following command in your RStudio console:

```R
install.packages(c("tibble", "ggplot2", "tidyr", "readr", "dplyr", "here", "ggforce", "devtools"))
```

## How to Run the Analysis

1. Open **RStudio**.
2. Set your working directory to the `survey_evaluation/` folder root. You can do this via the RStudio menu (`Session > Set Working Directory > Choose Directory...`) or by using the console:
   ```R
   setwd("path/to/your/copied/survey_evaluation")
   ```
3. Open and run the **`bts_survey_evaluations_for_gemini.R`** script. 
4. The script uses the `here` package to locate your inputs automatically. It will process the files inside `data/` and automatically create a new folder named `plots/` containing all generated CSV summaries and visual diagnostic plots.

## How to Access the Help Documentation

Because this project is structured as a local R package, you can explore the complete documentation for the evaluation function. 

In your console, run:
```R
# Load the local package framework interactively
devtools::load_all()

# Pull up the compiled help manual file
?survey_evaluation_plots
```
This will open a detailed description of the function, its expected input arguments, data filters, and package dependencies directly in your RStudio **Help** tab.
