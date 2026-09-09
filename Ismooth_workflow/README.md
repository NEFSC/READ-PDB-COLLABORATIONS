# Ismooth_workflow

A standard workflow for Ismooth uses the PlanBsmooth R package to estimate the multiplier from recent surveys and apply it to the recent mean catch from the stock. The [PlanBsmooth Wiki](https://github.com/cmlegault/PlanBsmooth/wiki) provides an introduction to the package and some guidance for using the built-in functions.

The PlanBsmooth R package can be installed using

```{r, eval=FALSE}
remotes::install_packages("cmlegault/PlanBsmooth")
```

The Ismooth workflow consists of identifying which surveys to use, whether to lag any of the surveys, and whether to fill any missing or incomplete survey values. 

The `ReadADIOS()` and `ReadRaw()` functions in the PlanBsmooth package can be used to read the data. Some new functions provided in the file `Ismooth_functions.R` can be used to clean StockEff data, fill holes, standardize and average surveys, and make numerous plots. These functions are described more fully in the `monkfish_Ismooth.html` file (created using the associated qmd file). A wrapper function is also provided to simplify the application of the approach.

A simple workflow could look something like the following:

Step 1. Get the data from StockEff and create time series for filled and lagged surveys.

Step 2. Use the wrapper function to standardize each survey and average them together and compute the Ismooth multiplier using both Filled and Missing treatments of missing and incomplete surveys, if appropriate.

Step 3. Use the fill test to determine whether to fill the missing and incomplete surveys, if needed.

Step 4. Apply the selected multiplier to the mean of the recent three years of catch for catch advice.

There are a number of aspects of Ismooth that can cause confusion or different results. There are also ways to include data not found in StockEff and conduct sensitivity analyses. Details about these aspects of Ismooth are provided in the [monkfish example](https://html-preview.github.io/?url=https://github.com/NEFSC/READ-PDB-COLLABORATIONS/blob/main/Ismooth_workflow/monkfish_Ismooth.html).


