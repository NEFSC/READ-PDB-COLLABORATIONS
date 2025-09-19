# READ-PDB-COLLABORATIONS: WHAM

This sub-directory contains WHAM support functions and use examples that are not yet integrated into the WHAM R package, contributors may also consider [submitting well-tested functions to WHAM directly.](https://github.com/timjmiller/wham/blob/80b2b727fb62e09fb880267fcc648cbdb3a16882/.github/CONTRIBUTING.md)

| File | Description | Initial Contributor |
| ---- | ----------- | ------------------- |
| compareWHAMslides.R    |  Function generates comparison slides (pptx or html) using auto-generated WHAM plot outputs. | Amanda Hart |
| drop_term_index_retro.R | Drops some or all of the indices from the terminal year of each peel of retrospective to see how the NAA devs and retro change. Reliant on the fit_hindcast and plotNAAfxn. | Jon Deroba |
| fit_hindcast.R | Fit hindcast model to data, dropping specified indices and years of data specified by the peel. | Jon Deroba (maybe stole from Perretti) |
| plotNAAfxn.R | Plot NAA devs from WHAM fit | Jon Deroba |
| Mdeaths.R | Calculate deaths (000s mt) due to M | Jon Deroba |
| do_selftest.R | Perform simulation self-tests of fitted WHAM model | Charles Perretti / Brian Stock |
| make_simtest_plots.R | Plot simulation self-tests | Charles Perretti / Chris Legault |
| sim_fn.R | Self-simulation function | Charles Perretti / Brian Stock |
| demo_projected_cohort_contributions.R | Script demonstrating how to plot the proportion of each cohort in the projections (including projected recruits, a.k.a. paper fish) | Charles Perretti | 
| LOO.R | Function to do leave one out for multi-wham | Jon Deroba |
| local_package_howto.R | Instructions on how to load a particular commit of a package from github using WHAM as an example | Charles Perretti |
| multi_selftestplot.R | Plotting results of self test in multi-wham | Charles Perretti (stolen by Jon Deroba) |
| simple_WHAM_rho_adjusted_projections.R | Deterministic (no uncertainty estimates) rho-adjusted projections | Chris Legault |
