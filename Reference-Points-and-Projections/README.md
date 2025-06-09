# READ-PDB-COLLABORATIONS: Reference-Points-and-Projections

This sub-directory contains specialty functions and using fitted models from the WHAM R package to test the reference point outputs or projections based on ABC, rather than OFL values

| File | Description | Initial Contributor |
| ---- | ----------- | ------------------- |
| P-star.basic.model.R    |  Function generates a WHAM projection model based on fitted multiWHAM model and specified P* specifications (calculating ABC from OFL in the MAFMC) |Chuck Adams, adapted by Emily Liljestrand |
| P-star.basic.table.R    |  Function generates an output table based on fitted multiWHAM model and specified P* specifications (calculating ABC from OFL in the MAFMC) |Chuck Adams, adapted by Emily Liljestrand|
| abccalc.R | Calculates ABC from the OFL, B/BMSY ratio, and OFL CV, supports "P-star.basic" codes | Chuck Adams and Mike Wilberg |
| pstarcalc.R | Calculates Pstar from B/BMSY ratio, supports "P-star.basic" codes | Chuck Adams and Mike Wilberg |
| invabccalc.R | Calculates Pstar value from OFL, ABC, and OFL CV, supports "P-star.basic" codes | Chuck Adams and Mike Wilberg |
| FX_SSBX_Calculation_Series.FIXED.R | Calculate the FX% for a specified number of years from any WHAM output | Cameron Hodgdon |
| ChangePoint.R | Estimate the location(s) of breakpoint(s) for an environmental covariate time series | Cameron Hodgdon |
