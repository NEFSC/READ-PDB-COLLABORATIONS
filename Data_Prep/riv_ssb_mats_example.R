

scup_my <- matrix(
  data = c(0.033,0.096,0.215,0.308,0.405,0.485,0.601,0.8,
           0.039,0.081,0.212,0.326,0.474,0.552,0.668,0.883,
           0.05,0.102,0.183,0.302,0.424,0.531,0.621,0.846,
           0.022,0.082,0.177,0.267,0.371,0.502,0.622,0.795,
           0.016,0.066,0.107,0.245,0.316,0.414,0.526,0.776,
           0.021,0.088,0.19,0.274,0.326,0.382,0.474,0.776,
           0.026,0.063,0.155,0.234,0.329,0.378,0.43,0.63,
           0.024,0.078,0.174,0.286,0.319,0.404,0.445,0.554),
  ncol = 8,
  byrow = TRUE)

# Spawning month = June (June 1)
spawn_time_frac <- 5/12

# Number of years to include in rivard terminal year average (used in the
# SSB WAA calcs)
riv_avg <- 5


source('misc/riv_ssb_mats.R')

sc_waa <- riv_ssb_mats(my_mat = scup_my,
                       riv_ty_avg_n = riv_avg,
                       frac_ann = spawn_time_frac)

# Rivard WAA
sc_waa$RIVARD_WAA

# Rivard WAA including year t+1 which is an average of the previous
# n years specified in the function (necessary to calculate the
# SSB WAA).
sc_waa$RIVARD_LY_AVG_WAA

# SSB WAA
sc_waa$SSB_WAA

# Specifications that were passed to the function
sc_waa$specs

