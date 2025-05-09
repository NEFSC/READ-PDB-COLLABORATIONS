#devtools::install_github("lichengxue/whamMSE", dependencies=TRUE)
#devtools::install_github("timjmiller/wham", dependencies=TRUE)
#devtools::install_github("timjmiller/wham", dependencies=TRUE,ref="devel")
library(wham)
library(whamMSE)
library(fmsb)
library(ggpubr)

#main.dir = here::here()
setwd("/home/jderoba/Herring/MSE Resource Planning 2025/WHAMmse_shortlived")
#setwd(main.dir)
## Baseline
#With minor modifications, basic info for a short-lived species based on @wiedenmann_evaluation_2017, Table 3 B and C.
#A short-lived species recruits at 1 year, lives to 7, mean M is 0.4, 50% mature at age 2 (1.75 in Wiedenmann). 
#30 year burn in and 30 MSE years.
#Historical fishing pattern is "F-H-L". Starting near F40% in year 1, increasing to 2.5 that value, and then decreasing to that value half way through the "burn-in".
#Assessments every 2 years, two surveys meant to approximate spring and fall. 
#ESS 100 in all cases. Catch CV=0.1, spring CV =0.2 and fall CV=0.3 which is conditioned on the herring assessment. 
#Fishery selectivity is 50% selected at age-3, one year older than maturity. Spring selectivity is 50% selected at age-2 and fall at age-3 
#to account for within year and growth and also consistency with the herring assessment. The fall selectivity also increases at a slightly faster rate than the fleet or spring.
#Sigma R and Sigma for NAA random effects similar to the herring assessment and equal 0.8 and 0.26, respectively.


year_start  <- 1  # starting year in the burn-in period
year_end    <- 30  # end year in the burn-in period
MSE_years   <- 32     # number of years in the feedback loop
# Note: no need to include MSE_years in simulation-estimation 

info <- generate_basic_info(n_stocks = 1, 
                            n_regions = 1,
                            n_indices = 2,
                            n_fleets = 1,
                            n_seasons = 1,
                            base.years = year_start:year_end,
                            n_feedback_years = MSE_years,
                            life_history = "short",
                            n_ages = 7,
                            F_info = list(F.year1 = 0.33, Fhist = "F-H-L", Fmax = 2.5, Fmin = 1, change_time = 0.5, user_F = NULL), #0.33 is approximately F40% based on Wiedenmann
                            catch_info = list(catch_cv = 0.1, catch_Neff = 100),
                            index_info = list(index_cv = c(0.25,0.3), index_Neff = c(100,100), fracyr_indices = c(0.25,0.75), q = c(0.000001,0.000001),units_indices=2,units_index_paa=2),
                            fracyr_spawn = 0.85) 

basic_info = info$basic_info # collect basic information
catch_info = info$catch_info # collect fleet catch information
index_info = info$index_info # collect survey information
F_info = info$F # collect fishing information

# see more details using ?generate_basic_info
basic_info <- generate_NAA_where(basic_info = basic_info, move.type = NULL) # "bidirectional" movement

move <- NULL

n_stocks  <- as.integer(basic_info['n_stocks'])
n_regions <- as.integer(basic_info['n_regions'])
n_fleets  <- as.integer(basic_info['n_fleets'])
n_indices <- as.integer(basic_info['n_indices'])
n_ages    <- as.integer(basic_info['n_ages'])

# Selectivity Configuration
fleet_pars <- c(3,1)
index_pars1 = c(4,1)
index_pars2 = c(3,0.5)
sel <- list(model=rep("logistic",n_fleets+n_indices),
            initial_pars=c(rep(list(fleet_pars),n_fleets),list(index_pars1, index_pars2)))
# M Configuration
M <- list(model="constant",initial_means=array(0.4, dim = c(n_stocks,n_regions,n_ages)))

sigma      <- "rec+1"
re_cor     <- "iid"
ini.opt    <- "equilibrium" # option   <- c("age-specific-fe", "equilibrium")

# Set para. mean rec
#alpha <- 12
#beta  <- 1.5e-4
meanrec=2500000  #similar to herring

# Set sigma for NAA
NAA_sig <- 0.26 
sigma_vals = array(NAA_sig, dim = c(n_stocks, n_regions, n_ages)) # n_stocks x n_regions x n_ages"
rec_sig=0.8
sigma_vals[[1]]=rec_sig

# Set initial NAA for each stock
# log_N1  <- rep(log(meanrec/2), n_stocks) # Create difference between stocks
N1_pars <- generate_ini_N1(basic_info, ini.opt, log_N1 = log(meanrec/2), log_N1_F = 0.5)

NAA_re <- list(N1_model=rep(ini.opt,n_stocks),
               sigma=rep(sigma,n_stocks),
               cor=rep(re_cor,n_stocks),
               recruit_model = 2,
               recruit_pars = rep(list(c(meanrec)),n_stocks), # assume same constant s-r 
               sigma_vals = sigma_vals,
               N1_pars = N1_pars)#,
#NAA_where = basic_info$NAA_where)

# recruit_model = 1: estimating annual recruitments as fixed effects or a random walk if NAA_re$sigma specified
# recruit_model = 2: estimating a mean recruitment with annual recruitments as random effects
# recruit_model = 3: Beverton-Holt stock-recruitment with annual recruitments as random effects
# recruit_model = 4: Ricker stock-recruitment with annual recruitments as random effects

# 1. recruit_pars: a list (length = n_stocks) of vectors of initial parameters for recruitment model. 
# If $recruit_model is 3 (B-H) or 4 (Ricker), parameters are "alpha" and "beta".

# 2. sigma_vals: Initial standard deviation values to use for the NAA deviations. Values are not used if recruit_model = 1 
# If sigma="rec": must be a list (length = n_stocks) of single values
# If sigma="rec+1": a list (length = n_stocks) of 2 values must be specified. First is for the first age class (recruits), second is for all other ages.

input <- prepare_wham_input(basic_info = basic_info, 
                            selectivity = sel, 
                            M = M, 
                            NAA_re = NAA_re, 
                            move = move,
                            catch_info = catch_info, 
                            index_info = index_info, 
                            F = F_info,
                            age_comp="logistic-normal-ar1-miss0")

# IMPORTANT!#
# This appears to be due to the initial F value for the Newton iterations to find F40. The default value is too high for long-lived fish
input$data$FXSPR_init[] <-0.3 # 0.01 # change to a low value

#specify logistic normal ar parms conditioned on herring with ESS = 100
input$par$catch_paa_pars[1,1:2]=c(2.2938,0.83)
input$par$index_paa_pars[1,1:2]=c(2.24,0.74)
input$par$index_paa_pars[2,1:2]=c(2.7,0.85)

random = input$random # check what processes are random effects
input$random = NULL # so inner optimization won't change simulated RE
om <- fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)
# Note: do.fit must be FALSE (no modeling fitting yet)


om_with_data <- update_om_fn(om, seed = 123, random = random)

assess.interval <- 2 # 
base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

mods <- list() # Create a list to save MSE outputs

## Make directory

sub.dir = "Baseline"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

library(doParallel)
library(foreach)

n_cores <- detectCores() # check how many cores available

# Run base case, 3 year assessment interval, 100 reps

cluster <- makeCluster(n_cores-1) 
registerDoParallel(cluster)

# 
foreach (i = 1:100) %dopar% {
  
  library(wham)
  library(whamMSE)
  
  om_with_data <- update_om_fn(om, seed = 123+i, random = random)
  NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
  
  mod = loop_through_fn(om = om_with_data,
                        em_info = info, 
                        random = random,
                        M_em = M, # use OM M
                        sel_em = sel, # use OM sel
                        NAA_re_em = NAA_re_em, # use rec assumed random around the mean instead, help runtime (est B-H is difficult)
                        move_em = NULL,
                        age_comp_em = "logistic-normal-ar1-miss0",
                        # Here is the correct code: separate.em = FALSE also works for one-area model
                        em.opt = list(separate.em = FALSE, separate.em.type = 1, 
                                      do.move = FALSE, est.move = FALSE),
                        assess_years = assess.years, 
                        assess_interval = assess.interval, 
                        base_years = base.years,
                        year.use = 30, # number of years of data you want to use in the assessment model
                        add.years = TRUE, # extends assessment time series instead of moving window of year.use years
                        seed = 123+i,
                        save.sdrep = FALSE, 
                        save.last.em = TRUE,
                        FXSPR_init = 0.3) # IMPORTANT!
  
  saveRDS(mod, file.path(paste(getwd(),sub.dir,sep="/"),sprintf("Mod1_%03d.RDS",i)))
  
}
stopCluster(cluster)

###Baseline but assessment interval of 4 instead of 2
## Make directory

sub.dir = "Long Interval"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

assess.interval <- 4 # 

base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)


cluster <- makeCluster(n_cores-1) 
registerDoParallel(cluster)


foreach (i = 1:100) %dopar% {
  
  library(wham)
  library(whamMSE)
  
  om_with_data <- update_om_fn(om, seed = 123+i, random = random)
  NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
  
  mod = loop_through_fn(om = om_with_data,
                        em_info = info, 
                        random = random,
                        M_em = M, # use OM M
                        sel_em = sel, # use OM sel
                        NAA_re_em = NAA_re_em, # use rec assumed random around the mean instead, help runtime (est B-H is difficult)
                        move_em = NULL,
                        age_comp_em = "logistic-normal-ar1-miss0",
                        # Here is the correct code: separate.em = FALSE also works for one-area model
                        em.opt = list(separate.em = FALSE, separate.em.type = 1, 
                                      do.move = FALSE, est.move = FALSE),
                        assess_years = assess.years, 
                        assess_interval = assess.interval, 
                        base_years = base.years,
                        year.use = 30, # number of years of data you want to use in the assessment model
                        add.years = TRUE, # extends assessment time series instead of moving window of year.use years
                        seed = 123+i,
                        save.sdrep = FALSE, 
                        save.last.em = TRUE,
                        FXSPR_init = 0.3) # IMPORTANT!
  
  saveRDS(mod, file.path(paste(getwd(),sub.dir,sep="/"),sprintf("Mod2_%03d.RDS",i)))
  
}
stopCluster(cluster)

#Baseline with degraded data; I tried alternating surveys but can't get it to work
## Make directory

sub.dir = "Degraded"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

agg_index_sigma = input$data$agg_index_sigma
agg_index_sigma[31:62,] = 0.75 # Increase CV for both survey indices in the feedback period
index_Neff = input$data$index_Neff
index_Neff[31:62,] = 25 # Decrease ESS for both survey indices in the feedback period

#alternate years fall and spring surveys
#remove_agg = TRUE # remove a aggregate index for some years
#remove_agg_pointer = 2 #c(1,2) # both
#remove_agg_years =  31#31:60 #matrix(data=c(seq(31,60,2), seq(32,60,2)), nrow=15, ncol=2) #alternating years by survey
#remove_paa = TRUE #Also remove age comp for that index 
#remove_paa_pointer = 2 # c(1,2) # both
#remove_paa_years = 31# 31:60 #matrix(data=c(seq(31,60,2), seq(32,60,2)), nrow=15, ncol=2) #alternating years by survey

input <- update_input_index_info(input, agg_index_sigma, index_Neff) #,
#remove_agg, remove_agg_pointer, remove_agg_years,
#remove_paa, remove_paa_pointer, remove_paa_years) # Update input file

agg_catch_sigma = input$data$agg_catch_sigma
agg_catch_sigma[31:62,] = 0.2 #double catch CV in the feedback period
catch_Neff = input$data$catch_Neff
catch_Neff[31:62] = 25

input <- update_input_catch_info(input, agg_catch_sigma, catch_Neff)

om <- fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)

# Run 3 year assessment frequency with degraded data

assess.interval <- 2 # 

base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

cluster <- makeCluster(n_cores-1) 
registerDoParallel(cluster)

foreach (i = 1:100) %dopar% {
  
  library(wham)
  library(whamMSE)
  
  om_with_data <- update_om_fn(om, seed = 123+i, random = random)
  NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
  
  mod = loop_through_fn(om = om_with_data,
                        em_info = info, 
                        random = random,
                        M_em = M, # use OM M
                        sel_em = sel, # use OM sel
                        NAA_re_em = NAA_re_em, # use rec assumed random around the mean instead, help runtime (est B-H is difficult)
                        move_em = NULL,
                        age_comp_em = "logistic-normal-ar1-miss0",
                        # Here is the correct code: separate.em = FALSE also works for one-area model
                        em.opt = list(separate.em = FALSE, separate.em.type = 1, 
                                      do.move = FALSE, est.move = FALSE),
                        # ------------------------------------------------------ #
                        # - Below is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        update_index_info  = list(agg_index_sigma = agg_index_sigma, index_Neff = index_Neff), # Must have this!
                        update_catch_info  = list(agg_catch_sigma = agg_catch_sigma, catch_Neff = catch_Neff), # Must have this!
                        # ------------------------------------------------------ #
                        # - Above is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        assess_years = assess.years, 
                        assess_interval = assess.interval, 
                        base_years = base.years,
                        year.use = 30, # number of years of data you want to use in the assessment model
                        add.years = TRUE, # extends assessment time series instead of moving window of year.use years
                        seed = 123+i,
                        save.sdrep = FALSE, 
                        save.last.em = TRUE,
                        FXSPR_init = 0.5) # IMPORTANT!
  
  saveRDS(mod, file.path(paste(getwd(),sub.dir,sep="/"),sprintf("Mod3_%03d.RDS",i)))
  
}
stopCluster(cluster)

# And degraded data with a 4 year assessment
sub.dir = "Degraded Long Interval"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

agg_index_sigma = input$data$agg_index_sigma
agg_index_sigma[31:62,] = 0.75 # Increase CV for both survey indices in the feedback period
index_Neff = input$data$index_Neff
index_Neff[31:62,] = 25 # Decrease ESS for both survey indices in the feedback period

#alternate years fall and spring surveys
#remove_agg = TRUE # remove a aggregate index for some years
#remove_agg_pointer = 2 #c(1,2) # both
#remove_agg_years =  31#31:60 #matrix(data=c(seq(31,60,2), seq(32,60,2)), nrow=15, ncol=2) #alternating years by survey
#remove_paa = TRUE #Also remove age comp for that index 
#remove_paa_pointer = 2 # c(1,2) # both
#remove_paa_years = 31# 31:60 #matrix(data=c(seq(31,60,2), seq(32,60,2)), nrow=15, ncol=2) #alternating years by survey

input <- update_input_index_info(input, agg_index_sigma, index_Neff) #,
#remove_agg, remove_agg_pointer, remove_agg_years,
#remove_paa, remove_paa_pointer, remove_paa_years) # Update input file

agg_catch_sigma = input$data$agg_catch_sigma
agg_catch_sigma[31:62,] = 0.2 #double catch CV in the feedback period
catch_Neff = input$data$catch_Neff
catch_Neff[31:62] = 25

input <- update_input_catch_info(input, agg_catch_sigma, catch_Neff)

om <- fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)

assess.interval <- 4 # 

base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

cluster <- makeCluster(n_cores-1) 
registerDoParallel(cluster)
#i=59
foreach (i = 1:100) %dopar% {
  
  library(wham)
  library(whamMSE)
  
  om_with_data <- update_om_fn(om, seed = 123+i, random = random)
  NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
  
  mod = loop_through_fn(om = om_with_data,
                        em_info = info, 
                        random = random,
                        M_em = M, # use OM M
                        sel_em = sel, # use OM sel
                        NAA_re_em = NAA_re_em, # use rec assumed random around the mean instead, help runtime (est B-H is difficult)
                        move_em = NULL,
                        age_comp_em = "logistic-normal-ar1-miss0",
                        # Here is the correct code: separate.em = FALSE also works for one-area model
                        em.opt = list(separate.em = FALSE, separate.em.type = 1, 
                                      do.move = FALSE, est.move = FALSE),
                        # ------------------------------------------------------ #
                        # - Below is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        update_index_info  = list(agg_index_sigma = agg_index_sigma, index_Neff = index_Neff), # Must have this!
                        update_catch_info  = list(agg_catch_sigma = agg_catch_sigma, catch_Neff = catch_Neff), # Must have this!
                        # ------------------------------------------------------ #
                        # - Above is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        assess_years = assess.years, 
                        assess_interval = assess.interval, 
                        base_years = base.years,
                        year.use = 30, # number of years of data you want to use in the assessment model
                        add.years = TRUE, # extends assessment time series instead of moving window of year.use years
                        seed = 123+i,
                        save.sdrep = FALSE, 
                        save.last.em = TRUE,
                        FXSPR_init = 0.3) # IMPORTANT!
  
  saveRDS(mod, file.path(paste(getwd(),sub.dir,sep="/"),sprintf("Mod4_%03d.RDS",i)))
  
}
stopCluster(cluster)

#Baseline with degraded data and alternating surveys
## Make directory

sub.dir = "Degraded Alternating"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

agg_index_sigma = input$data$agg_index_sigma
agg_index_sigma[31:62,] = 0.75 # Increase CV for both survey indices in the feedback period
index_Neff = input$data$index_Neff
index_Neff[31:62,] = 25 # Decrease ESS for both survey indices in the feedback period

#alternate years fall and spring surveys
remove_agg = TRUE # remove a aggregate index for some years
remove_agg_pointer = c(1,2) # both
remove_agg_years = matrix(data=c(seq(31,62,2), seq(32,62,2)), nrow=16, ncol=2)    #alternating years by survey
remove_paa = TRUE #Also remove age comp for that index 
remove_paa_pointer = c(1,2) # both
remove_paa_years =  matrix(data=c(seq(31,62,2), seq(32,62,2)), nrow=16, ncol=2) #alternating years by survey

input <- update_input_index_info(input, agg_index_sigma, index_Neff,
                                 remove_agg, remove_agg_pointer, remove_agg_years,
                                 remove_paa, remove_paa_pointer, remove_paa_years) # Update input file

agg_catch_sigma = input$data$agg_catch_sigma
agg_catch_sigma[31:62,] = 0.2 #double catch CV in the feedback period
catch_Neff = input$data$catch_Neff
catch_Neff[31:62] = 25

input <- update_input_catch_info(input, agg_catch_sigma, catch_Neff)

om <- fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)

# Run 2 year assessment frequency with degraded data and alternating surveys

assess.interval <- 2 # 

base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

cluster <- makeCluster(n_cores-1) 
registerDoParallel(cluster)

foreach (i = 1:100) %dopar% {
  
  library(wham)
  library(whamMSE)
  
  om_with_data <- update_om_fn(om, seed = 123+i, random = random)
  NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
  
  mod = loop_through_fn(om = om_with_data,
                        em_info = info, 
                        random = random,
                        M_em = M, # use OM M
                        sel_em = sel, # use OM sel
                        NAA_re_em = NAA_re_em, # use rec assumed random around the mean instead, help runtime (est B-H is difficult)
                        move_em = NULL,
                        age_comp_em = "logistic-normal-ar1-miss0",
                        # Here is the correct code: separate.em = FALSE also works for one-area model
                        em.opt = list(separate.em = FALSE, separate.em.type = 1, 
                                      do.move = FALSE, est.move = FALSE),
                        # ------------------------------------------------------ #
                        # - Below is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        update_index_info  = list(agg_index_sigma = agg_index_sigma, index_Neff = index_Neff,
                                                  remove_agg, remove_agg_pointer, remove_agg_years,
                                                  remove_paa, remove_paa_pointer, remove_paa_years), # Must have this!
                        update_catch_info  = list(agg_catch_sigma = agg_catch_sigma, catch_Neff = catch_Neff), # Must have this!
                        # ------------------------------------------------------ #
                        # - Above is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        assess_years = assess.years, 
                        assess_interval = assess.interval, 
                        base_years = base.years,
                        year.use = 30, # number of years of data you want to use in the assessment model
                        add.years = TRUE, # extends assessment time series instead of moving window of year.use years
                        seed = 123+i,
                        save.sdrep = FALSE, 
                        save.last.em = TRUE,
                        FXSPR_init = 0.3) # IMPORTANT!
  
  saveRDS(mod, file.path(paste(getwd(),sub.dir,sep="/"),sprintf("Mod5_%03d.RDS",i)))
  
}
stopCluster(cluster)


#Baseline with degraded data, alternating surveys, and long interval
## Make directory

sub.dir = "Degraded Long Interval Alternating"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

agg_index_sigma = input$data$agg_index_sigma
agg_index_sigma[31:62,] = 0.75 # Increase CV for both survey indices in the feedback period
index_Neff = input$data$index_Neff
index_Neff[31:62,] = 25 # Decrease ESS for both survey indices in the feedback period

#alternate years fall and spring surveys
remove_agg = TRUE # remove a aggregate index for some years
remove_agg_pointer = c(1,2) # both
remove_agg_years = matrix(data=c(seq(31,62,2), seq(32,62,2)), nrow=16, ncol=2)    #alternating years by survey
remove_paa = TRUE #Also remove age comp for that index 
remove_paa_pointer = c(1,2) # both
remove_paa_years =  matrix(data=c(seq(31,62,2), seq(32,62,2)), nrow=16, ncol=2) #alternating years by survey

input <- update_input_index_info(input, agg_index_sigma, index_Neff,
                                 remove_agg, remove_agg_pointer, remove_agg_years,
                                 remove_paa, remove_paa_pointer, remove_paa_years) # Update input file

agg_catch_sigma = input$data$agg_catch_sigma
agg_catch_sigma[31:62,] = 0.2 #double catch CV in the feedback period
catch_Neff = input$data$catch_Neff
catch_Neff[31:62] = 25

input <- update_input_catch_info(input, agg_catch_sigma, catch_Neff)

om <- fit_wham(input, do.fit = F, do.brps = T, MakeADFun.silent = TRUE)

# Run 2 year assessment frequency with degraded data and alternating surveys

assess.interval <- 4 # 

base.years      <- year_start:year_end # Burn-in period
first.year      <- head(base.years,1)
terminal.year   <- tail(base.years,1)
assess.years    <- seq(terminal.year, tail(om$years,1)-assess.interval,by = assess.interval)

cluster <- makeCluster(n_cores-1) 
registerDoParallel(cluster)

foreach (i = 1:100) %dopar% {
  
  library(wham)
  library(whamMSE)
  
  om_with_data <- update_om_fn(om, seed = 123+i, random = random)
  NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
  
  mod = loop_through_fn(om = om_with_data,
                        em_info = info, 
                        random = random,
                        M_em = M, # use OM M
                        sel_em = sel, # use OM sel
                        NAA_re_em = NAA_re_em, # use rec assumed random around the mean instead, help runtime (est B-H is difficult)
                        move_em = NULL,
                        age_comp_em = "logistic-normal-ar1-miss0",
                        # Here is the correct code: separate.em = FALSE also works for one-area model
                        em.opt = list(separate.em = FALSE, separate.em.type = 1, 
                                      do.move = FALSE, est.move = FALSE),
                        # ------------------------------------------------------ #
                        # - Below is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        update_index_info  = list(agg_index_sigma = agg_index_sigma, index_Neff = index_Neff,
                                                  remove_agg, remove_agg_pointer, remove_agg_years,
                                                  remove_paa, remove_paa_pointer, remove_paa_years), # Must have this!
                        update_catch_info  = list(agg_catch_sigma = agg_catch_sigma, catch_Neff = catch_Neff), # Must have this!
                        # ------------------------------------------------------ #
                        # - Above is needed when making changes on data quality- #
                        # ------------------------------------------------------ #
                        
                        assess_years = assess.years, 
                        assess_interval = assess.interval, 
                        base_years = base.years,
                        year.use = 30, # number of years of data you want to use in the assessment model
                        add.years = TRUE, # extends assessment time series instead of moving window of year.use years
                        seed = 123+i,
                        save.sdrep = FALSE, 
                        save.last.em = TRUE,
                        FXSPR_init = 0.3) # IMPORTANT!
  
  saveRDS(mod, file.path(paste(getwd(),sub.dir,sep="/"),sprintf("Mod6_%03d.RDS",i)))
  
}
stopCluster(cluster)

#Check ESS or other is working
mods = list()
sub.dir=c("Baseline","Long Interval","Degraded","Degraded Long Interval","Degraded Alternating","Degraded Long Interval Alternating")
m = 1 # double check model 5
r = 1 # realization
mod <- readRDS(file.path(paste(getwd(),sub.dir[m],sep="/"), sprintf("Mod%d_%03d.RDS", m, r)))
mod$em_input[[3]]$data$index_Neff # double check
mod$om$input$data$age_comp_model_fleets #6 is logistic normal ar1 miss 0
mod$om$input$data$age_comp_model_indices
mod$em_full[[1]]$is_sdrep
mod$em_full[[1]]$opt

###try plotting
model_nums <- 1:6
nsim <- 5 # number of simulations/seed

mods <- lapply(1:nsim, function(r) {
  
  mod_list <- lapply(model_nums, function(m) {
    file_path <- file.path("Results", sprintf("Mod%d_%03d.RDS", m, r))
    readRDS(file_path)
  })
  
  names(mod_list) <- paste0("Mod", model_nums)
  
  return(mod_list)
})

mods[[1]][[6]]$em_full[[1]]$input$data$index_Neff
mods[[1]][[3]]$em_full[[1]]$input$data$index_Neff

plot_mse_output(mods,
                main_dir = getwd(),
                output_dir = "Report",
                output_format = c("pdf"), # or html or png
                width = 10, height = 7, dpi = 300,
                col.opt = "D",
                new_model_names = c("M1","M2","M3","M4","M5","M6"),
                base.model = "M1",
                start.years = 31,
                use.n.years.first = 5,
                use.n.years.last = 5)


