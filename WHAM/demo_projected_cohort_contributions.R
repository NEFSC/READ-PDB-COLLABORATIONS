# This code demonstrates how to calculate the proportion of projected recruits
# (aka paper fish, aka faith-based fish) in the catch.
#
# Written by Charles Perretti
# Updated 2025-09-16 to improve colors in plots


# Set wd to source file location (will save output to same directory as script)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(wham)
library(dplyr)
library(tidyr)
library(ggplot2)

# Fit model
path_to_examples <- system.file("extdata", package="wham")
asap3 <- read_asap3_dat(file.path(path_to_examples,"ex1_SNEMAYT.dat"))
NAA_re = list(
  sigma="rec", #random effects for recruitment only
  cor="iid", #random effects are independent
  recruit_model = 2) #mean recruitment is the only fixed effect parameter

input <- prepare_wham_input(
  asap3, 
  NAA_re = NAA_re,
  model_name="Ex 1: SNEMA Yellowtail Flounder") 

mod <- fit_wham(input, do.osa = F, do.retro=F)

# Do projections
catchvals <- c(150, 200, 210, 300)

proj <- project_wham(mod,
                     proj.opts = list(n.yrs = 4,
                                      proj_F_opt = rep(5, 4),
                                      proj_Fcatch = catchvals))


min_year <- 2016 # Choose first year to plot

plot_data <- proj_cbaa %>% filter(Year >= min_year)

cohort_levels <- unique(plot_data$Cohort)

manual_level <- "Projected recruits"

other_levels <- setdiff(cohort_levels, manual_level)

other_colors <- viridis::viridis(length(other_levels))
names(other_colors) <- other_levels

all_colors <- c(other_colors, "Projected recruits" = "gray")

# Plot projected catch biomass-at-age by cohort
proj_cbaa <- 
  data.frame(Year = proj$years_full, 
             proj$rep$pred_CAA[,,]) %>%
  gather(Age, caa, -Year) %>%
  mutate(Age = substr(Age, 2, 3)) %>%
  left_join({data.frame(Year = proj$years_full, 
                        proj$rep$waa_catch[,,]) %>%
      gather(Age, waa, -Year) %>%
      mutate(Age = substr(Age, 2, 3))}) %>%
  mutate(cbaa = caa * waa,
         Age = as.numeric(Age),
         Cohort = as.character(Year - Age),
         Cohort = ifelse(Age == mod$env$data$n_ages, "Plus group", Cohort),
         Cohort = ifelse(Cohort %in% as.character(setdiff(proj$years_full, proj$years)), 
                         "Projected recruits",
                         Cohort),
         Cohort = forcats::fct_relevel(Cohort, "Plus group")) %>%
  group_by(Year) %>%
  mutate(cbaa_prop = cbaa/sum(cbaa))

ggplot(proj_cbaa %>% filter(Year >= min_year), 
       aes(x = Year, y = cbaa_prop, fill = Cohort)) +
  geom_bar(position="stack", stat="identity") +
  geom_vline(xintercept = max(proj$years + 0.5)) +
  scale_fill_manual(values = all_colors) +
  ylab("Proportion of catch") +
  theme_bw() +
  ggtitle("Proportion of catch by cohort")


ggsave("projected_catch_by_cohort.png", w = 6, h = 5)

ggplot(proj_cbaa %>% filter(Year >= min_year), 
       aes(x = Year, y = cbaa, fill = Cohort)) +
  geom_bar(position="stack", stat="identity") +
  geom_vline(xintercept = max(proj$years + 0.5)) +
  scale_fill_manual(values = all_colors) +
  ylab("Catch (mt)") +
  theme_bw() +
  ggtitle("Catch by cohort")


ggsave("projected_catch_biomass_by_cohort.png", w = 6, h = 5)
