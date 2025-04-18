% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_for_cohort_increase.R
\name{check_for_cohort_increase}
\alias{check_for_cohort_increase}
\title{Check for cohort increase in WHAM output}
\usage{
check_for_cohort_increase(mod)
}
\arguments{
\item{mod}{A model result from WHAM, can be either single WHAM or multi WHAM.}
}
\value{
A ggplot showing the relative percent change for each cohort (including the plus group) with the number denoting the age and year of the surviving fish. Values above zero (the red dashed line) indicate the cohort has increased. Values above 100 may be cause for concern.
}
\description{
Random effects can allow a cohort to increase from one year and age to the next year and age. This function makes a plot to quickly identify when this happens.
}
\examples{
mod <- readRDS("m38.RDS")
p <- check_for_cohort_increase(mod)
ggsave(filename = "my_cohort_check.png", p)
Note: for those who do not like ggplot, matplot(naa_percent_rel_diff) can be used instead
}
