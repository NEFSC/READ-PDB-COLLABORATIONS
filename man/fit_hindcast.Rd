% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_hindcast.R
\name{fit_hindcast}
\alias{fit_hindcast}
\title{Fit hindcast model to data}
\usage{
fit_hindcast(model, peel, drop)
}
\arguments{
\item{model}{An rds from a WHAM fit}

\item{peel}{An integer indicating how many years are peeled from the terminal year, just as in a retro}

\item{drop}{A named list with 'indices' and 'index_paa', where each element is a vector of indices/index_paa to drop in each hindcast. To not drop any indices or index_paa in a hindcast, set to NA.}
}
\value{
A WHAM model fit
}
\description{
Fit hindcast model to data, dropping specified indices and years of data specified by the peel
}
\examples{
 fit_hindcast(model=WHAMRUN.rds, peel=1, drop=list(indices=1:2, index_paa=1:2))
}
