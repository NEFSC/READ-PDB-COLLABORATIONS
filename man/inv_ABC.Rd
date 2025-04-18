% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invabccalc.R
\name{inv_ABC}
\alias{inv_ABC}
\title{Inverse function to calculate P* from ABC and OFL}
\usage{
inv_ABC(ABC, OFL, CV)
}
\arguments{
\item{OFL}{OFL for the stock}

\item{CV}{Coefficient of variation for OFL}

\item{relB}{ABC - allowable biological catch}
}
\value{
P* value
}
\description{
takes as inputs the OFL, biomass relative to its target, and the assumed CV for the OFL
}
\examples{
inv_ABC <- inv_ABC(11456,13234,1.0)
}
\author{
E Liljestrand, expanded code from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
}
