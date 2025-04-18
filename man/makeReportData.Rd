% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeReportData.R
\name{makeReportData}
\alias{makeReportData}
\title{Extract WHAM results for report}
\usage{
makeReportData(
  model_MT = NULL,
  model_MTproj = NULL,
  model_prior = NULL,
  multiWHAM = c(FALSE, FALSE, FALSE),
  index_names = NULL,
  outdir = FALSE
)
}
\arguments{
\item{model_MT}{The current fitted WHAM assessment model object, no default.}

\item{model_MTproj}{The current fitted WHAM assessment with projections implemented, no default.}

\item{model_prior}{The prior assessment model provided as a WHAM model object}

\item{multiWHAM}{A vector of booleans for 1) the current MT model and 2) the current MT model with projections applied and 3) the prior assessment, if TRUE, assumes that the model is generated by multi-WHAM and includes additional indexing for region/stock as well as any differences in labels. Default = FALSE.}

\item{index_names}{A vector of index names (with NO spaces) that match the order they are listed in the model_MT, default = "index1", "index2"..."index_n". Also used in automatically labeling indices in report unless column names manually overwritten in MT report template. Using spaces or special characters (like "&") in index_names will result in nasty LaTex errors.}

\item{outdir}{A file path specifying the directory where MT report data should be saved}
}
\value{
A list containing the following and saved as an RDS object named "reportData", also saves CSV files with model results and index time series:
\itemize{
  \item{comb.model.summary - A table containing SSB, F, and Recruitment estimates with CVs for the current MT and prior assessments}
  \item{text_msy - Text for MSY proxy BRP}
  \item{text_ssb - Text for SSB proxy BRP}
  \item{text_f - Text for F proxy BRP}
  \item{proj_yrs - Projection years, pulled from model_MTproj}
  \item{MT_BRPs - A table of MT BRPs with 95\% CI}
  \item{indices - A table of index time series with associated CVs}
  \item{n_indices - The number of indices in model_MT}
  \item{MT_termyr - A table of estimates and rho-adjusted values for F, SSB, and Recruitment terminal year estimates, associated CVs and the BRP ratio}
}
}
\description{
Pull WHAM results from the current MT model, projections, and the prior MT assessment, format, and save as an .RDS object for use in generating short reports.
}
