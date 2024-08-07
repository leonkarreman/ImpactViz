% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_regressions.R
\name{impact_regression}
\alias{impact_regression}
\title{Run Regression Models with Fixed Effects and Clustered Standard Errors}
\usage{
impact_regression(
  outcomes,
  treatment_var,
  dataset,
  fixed_effect_var = NULL,
  cluster_var = NULL,
  control_vars = NULL,
  level = 5
)
}
\arguments{
\item{outcomes}{Vector of outcome variable names.}

\item{treatment_var}{Name of the treatment variable.}

\item{dataset}{The data frame containing the data.}

\item{fixed_effect_var}{Name of the fixed effect variable, or NULL if not used.}

\item{cluster_var}{Name of the clustering variable, or NULL if not used.}
}
\value{
A data frame with regression results.
}
\description{
This function uses the \code{fixest} package to fit linear models for a given set of outcomes
with an option to include fixed effects and calculate clustered standard errors.
}
\examples{
\dontrun{
  results <- run_regression_models_fixest(depvars, treatment_var, dataset, fixed_effect_var, cluster_var)
  print(results)
}
}
