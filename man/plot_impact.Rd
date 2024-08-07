% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_impact.R
\name{plot_impact}
\alias{plot_impact}
\title{Plot Results of Impact Analysis}
\usage{
plot_impact(
  dataset,
  outcomes,
  treatment_var,
  fixed_effect_var = NULL,
  cluster_var = NULL,
  control_vars = NULL,
  level = 5,
  accuracy = NULL,
  font = "Lato",
  colors = NULL,
  treatment_labs = NULL,
  subtitle = NULL,
  legend_title = NULL,
  outcome_labs = NULL,
  outcome_pct = F,
  errorbars = T,
  buffer_adj = 20,
  buffer_diff_adj = 0,
  label_x_adj = 0,
  y_expansion = c(0, 0.1),
  y_breaks = waiver()
)
}
\arguments{
\item{dataset}{Data frame containing the dataset.}

\item{outcomes}{Character vector of outcome variable names.}

\item{treatment_var}{The name of the treatment variable.}

\item{fixed_effect_var}{The name of the fixed effect variable (optional).}

\item{cluster_var}{The name of the cluster variable (optional).}

\item{level}{The level for confidence intervals.}

\item{colors}{Character vector of colors for treatment groups.}

\item{treatment_labs}{Character vector of labels for treatment groups.}

\item{subtitle}{The subtitle for the plot.}

\item{legend_title}{The title for the legend.}

\item{outcome_labs}{Character vector of labels for outcome variables.}
}
\value{
A ggplot object representing the impact analysis results.
}
\description{
This function plots the results of an impact analysis using the ImpactViz package.
}
\examples{
plot_results(dataset = your_dataset,
             outcomes = c("outcome1", "outcome2"),
             treatment_var = "your_treatment_var",
             colors = c("grey", "blue"),
             treatment_labs = c("Control", "Treatment"),
             subtitle = "Your Subtitle",
             legend_title = "Treatment Group")
}
