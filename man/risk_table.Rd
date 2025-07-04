% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afsc_risk_table.R
\name{risk_table}
\alias{risk_table}
\title{Creates risk table. This table was adapted from a table
created by Ben Williams, a NOAA Research Fisheries Biologist,
as part of the AFSC SAFE reporting tool:
https://github.com/BenWilliams-NOAA/safe/blob/main/R/main_table.R}
\usage{
risk_table(dat, end_year, tier, c1, c2, c3)
}
\arguments{
\item{dat}{A data frame returned from \link[asar]{convert_output}}

\item{end_year}{last year of assessment}

\item{tier}{Tier}

\item{c1}{Estimated catch for the current year}

\item{c2}{Projected catch year + 1}

\item{c3}{Projected catch year + 2}
}
\description{
Creates risk table. This table was adapted from a table
created by Ben Williams, a NOAA Research Fisheries Biologist,
as part of the AFSC SAFE reporting tool:
https://github.com/BenWilliams-NOAA/safe/blob/main/R/main_table.R
}
