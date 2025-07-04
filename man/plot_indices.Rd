% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_indices.R
\name{plot_indices}
\alias{plot_indices}
\title{Plot Index of Abundance}
\usage{
plot_indices(
  dat,
  unit_label = NULL,
  end_year = NULL,
  make_rda = FALSE,
  figures_dir = NULL
)
}
\arguments{
\item{dat}{A data frame returned from \link[asar]{convert_output}}

\item{unit_label}{units for index of abundance/CPUE}

\item{end_year}{last year of assessment}

\item{make_rda}{TRUE/FALSE; indicate whether to produce an .rda file containing
a list with the figure/table, caption, and alternative text (if figure). If TRUE,
the rda will be exported to the folder indicated in the argument "figures_dir".
Default is FALSE.}

\item{figures_dir}{The location of the folder containing the generated figure
rda files ("figures") that will be created if the argument `make_rda` = TRUE.
Default is the working directory.}
}
\value{
Plot the estimated indices as indicated from a standard assessment
model output file.
}
\description{
Plot Index of Abundance
}
\examples{
\dontrun{
plot_indices(dat)

plot_indices(
  dat,
  unit_label = "my_unit",
  end_year = 2024,
  make_rda = TRUE,
  figures_dir = getwd()
)
}
}
