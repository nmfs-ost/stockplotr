% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_spawning_biomass.R
\name{plot_spawning_biomass}
\alias{plot_spawning_biomass}
\title{Plot spawning biomass (SB)}
\usage{
plot_spawning_biomass(
  dat,
  unit_label = "metric tons",
  scale_amount = 1,
  ref_line = c("target", "unfished", "msy"),
  ref_point = NULL,
  end_year = NULL,
  relative = FALSE,
  make_rda = FALSE,
  figures_dir = getwd()
)
}
\arguments{
\item{dat}{A data frame returned from \link[asar]{convert_output}}

\item{unit_label}{units for spawning_biomass}

\item{scale_amount}{A number describing how much to scale down the quantities
shown on the y axis. For example, scale_amount = 100 would scale down a value
from 500,000 --> 5,000. This scale will be reflected in the y axis label.}

\item{ref_line}{A string specifying the type of reference you want to
compare spawning biomass to. The default is `"target"`, which looks for
`"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
searching in `dat` is case agnostic and will work with either upper- or
lower-case letters but you must use one of the options specified in the
default list to ensure that the label on the figure looks correct
regardless of how it is specified in `dat`.}

\item{ref_point}{A known value of the reference point along with the label
for the reference point as specified in the output file. Please use this
option if the ref_line cannot find your desired point. Indicate the
reference point in the form c("label" = value).}

\item{end_year}{last year of assessment}

\item{relative}{A logical value specifying if the resulting figures should
be relative spawning biomass. The default is `FALSE`. `ref_line` indicates
which reference point to use.}

\item{make_rda}{TRUE/FALSE; indicate whether to produce an .rda file containing
a list with the figure/table, caption, and alternative text (if figure). If TRUE,
the rda will be exported to the folder indicated in the argument "figures_dir".
Default is FALSE.}

\item{figures_dir}{The location of the folder containing the generated figure
rda files ("figures") that will be created if the argument `make_rda` = TRUE.
Default is the working directory.}
}
\value{
Plot spawning biomass from the results of an assessment model translated to
the standard output. The [ggplot2::ggplot()] object is returned for further
modifications if needed.
}
\description{
Plot spawning biomass with a reference line as a dashed line. The figure can
also be made relative to this reference line rather than in absolute units.
}
\examples{
\dontrun{
plot_spawning_biomass(dat)

plot_spawning_biomass(
  dat,
  unit_label = "my_unit",
  ref_line = "msy",
  end_year = 2024,
  figures_dir = getwd()
)

plot_spawning_biomass(
  dat,
  unit_label = "my_unit",
  scale_amount = 100,
  ref_point = 1000,
  end_year = 2024,
  relative = TRUE,
  make_rda = TRUE,
  figures_dir = getwd()
)
}
}
