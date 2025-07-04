% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exp_all_figs_tables.R
\name{exp_all_figs_tables}
\alias{exp_all_figs_tables}
\title{Export all figures and tables}
\usage{
exp_all_figs_tables(
  dat,
  recruitment_unit_label = "mt",
  recruitment_scale_amount = 1,
  end_year = NULL,
  relative = FALSE,
  figures_tables_dir = getwd(),
  ref_line = c("target", "MSY", "msy", "unfished"),
  ref_point = NULL,
  biomass_scale_amount = 1,
  landings_unit_label = "mt",
  spawning_biomass_label = "mt",
  spawning_biomass_scale_amount = 1,
  ref_line_sb = c("target", "MSY", "msy", "unfished"),
  ref_point_sb = NULL,
  abundance_at_age_scale_amount = 1000,
  abundance_at_age_unit_label = "fish",
  biomass_at_age_scale_amount = 1,
  biomass_at_age_unit_label = "metric tons",
  indices_unit_label = NULL,
  biomass_unit_label = "mt",
  catch_unit_label = "mt"
)
}
\arguments{
\item{dat}{A data frame returned from \link[asar]{convert_output}}

\item{recruitment_unit_label}{Units for recruitment}

\item{recruitment_scale_amount}{A number describing how much to scale down
the recruitment quantities shown on the y axis. For example,
recruitment_scale_amount = 100 would scale down a value from 500,000 -->
5,000. This scale will be reflected in the y axis label.}

\item{end_year}{last year of assessment}

\item{relative}{A logical value specifying if the resulting figures should
be relative spawning biomass. The default is `FALSE`. `ref_line` indicates
which reference point to use.}

\item{figures_tables_dir}{The location of the folder containing
figures and tables ("figures" and "tables").}

\item{ref_line}{A string specifying the type of reference you want to
compare biomass to. The default is `"target"`, which looks for
`"biomass_target"` in the `"label"` column of `dat`. The actual
searching in `dat` is case agnostic and will work with either upper- or
lower-case letters but you must use one of the options specified in the
default list to ensure that the label on the figure looks correct
regardless of how it is specified in `dat`.}

\item{ref_point}{A known value of the reference point along with the label
for the reference point as specified in the output file. Please use this
option if the ref_line cannot find your desired point. Indicate the
reference point in the form c("label" = value).}

\item{biomass_scale_amount}{A number describing how much to scale down the
biomass quantities shown on the y axis. See `recruitment_scale_amount`.}

\item{landings_unit_label}{Units for landings}

\item{spawning_biomass_label}{Units for spawning biomass}

\item{spawning_biomass_scale_amount}{A number describing how much to scale down the
spawning biomass quantities shown on the y axis. See `recruitment_scale_amount`.}

\item{ref_line_sb}{Identical definition as `ref_line`, but this argument is
applied to plot_spawning_biomass.}

\item{ref_point_sb}{Identical definition as `ref_point`, but this argument is
applied to plot_spawning_biomass.}

\item{abundance_at_age_scale_amount}{A number describing how much to scale down the
abundance quantities shown via bubble size. See `recruitment_scale_amount`.}

\item{abundance_at_age_unit_label}{Abbreviated units for abundance at age}

\item{biomass_at_age_scale_amount}{A number describing how much to scale down the
biomass quantities shown via bubble size. See `recruitment_scale_amount`.}

\item{biomass_at_age_unit_label}{Abbreviated units for biomass at age}

\item{indices_unit_label}{Units for index of abundance/CPUE}

\item{biomass_unit_label}{Abbreviated units for biomass}

\item{catch_unit_label}{Abbreviated units for catch}
}
\value{
Rda files for each figure/table.
}
\description{
Export all figures and tables to Rda files within one function.
}
\examples{
\dontrun{
exp_all_figs_tables(dat,
  end_year = 2022,
  ref_line = "unfished",
  ref_point = 13000,
  ref_point_sb = 13000,
  ref_line_sb = "target",
  indices_unit_label = "CPUE",
  biomass_at_age_scale_amount = 1,
  biomass_at_age_unit_label = "metric tons"
)
}
}
