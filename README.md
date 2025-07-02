# stockplotr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![coverage](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/nmfs-ost/stockplotr/refs/heads/badges/coverage-badge.json)](https://github.com/nmfs-ost/stockplotr/tree/badges)
[![R-cmd-check](https://github.com/nmfs-ost/stockplotr/actions/workflows/call-r-cmd-check.yml/badge.svg)](https://github.com/nmfs-ost/stockplotr/actions/workflows/call-r-cmd-check.yml)
<!-- badges: end -->

<img src="man/figures/stockplotr-hex.png" align="right" height="200" style="float:right; height:200px;" />

***Previously named 'satf'***

The goal of `stockplotr` is to create a centralized package that contains all of the figures and tables that are used when analyzing stock assessment model outputs, writing a report, and other various procedures performed during the stock assessment workflow. There are multiple current packages that perform a similar function, but they are typically region and/or model dependent. Across the US, there are multiple packages that create plots that are directly used in a stock assessment report used for management. For example, an analyst that uses Stock Synthesis (SS3) to assess a stock will utilize [`r4ss`](https://github.com/r4ss/r4ss/), a package that reads outputs, plots key parameters, and more to increase throughput and reduce tedious tasks for an analyst.

Please note that this package is still in development. As such, some functions are still in development, such as the functions that create indices figures (i.e., `plot_indices`) and landings tables (i.e., `table_landings`).

## Installation

Install the package using one of the three following ways:

(1) Using `pak`

```r
install.packages("pak")
pak::pak("nmfs-ost/stockplotr")
```

(2) Using `remotes`

```r
install.packages("remotes")
remotes::install_github("nmfs-ost/stockplotr")
```

(3) From the nmfs-ost r-universe

```r
install.packages("stockplotr", repos = c("https://nmfs-ost.r-universe.dev", "https://cloud.r-project.org"))
```

## Usage

Please refer to the [`asar` tutorial](https://connect.fisheries.noaa.gov/asar_tutorial/#section-preparing-to-run-create_template) to learn how to produce the input file necessary to create figures and tables with `stockplotr`.

Then, once your converted model results are saved as an object in your R environment, you can use `stockplotr` functions to create plots from the object.

For example:

```r
output_file <- fs::path("path/to/my/Report.sso")

# convert your model results file into a `stockplotr` object
converted_output <- asar::convert_output(
  file = output_file,
  model = "SS3"
)

# create a landings figure from your object
plot_landings(converted_output)

# create a landings table from your object
table_landings(converted_output)
```

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
