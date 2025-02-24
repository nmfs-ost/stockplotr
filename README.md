# stockplotr

<!-- badges: start -->
<!-- badges: end -->

The goal of stockplotr is to create a centralized package that contains all of the figures and tables that are used when analyzing stock assessment model outputs, writing a report, and other various procedures performed during the stock assessment workflow. There are multiple current packages that perform a similar function, but they are typically region and/or model dependent. Across the US, there are multiple packages that create plots that are directly used in a stock assessment report used for management. For example, an analyst that uses Stock Synthesis (SS3) to assess a stock will utilize [`r4ss`](https://github.com/r4ss/r4ss/), a package that reads outputs, plots key parameters, and more to increase throughput and reduce tedious tasks for an analyst.

Please note that this package is still in development. As such, some functions are still in development, such as the functions that create indices figures (i.e., `plot_indices`) and landings tables (i.e., `table_landings`).

## Installation

You can install the development version of stockplotr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nmfs-ost/stockplotr")
```

Alternatively, you can install stockplotr using the `remotes` package:

```r
install.packages("remotes")
remotes::install_github("nmfs-ost/stockplotr")
```

Occassionally, the package can not be installed using the `remotes` package. If this is the case for you and the other two installation options don't work please try:

```r
install.packages("pak")
pak::pak("nmfs-ost/stockplotr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stockplotr)
## basic example code
```

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
