# Vignettes

This folder contains the vignettes found in the [stockplotr website](https://nmfs-ost.github.io/stockplotr/) under the 
"Articles" drop-down tab.

To create a new vignette, create a new Rmarkdown file then add this information as the yaml:

```
title: "My Title"
output:
  html_document:
    toc: true
    toc_float: true
vignette: >
  %\VignetteIndexEntry{My Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
```
  
Afterwards, edit the _pkgdown.yml file located in the pkgdown folder. The new 
vignette can be added under the "articles:" then "menu:" lines. Please include:

* test -- the title that's presented to the site visitor when looking at the drop-down menu
* href -- the path to the vignette file. It should be "articles/{name_of_file}.Rmd"


