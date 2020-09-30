
<!-- README.md is generated from README.Rmd. Please edit that file -->

# findpeaks

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of findpeaks is to find maxima in fluorescence profile
generated from microscopic time-lapse movie of Streptomyces

## Installation

You can install the released version of EDA from Github:

``` r
devtools::install_github('astrzalka/findpeaks')

findpeaks::run_app()
```

For running the app package Peaks in required. It can be installed on
linux using:

``` r
devtools::install_version('Peaks', version = '0.2', repos = "http://cran.us.r-project.org")
```

Fluorescence profile data can be prepared using an ImageJ script:
imagej\_get\_profile.ijm , which will save the profiles for all selected
ROIs for each channel separately. It will also save ROIs and duplicated
tiff file of analyzed hypha in the same directory.
