
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://docs.mysurvey.solutions/'><img src="man/img/susospatial.png" align="right" height="139"/></a>

# Survey Solutions Raster-to-Frame Application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div align="justify">

This repository contains the Survey Solutions Raster-to-Frame
Application. It is part of the Survey Solutions Spatial Toolbox to
support survey sampling and implementation using Spatial Resources along
with the [Survey Solutions Computer Assisted Survey System
(CASS)](https://mysurvey.solutions/en/).

The goal of the Survey Solutions Raster-to-Frame Application is to
provide a simple shiny application which allows to update a spatial
polygons area sampling frame with one or more raster layers, which can
be used for PPS sampling of enumeration areas for example in the
[susospatsample app](https://github.com/michael-cw/susospatsample).
Moreover, the application allows also for the creation of a composite
measure of size (MOS), using several input raster to create a composite
MOS.

In addition it also allows to create homogeneous groups of clusters
based on the extracted raster data through either K-means clustering or
spatially constrained clustering implemented with the [Skater
algorithm](https://search.r-project.org/CRAN/refmans/spdep/html/skater.html).
The result can subsequently be used as sampling domains in the
[susospatsample app](https://github.com/michael-cw/susospatsample).

## Installation

The package is not released on CRAN yet, so installation has to be done
through installation from this repository.

#### Prerequisites

- [Install R, version 4.1.1 or
  greater](https://cran.r-project.org/mirrors.html)

- [Install R Studio, version 1.2.5001-3 or
  newer](https://posit.co/download/rstudio-desktop/)

- Make sure the *devtools* package is installed, if not install it with:

``` r
install.packages("devtools")
```

- After that install the actual package:

``` r
devtools::install_github("michael-cw/susorastoframe")
```

In case R prompts you to update/install any additional packages, please
confirm, ideally with update all. This may take a while, however these
packages are required to run the application. **In case you are asked to
install packages from source, please refuse, as this may take very long
in particular in windows environments.**

## Running the application interactively

There are two options to run the application. The first one is in
interactive mode. Start R Studio and run the following commands:

``` r
library(susorastoframe)
## to use the leaflet map
susorastoframe::runRasToFrameApp()
```

</div>
