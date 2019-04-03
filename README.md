# farsAJS

The goal of farsAJS is to make it easier to read, filter, summarise, and plot data 
  for road accidents from the US National Highway Traffic Safety Administration's 
  Fatality Analysis Reporting System (FARS).

## Installation

You can install the released version of farsAJS from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("farsAJS")
```

## Example

This is a basic example which shows you how use the farsAJS package to laod FARS data for the year 2013.

``` r
library(farsAJS)

year<-c(2013)
filename<-make_filename(year)
filename

ds<-fars_read(filename)
head(ds)

```

[![Travis build status](https://travis-ci.org/ajsarradin/farsAJS.svg?branch=master)](https://travis-ci.org/ajsarradin/farsAJS)
