
<!-- README.md is generated from README.Rmd. Please edit that file -->
lmeresampler [![Build Status](https://travis-ci.org/aloy/lmeresampler.svg?branch=master)](https://travis-ci.org/aloy/lmeresampler)
==================================================================================================================================

Overview
--------

The `lme4` and `nlme` packages have made fitting nested linear mixed-effects models quite easy. Using the the functionality of these packages we can easily use maximum likelihood or restricted maximum likelihood to fit our model and conduct inference using our parametric toolkit. In practice, the assumptions of our model are often violated to such a degree that leads to biased estimators and incorrect standard errors. In these situations, resampling methods such as the bootstrap can be used to obtain consistent estimators of the bias and standard errors for inference. `lmeresampler` provides an easy way to bootstrap nested linear-mixed effects models using either the parametric, residual, cases, CGR (semi-parametric), or random effects block (REB) bootstrap fit using either `lme4` or `nlme`. The output from `lmeresampler` is compatible with the `boot` package.

Installation
------------

You can install the latest released version from CRAN with

``` r
install.packages("lmeresampler")
```

or the latest development version from github

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("aloy/lmeresampler")
```
