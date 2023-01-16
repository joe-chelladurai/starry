# starry

<!-- badges: start -->
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/starry?color=blue)](https://r-pkg.org/pkg/starry)
<!-- badges: end -->




The goal of starry is to provide an easy to use interactive interface to plot data and perform statistical tests.



## Installation


Starry is available on CRAN and can be installed using:

```r
install.packages("starry")
```

You can install the development version of starry from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joe-chelladurai/starry")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(starry)
## basic example code
```


## Available Functions

**Plot**
``` r
plot_bar
plot_box
plot_density
plot_histogram
plot_line
plot_scatter
```

**Stat**
``` r
stat_anova
stat_correlation
stat_frequency
stat_regression_linear
stat_ttest
```


## Examples

Each function takes a data argument and variables.
``` r
plot_scatter(mtcars, disp, hp)
```
These functions are also pipeable

``` r
mtcars |>
  plot_scatter(disp, hp)
```

You can also pass just a data argument and choose the variables in the interactive app
``` r
plot_scatter(mtcars)
```
