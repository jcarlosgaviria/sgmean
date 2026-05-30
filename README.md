
# sgmean

`sgmean` computes the trimmed mean using a proportional discount method
on the extremes, replicating the behavior of Statgraphics software.

Unlike R’s built-in `mean(..., trim)`, which removes boundary values
entirely, `sgmean` applies a weighted reduction — producing results
consistent with Statgraphics.

## Installation

You can install `sgmean` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jcarlosgaviria/sgmean")
```

## Usage

``` r
library(sgmean)

x <- c(2, 4, 6, 8, 100)

# sgmean method (Statgraphics compatible)
sgmean(x, trim = 0.05)

# Base R method (different result)
mean(x, trim = 0.05)
```

## Author

Juan Carlos Gaviria Chaverra
