# Scatterplot Matrix with 2D Kernel Density

Scatterplot matrix with 2D kernel density.

## Usage

``` r
kdepairs(x, ...)

# Default S3 method
kdepairs(x, n=25, density=TRUE, contour=TRUE, ...)

# S3 method for class 'rsf'
kdepairs(x, n=25, density=TRUE, contour=TRUE, ...)
```

## Arguments

- x:

  a matrix or data frame (or a fitted model object of class `"rsf"` or
  `"rspf"`).

- n:

  number of bins to be used in kernel density estimation.

- density:

  logical, if shades corresponding to densities should be plotted.

- contour:

  logical, if contour on top of shades should be plotted.

- ...:

  other possible arguments passed to
  [`pairs`](https://rdrr.io/r/graphics/pairs.html).

## Value

Produces a scatterplot matrix with histograms in diagonal, 2D kernel
density estimates and contours in the lower half and bivariate
scatterplots with lowess smooth curves and Pearson correlation values in
the upper half as a side effect. Returns `NULL` invisibly.

## Author

Peter Solymos

## See also

[`pairs`](https://rdrr.io/r/graphics/pairs.html),
[`lowess`](https://rdrr.io/r/stats/lowess.html),
[`kde2d`](https://rdrr.io/pkg/MASS/man/kde2d.html),
[`contour`](https://rdrr.io/r/graphics/contour.html)

## Examples

``` r
kdepairs(iris[1:4])
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
```
