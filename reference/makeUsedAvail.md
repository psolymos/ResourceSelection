# Make a Used-Available Data Frame

Make a used-available data frame from a presence-absence type data.

## Usage

``` r
makeUsedAvail(x, ...)

# Default S3 method
makeUsedAvail(x, y, ...)

# S3 method for class 'formula'
makeUsedAvail(formula, data = parent.frame(), ...)
```

## Arguments

- x:

  a matrix or data frame.

- y:

  a vector with 0/1 entries, 1s are taken as used observations.

- formula:

  two sided model formula of the form `y ~ x`.

- data:

  data.

- ...:

  other arguments.

## Value

The function returns a data frame, where used and available portions of
the input data are bound on top of each other, the first column refers
to `y`, where used (1) and available (0) locations are indicated
different from the input values. All locations in the input data are
treated as available (0), while only nonzero observations in `y` are
treated as used (1).

## Author

Peter Solymos

## Examples

``` r
(x <- data.frame(species=rep(1:0,each=4), var1=1:8, var2=11:18))
#>   species var1 var2
#> 1       1    1   11
#> 2       1    2   12
#> 3       1    3   13
#> 4       1    4   14
#> 5       0    5   15
#> 6       0    6   16
#> 7       0    7   17
#> 8       0    8   18
makeUsedAvail(species ~ var1 + var2, x)
#>    species var1 var2
#> 1        1    1   11
#> 2        1    2   12
#> 3        1    3   13
#> 4        1    4   14
#> 5        0    1   11
#> 6        0    2   12
#> 7        0    3   13
#> 8        0    4   14
#> 9        0    5   15
#> 10       0    6   16
#> 11       0    7   17
#> 12       0    8   18
```
