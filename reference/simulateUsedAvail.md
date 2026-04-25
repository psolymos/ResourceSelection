# Simulate Used-Available Data

Simulates used-available data.

## Usage

``` r
simulateUsedAvail(data, parms, n.used, m, link="logit")
```

## Arguments

- data:

  a matrix or data frame.

- parms:

  coefficients corresponding to the columns of the design matrix derived
  as `model.matrix(~., data)`.

- n.used, m:

  number of used points (`n.used`) and number of available points for
  each (`m`).

- link:

  character, the type of link function to be used.

## Value

A used-available data frame.

## Author

Subhash Lele, Peter Solymos

## Examples

``` r
n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
cfs <- c(1.5,-1,0.5)
dat1 <- simulateUsedAvail(x, cfs, n.used, m, link="log")
str(dat1)
#> 'data.frame':    11000 obs. of  3 variables:
#>  $ status: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ x1    : num  -0.19 -2.023 0.422 -0.982 -0.569 ...
#>  $ x2    : num  0.995 0.542 0.849 0.521 0.531 ...
dat2 <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
str(dat2)
#> 'data.frame':    11000 obs. of  3 variables:
#>  $ status: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ x1    : num  0.1955 0.3459 -0.0139 -0.2431 -0.9787 ...
#>  $ x2    : num  0.0527 0.4838 0.553 0.2334 0.9344 ...
```
