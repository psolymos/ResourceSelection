# Consistent AIC

Consistent AIC

## Usage

``` r
CAIC(object, ..., alpha)
# Default S3 method
CAIC(object, ..., alpha)
CAICtable(object, ..., alpha)
```

## Arguments

- object:

  A fitted model object.

- ...:

  More fitted model objects.

- alpha:

  Weight factor between 0 and 1 (see Details). Default value is 0.5.

## Details

CAIC = alpha \* AIC + (1 - alpha) \* BIC

## Value

Atomic vector if only one input object provided, a data frame similar to
what is returned by [`AIC`](https://rdrr.io/r/stats/AIC.html) and
[`BIC`](https://rdrr.io/r/stats/AIC.html) if there are more than one
input objects.

`CAICtable` returns a data frame with delta CAIC (dCAIC = CAIC -
min(CAIC)) and CAIC weights (wCAIC = exp(-0.5 dCAIC_i) / sum(exp(-0.5
dCAIC_i))) where i = 1,...,m are candidate models.

## References

Bozdogan, H. 1987. Model selection and Akaike's information criterion
(AIC): the general theory and its analytical extensions. Psychometrika,
52, 345-370.

Taper, M. 2004. Model identification from many candidates. In: Taper, M.
and Lele, S. R. (eds), The Nature of Scientific Evidence: Statistical,
Philosophical, and Empirical Considerations. The University of Chicago
Press, Chicago, IL, 567 pp.

## Author

Subhash Lele and Peter Solymos

## See also

[`AIC`](https://rdrr.io/r/stats/AIC.html),
[`BIC`](https://rdrr.io/r/stats/AIC.html)

## Examples

``` r
## compare some random models
y <- rnorm(10)
a <- lm(y ~ runif(10))
b <- lm(y ~ runif(10))

0.5*(AIC(a) + BIC(a))
#> [1] 33.73626
CAIC(a)
#> [1] 33.73626
AIC(a)
#> [1] 33.28238
CAIC(a, alpha=1)
#> [1] 33.28238
BIC(a)
#> [1] 34.19014
CAIC(a, alpha=0)
#> [1] 34.19014

CAIC(a, b)
#>   df     CAIC
#> a  3 33.73626
#> b  3 35.90616
CAIC(a, b, alpha=0.2)
#>   df     CAIC
#> a  3 34.00859
#> b  3 36.17849

CAICtable(a, b, alpha=1)
#>   df     CAIC    dCAIC     wCAIC
#> a  3 33.28238 0.000000 0.7474294
#> b  3 35.45228 2.169898 0.2525706

## you can use global option
## useful when inside of xv or bootstrap
## no need for extra argument
getOption("CAIC_alpha")
#> NULL
op <- options(CAIC_alpha = 0.2)
getOption("CAIC_alpha")
#> [1] 0.2
CAIC(a,b)
#>   df     CAIC
#> a  3 34.00859
#> b  3 36.17849
options(op)
getOption("CAIC_alpha")
#> NULL
```
