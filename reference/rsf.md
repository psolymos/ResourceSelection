# Resource Selection (Probability) Functions for Use-Availability Data

Resource Selection (Probability) Functions for use-availability wildlife
data as described in Lele and Keim (2006) and Lele (2009).

## Usage

``` r
rsf(formula, data, m, B = 99, inits, method = "Nelder-Mead",
control, model = TRUE, x = FALSE, ...)

rspf(formula, data, m, B = 99, link = "logit", inits,
method = "Nelder-Mead", control, model = TRUE, x = FALSE, ...)

rsf.fit(X, Y, m, link = "logit", B = 99,
inits, method = "Nelder-Mead", control, ...)

rsf.null(Y, m, inits, ...)
```

## Arguments

- formula:

  two sided model formula of the form `y ~ x`, where `y` is a vector of
  observations, `x` is the set of covariates.

- m:

  argument describing the matching of use and available points, see
  Details.

- data:

  data.

- B:

  number of bootstrap iterations to make.

- link:

  character, type of link function to be used.

- inits:

  initial values, optional.

- method:

  method to be used in [`optim`](https://rdrr.io/r/stats/optim.html) for
  numerical optimization.

- control:

  control options for [`optim`](https://rdrr.io/r/stats/optim.html).

- model:

  a logical value indicating whether model frame should be included as a
  component of the returned value

- x:

  logical values indicating whether the model matrix used in the fitting
  process should be returned as components of the returned value.

- Y:

  vector of observations.

- X:

  covariate matrix.

- ...:

  other arguments passed to the functions.

## Details

The `rsf` function fits the Exponential Resource Selection Function
(RSF) model to presence only data.

The `rspf` function fits the Resource Selection Probability Function
(RSPF) model to presence only data Link function `"logit"`, `"cloglog"`,
and `"probit"` can be specified via the `link` argument.

The `rsf.fit` is the workhorse behind the two functions. `link="log"`
leads to Exponential RSF.

The `rsf.null` function fits the 'no selection' version of the
Exponential Resource Selection Function (RSF) model to presence only
data.

LHS of the `formula` data must be binary, ones indicating used
locations, while zeros indicating available location.

All available points are used for each use points if `m=0` (global
availability). If `m` is a single value, e.g. `m=5`, it is assumed that
available data points are grouped in batches of 5, e.g. with IDs
`c(1,2)` for used point locations and `c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)`
for available locations (local availability, matched use-available
design). Similarly, a vector of matching IDs can also be provided, e.g.
`c(1, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)` by combining the above two. This
potentially could allow for unbalanced matching (e.g.
`c(1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2)`) and for easier subsetting of
the data, but comes with an increased computing time. Note, the response
in the LHS of the formula should be coded as
`c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)` for all of the above examples.
When `m` is defined as a mapping vector or the value is 0, the order of
course does not matter. However, ordering matters when `m` is constant
because that implies a certain structure.

For model description and estimation details, see Lele and Keim (2006),
Lele (2009), and Solymos and Lele (2016).

## Value

A list with class `"rsf"`, `"rsf.null"`, or `"rspf"` containing the
following components:

- call:

  the matched call.

- y:

  vector from LHS of the formula.

- coefficients:

  a named vector of coefficients.

- std.error:

  a named vector of standard errors for the coefficients.

- loglik:

  the maximized pseudo log-likelihood according to Lele 2009.

- results:

  [`optim`](https://rdrr.io/r/stats/optim.html) results.

- link:

  character, value of the link function used.

- control:

  control parameters for [`optim`](https://rdrr.io/r/stats/optim.html).

- inits:

  initial values used in optimization.

- m:

  value of the `m` argument with possibly matched use-available design.

- np:

  number of active parameters.

- fitted.values:

  vector of fitted values. These are relative selection values for RSF
  models, and probability of selection for RSPF models.

- nobs:

  number of used locations.

- bootstrap:

  component to store bootstrap results if `B`\>0.

- converged:

  logical, indicating convergence of the optimization.

- formula:

  the formula supplied.

- terms:

  the [`terms`](https://rdrr.io/r/stats/terms.html) object used.

- levels:

  a record of the levels of the factors used in fitting.

- contrasts:

  the contrasts used.

- model:

  if requested, the model frame.

- x:

  if requested, the model matrix.

## References

Lele, S.R. (2009) A new method for estimation of resource selection
probability function. Journal of Wildlife Management 73, 122–127.

Lele, S. R. & Keim, J. L. (2006) Weighted distributions and estimation
of resource selection probability functions. Ecology 87, 3021–3028.

Solymos, P. & Lele, S. R. (2016) Revisiting resource selection
probability functions and single-visit methods: clarification and
extensions. Methods in Ecology and Evolution 7, 196–205.

## Author

Subhash R. Lele, Jonah L. Keim, Peter Solymos

## Examples

``` r
## --- Simulated data example ---

## settings
n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
cfs <- c(1.5,-1,0.5)
## fitting Exponential RSF model
dat1 <- simulateUsedAvail(x, cfs, n.used, m, link="log")
m1 <- rsf(status ~ .-status, dat1, m=0, B=0)
summary(m1)
#> 
#> Call:
#> rsf(formula = status ~ . - status, data = dat1, m = 0, B = 0)
#> 
#> Resource Selection Function (Exponential RSF) model
#> Non-matched Used-Available design
#> Maximum Likelihood estimates
#> 
#> Fitted values:
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>  0.03444  0.67639  1.34572  2.34955  2.75012 40.08155 
#> 
#> Coefficients (log link):
#>    Estimate Std. Error z value Pr(>|z|)
#> x1  -1.0036         NA      NA       NA
#> x2   0.4506         NA      NA       NA
#> 
#> Log-likelihood: -8714 
#> BIC = 1.744e+04 
#> 
#> Hosmer and Lemeshow goodness of fit (GOF) test:
#> X-squared = 6.293, df = 8, p-value 0.6145
#> 
## fitting Logistic RSPF model
dat2 <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
m2 <- rspf(status ~ .-status, dat2, m=0, B=0)
summary(m2)
#> 
#> Call:
#> rspf(formula = status ~ . - status, data = dat2, m = 0, B = 0)
#> 
#> Resource Selection Probability Function (Logistic RSPF) model
#> Non-matched Used-Available design
#> Maximum Likelihood estimates
#> 
#> Fitted probabilities:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.1158  0.8306  0.9151  0.8712  0.9604  0.9986 
#> 
#> Coefficients (logit link):
#>             Estimate Std. Error z value Pr(>|z|)  
#> (Intercept)   1.7294     0.8221   2.104   0.0354 *
#> x1           -1.1667     0.5352  -2.180   0.0293 *
#> x2            1.2669     1.0942   1.158   0.2469  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#> 
#> Log-likelihood: -9197 
#> BIC = 1.841e+04 
#> 
#> Hosmer and Lemeshow goodness of fit (GOF) test:
#> X-squared = 4.256, df = 8, p-value 0.8334
#> 

## --- Real data analysis from Lele & Keim 2006 ---

if (FALSE) { # \dontrun{
goats$exp.HLI <- exp(goats$HLI)
goats$sin.SLOPE <- sin(pi * goats$SLOPE / 180)
goats$ELEVATION <- scale(goats$ELEVATION)
goats$ET <- scale(goats$ET)
goats$TASP <- scale(goats$TASP)

## Fit two RSPF models:
## global availability (m=0) and bootstrap (B=99)
m1 <- rspf(STATUS ~ TASP + sin.SLOPE + ELEVATION, goats, m=0, B = 99)
m2 <- rspf(STATUS ~ TASP + ELEVATION, goats, m=0, B = 99)

## Inspect the summaries
summary(m1)
summary(m2)

## Compare models: looks like m1 is better supported
CAIC(m1, m2)

## Visualize the relationships
plot(m1)
mep(m1) # marginal effects similar to plot but with CIs
kdepairs(m1) # 2D kernel density estimates
plot(m2)
kdepairs(m2)
mep(m2)

## fit and compare to null RSF model (not available for RSPF)
m3 <- rsf(STATUS ~ TASP + ELEVATION, goats, m=0, B = 0)
m4 <- rsf.null(Y=goats$STATUS, m=0)
CAIC(m3, m4)
} # }
```
