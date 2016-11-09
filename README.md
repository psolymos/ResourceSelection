# ResourceSelection: Resource Selection (Probability) Functions for Use-Availability Data

[![CRAN version](http://www.r-pkg.org/badges/version/ResourceSelection)](http://cran.rstudio.com/web/packages/ResourceSelection/index.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/ResourceSelection)](http://cran.rstudio.com/web/packages/ResourceSelection/index.html)
[![Linux build status](https://travis-ci.org/psolymos/ResourceSelection.svg?branch=master)](https://travis-ci.org/psolymos/ResourceSelection)
[![Windows build status](https://ci.appveyor.com/api/projects/status/a4a31xk3k18ubdku?svg=true)](https://ci.appveyor.com/project/psolymos/resourceselection)
[![Code coverage status](https://codecov.io/gh/psolymos/ResourceSelection/branch/master/graph/badge.svg)](https://codecov.io/gh/psolymos/ResourceSelection)

Resource Selection (Probability) Functions
for use-availability wildlife data
based on weighted distributions as described in
Lele and Keim (2006), Lele (2009), and Solymos & Lele (2016).

## Install

```R
install.packages("ResourceSelection")
```

User visible changes in the package are listed in the [NEWS](https://github.com/psolymos/ResourceSelection/blob/master/NEWS.md) file.

## Report a problem

Use the [issue tracker](https://github.com/psolymos/ResourceSelection/issues)
to report a problem.

## License

[GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

## Example

```R
## Some data processing
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
# Call:
# rspf(formula = STATUS ~ TASP + sin.SLOPE + ELEVATION, data = goats, m = 0,
#     B = 99)
#
# Resource Selection Probability Function (Logistic RSPF) model
# Non-matched Used-Available design
# Maximum Likelihood estimates
# with Nonparametric Bootstrap standard errors (B = 99)
#
# Fitted probabilities:
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 1.947e-08 4.280e-07 9.977e-07 1.376e-06 1.924e-06 8.793e-06
#
# Coefficients (logit link):
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept) -16.89454    0.26284 -64.276   <2e-16 ***
# TASP          0.39116    0.01396  28.011   <2e-16 ***
# sin.SLOPE     5.36640    0.09740  55.098   <2e-16 ***
# ELEVATION     0.09829    0.01165   8.439   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Log-likelihood: -5.729e+04
# BIC = 1.146e+05
#
# Hosmer and Lemeshow goodness of fit (GOF) test:
# X-squared = 152.4, df = 8, p-value < 2.2e-16

summary(m2)
# Call:
# rspf(formula = STATUS ~ TASP + ELEVATION, data = goats, m = 0, B = 99)
#
# Resource Selection Probability Function (Logistic RSPF) model
# Non-matched Used-Available design
# Maximum Likelihood estimates
# with Nonparametric Bootstrap standard errors (B = 99)
#
# Fitted probabilities:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.01194 0.58010 0.86180 0.73660 0.95710 0.99830
#
# Coefficients (logit link):
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  1.62906    0.10110   16.11   <2e-16 ***
# TASP         1.86071    0.07751   24.01   <2e-16 ***
# ELEVATION    1.14338    0.08315   13.75   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Log-likelihood: -5.91e+04
# BIC = 1.182e+05
#
# Hosmer and Lemeshow goodness of fit (GOF) test:
# X-squared = 174.3, df = 8, p-value < 2.2e-16

## Compare models: looks like m1 is better supported
CAIC(m1, m2)
#    df     CAIC
# m1  4 114591.7
# m2  3 118225.2

## Visualize the reslationships
plot(m1)
kdepairs(m1)
plot(m2)
kdepairs(m2)
```

![](https://github.com/psolymos/ResourceSelection/raw/master/images/goats-m1.png)

## References

Lele, S.R. (2009)
A new method for estimation of resource selection probability function.
_Journal of Wildlife Management_ 73, 122--127. [[link](http://dx.doi.org/10.2193/2007-535)]

Lele, S. R. &  Keim, J. L. (2006)
Weighted distributions and estimation of resource selection probability functions.
_Ecology_ 87, 3021--3028. [[link](http://dx.doi.org/10.1890/0012-9658(2006)87[3021:WDAEOR]2.0.CO;2)]

Solymos, P. & Lele, S. R. (2016) Revisiting resource selection probability functions and single-visit methods: clarification and extensions. _Methods in Ecology and Evolution_ 7, 196--205. [[link](http://dx.doi.org/10.1111/2041-210X.12432), [preprint](http://arxiv.org/abs/1501.05880)]
