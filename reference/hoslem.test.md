# Hosmer-Lemeshow Goodness of Fit (GOF) Test

Hosmer-Lemeshow Goodness of Fit (GOF) Test.

## Usage

``` r
hoslem.test(x, y, g = 10)
```

## Arguments

- x:

  a numeric vector of observations, binary (0/1).

- y:

  expected values.

- g:

  number of bins to use to calculate quantiles. Needs to be at least 2.
  The degrees of freedom of the test is `g-2` but the number of bins
  might also depend on the data (i.e. due to identical quantile values
  that lead to empty bins). In case when the number of bins does not
  equal `g`, the degrees of freedom will depend on the number of bins
  that is less than `g`. In such case a warning is produced.

## Details

The Hosmer-Lemeshow test is a statistical test for goodness of fit for
logistic regression models.

## Value

A list with class `"htest"` containing the following components:

- statistic:

  the value of the chi-squared test statistic,
  (`sum((observed - expected)^2 / expected)`).

- parameter:

  the degrees of freedom of the approximate chi-squared distribution of
  the test statistic (`g - 2`).

- p.value:

  the p-value for the test.

- method:

  a character string indicating the type of test performed.

- data.name:

  a character string giving the name(s) of the data.

- observed:

  the observed frequencies in a `g`-by-2 contingency table.

- expected:

  the expected frequencies in a `g`-by-2 contingency table.

## References

Hosmer D W, Lemeshow S 2000. Applied Logistic Regression. New York, USA:
John Wiley and Sons.

## Author

Peter Solymos by adapting code pieces from R help mailing list

## Examples

``` r
set.seed(123)
n <- 500
x <- rnorm(n)
y <- rbinom(n, 1, plogis(0.1 + 0.5*x))
m <- glm(y ~ x, family=binomial)
hoslem.test(m$y, fitted(m))
#> 
#>  Hosmer and Lemeshow goodness of fit (GOF) test
#> 
#> data:  m$y, fitted(m)
#> X-squared = 4.5227, df = 8, p-value = 0.8072
#> 
```
