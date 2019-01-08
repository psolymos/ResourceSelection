# ResourceSelection package version history

## Version 0.3-4, January 8, 2019

* Fixed family detection in `mep`.

## Version 0.3-3, January 7, 2019

* Fixed `vcov` method for `rsf.null` subclass: now returns `<0 x 0 matrix>`.
* Fixed `mep` function to properly handle the lack of `family(object)`,
  as a result now it is not failing for `rsf`/`rspf` models.
* New functions: `wrsi` and `sindex` for weighted selection index.

## Version 0.3-2, Feb 28, 2017

* New function: `rsf.null` for fitting 'no selection' RSF model.

## Version 0.3-1, Feb 10, 2017

* New function: `mep` for marginal effect plots.

## Version 0.3-0, Nov 4, 2016

* `vcov.rsf` with log link dropped non-existent intercept, now fixed.
* `ChangeLog` is now `NEWS.md`.
* `residuals` method added that follows the scaling used for the H-L
  test in the summaries.
* `predict` method failed due to missing intercepts, now fixed.
* Continuous integration and testing added to development suite.
* Documentation is revised.

## Version 0.2-6, Feb 15, 2016

* Fixed critical error messages in `rsf` and `rspf` functions that
  alert the user about the inclusion of continuous covariates.
* Log link was matched as logit in `rspf` call, now gives and error.
* Imports pbapply (not suggests).

## Version 0.2-5, Nov 6, 2015

* `predict.rsf` produced error with Exponential RSF
  when `se.fit = TRUE`. Coefficients were not augmented
  with a 0, thus a non-conformable arguments error.
  Reported by Martha Ellis.
* `ChangeLog` reformatted as markdown file.

## Version 0.2-4, May 19, 2014

* Cleaning up `:::` to satisfy R 3.0.2 check.
* `simulateUsedAvail` keeps column name when 1 variable supplied.

## Version 0.2-3, June 18, 2013

* Summary threw an error when H-L test failed.
  Now this is a warning. Reported by Tyler Muhly (AITF).
* `predict.rsf`: response is not required when newdata
  is provided. Bug reported and patch provided by
  Clement Calenge (3/7/2013).

## Version 0.2-2, Mar 5, 2013

* `predict.rsf`: `se.fit=TRUE` produced error with bootstrapped
  model. Bug reported by Clement Calenge.

## Version 0.2-1, Oct 3, 2012

* `rsf` estimation: intercept is now fixed at 0, and not
  part of the `optim` result.

## Version 0.2-0, Oct 1, 2012

* Generic and default method for `CAIC` added.
* Bugfix: SE in RSF case was wrong (subsetting the Hessian
  before inverting) -- now fixed.
* `predict` method for RSF could not deal with `newdata`
  due to missing intercept -- now fixed.

## Version 0.1-5, Sept 13 20, 2012

* new argument `m` in `rsf()`, `rspf()` for allowing
  local availability (matched use-available design).
* `goats` data set added from Lele and Keim 2006.
* Rd for `goats` contains an example analysis, too.
* `/inst/COPYING` removed.

## Version 0.1-4, Oct 18, 2011

* R (>= 2.13.0) dependency added
  to avoid `R CMD check ERROR` on old R
  (reported by Uwe Ligges).

## Version 0.1-3, August 23, 2011

* `.onLoad` added to `zzz.R`.
* `predict` method subsetted the result
  when `newdata` was provided. Reported by
  Rajapandian Kanagaraj (UFZ).

## Version 0.1-2, July 9, 2011

* `packageStartupMessage` used in `zzz.R` `.onAttach`.

## Version 0.1-1, June 7, 2011

* `kdepairs` plots `abs(cor)`, so negative values can get large.
* `hoslem.test`: only unique quantile values are used in `cut()`.

## Version 0.1-0, May 20, 2011

* Initial bunch of functions released on R-Forge and CRAN.
