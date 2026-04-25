# Mountain Goats Data Set

GPS collar data of mountain goats (*Oreamnos americanus*) from Lele and
Keim (2006).

## Usage

``` r
data(goats)
```

## Format

A data frame with 19014 observations on the following 8 variables.

- `STATUS`:

  a numeric vector, 1: used, 0: available

- `ID`:

  a numeric vector, individuals

- `ELEVATION`:

  a numeric vector (m)

- `SLOPE`:

  a numeric vector (degrees, steep)

- `ET`:

  a numeric vector, access to escape terrain (distance from steep
  slopes, m)

- `ASPECT`:

  a numeric vector (degrees)

- `HLI`:

  a numeric vector, heat load index (0-1)

- `TASP`:

  a numeric vector, transformed aspect

## Details

Mountain goat telemetry data were collected in the Coast Mountains of
northwest British Columbia, Canada, as described in Lele and Keim
(2006).

## Source

Ecological Archives E087-181-S1,
<http://www.esapubs.org/archive/ecol/E087/181/>

## References

Lele, S. R. & Keim, J. L. (2006) Weighted distributions and estimation
of resource selection probability functions. Ecology 87, 3021–3028.

## Examples

``` r
data(goats)
str(goats)
#> 'data.frame':    19014 obs. of  8 variables:
#>  $ STATUS   : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ ID       : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ ELEVATION: int  651 660 316 334 454 343 429 493 400 442 ...
#>  $ SLOPE    : num  38.5 39.7 20.5 34.1 41.6 ...
#>  $ ET       : num  35.4 70.7 50 35.4 25 ...
#>  $ ASPECT   : num  243 270 279 266 258 ...
#>  $ HLI      : num  0.918 0.884 0.713 0.864 0.935 ...
#>  $ TASP     : num  0.947 0.699 0.575 0.745 0.829 ...
summary(goats)
#>      STATUS             ID           ELEVATION        SLOPE      
#>  Min.   :0.0000   Min.   : 1.000   Min.   : 248   Min.   : 0.00  
#>  1st Qu.:0.0000   1st Qu.: 3.000   1st Qu.: 685   1st Qu.:25.41  
#>  Median :0.0000   Median : 6.000   Median : 985   Median :34.18  
#>  Mean   :0.3333   Mean   : 5.785   Mean   :1000   Mean   :33.65  
#>  3rd Qu.:1.0000   3rd Qu.: 8.000   3rd Qu.:1322   3rd Qu.:42.68  
#>  Max.   :1.0000   Max.   :10.000   Max.   :1966   Max.   :70.80  
#>        ET            ASPECT           HLI               TASP         
#>  Min.   :  0.0   Min.   : -1.0   Min.   :0.07749   Min.   :-1.00000  
#>  1st Qu.: 25.0   1st Qu.:139.5   1st Qu.:0.53882   1st Qu.:-0.04676  
#>  Median : 75.0   Median :178.2   Median :0.73698   Median : 0.53151  
#>  Mean   :126.8   Mean   :180.7   Mean   :0.68497   Mean   : 0.33343  
#>  3rd Qu.:180.3   3rd Qu.:229.6   3rd Qu.:0.86565   3rd Qu.: 0.86441  
#>  Max.   :995.3   Max.   :359.6   Max.   :0.98321   Max.   : 1.00000  

if (FALSE) { # \dontrun{
goats$exp.HLI <- exp(goats$HLI)
goats$sin.SLOPE <- sin(pi * goats$SLOPE / 180)
goats$ELEVATION <- scale(goats$ELEVATION)
goats$ET <- scale(goats$ET)
goats$TASP <- scale(goats$TASP)
m1 <- rspf(STATUS ~ TASP + sin.SLOPE + ELEVATION, goats, m=0, B = 99)
m2 <- rspf(STATUS ~ TASP + ELEVATION, goats, m=0, B = 99)
summary(m1)
summary(m2)
AIC(m1, m2)
plot(m1)
} # }
```
