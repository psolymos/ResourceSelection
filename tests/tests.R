#devtools::install_github("psolymos/ResourceSelection")
library(ResourceSelection)

## --- run examples with \dontrun sections ---

help_pages <- c("CAIC", "goats", "hoslem.test", "kdepairs",
    "makeUsedAvail", "rsf", "simulateUsedAvail")

for (i in help_pages) {
    cat("\n\n---------- ResourceSelection example:", i, "----------\n\n")
    eval(parse(text=paste0("example(", i,
        ", package = 'ResourceSelection', run.dontrun = TRUE)")))
}

## --- test methods ---

## settings
n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
cfs <- c(1.5,-1,0.5)
dat1 <- simulateUsedAvail(x, cfs, n.used, m, link="log")
m1 <- rsf(status ~ .-status, dat1, m=0, B=0)
m1b <- rsf(status ~ .-status, dat1, m=0, B=9)
dat2 <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
m2 <- rspf(status ~ .-status, dat2, m=0, B=0)
m2b <- rspf(status ~ .-status, dat2, m=0, B=9)

print(m1)
print(m1b)
print(m2)
print(m2b)

summary(m1)
summary(m1b)
summary(m2)
summary(m2b)

CAIC(m1, m2)

coef(m1)
coef(m2)

vcov(m1)
vcov(m1b)
vcov(m2)
vcov(m2b)

str(predict(m1, se.fit = FALSE))
str(predict(m1b, se.fit = FALSE))
str(predict(m2, se.fit = FALSE))
str(predict(m2b, se.fit = FALSE))

str(predict(m1, se.fit = FALSE, newdata = dat1))
str(predict(m1b, se.fit = FALSE, newdata = dat1))
str(predict(m2, se.fit = FALSE, newdata = dat1))
str(predict(m2b, se.fit = FALSE, newdata = dat1))

## cannot provide prediction Std Error without Bootstrap
tmp <- try(predict(m1, se.fit = TRUE))
stopifnot(inherits(tmp, "try-error"))
tmp <- try(predict(m2, se.fit = TRUE))
stopifnot(inherits(tmp, "try-error"))
tmp <- try(predict(m1, se.fit = TRUE, newdata = dat1))
stopifnot(inherits(tmp, "try-error"))
tmp <- try(predict(m2, se.fit = TRUE, newdata = dat1))
stopifnot(inherits(tmp, "try-error"))

str(predict(m1b, se.fit = TRUE))
str(predict(m2b, se.fit = TRUE))
str(predict(m1b, se.fit = TRUE, newdata = dat1))
str(predict(m2b, se.fit = TRUE, newdata = dat1))

confint(m1)
confint(m1b)
confint(m2)
confint(m2b)

str(model.matrix(m1))
str(model.matrix(m2))

str(model.frame(m1))
str(model.frame(m2))

logLik(m1)
logLik(m2)

kdepairs(m1)
kdepairs(m2)

str(fitted(m1))
str(fitted(m2))

str(residuals(m1))
str(residuals(m2))
