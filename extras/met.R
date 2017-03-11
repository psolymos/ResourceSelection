## marginal effects test (met)

library(ResourceSelection)

.met <-
function(yobs, yfit, xobs, xfit,
wtd=TRUE, scale=c("y", "x"),
n=512, kernel="gaussian", bw="nrd0",
plot=TRUE, dmin=0, xlab, ylab, ...)
{
    scale <- match.arg(scale)
    if (missing(xfit))
        xfit <- xobs
    ok1 <- !is.na(xobs) & !is.na(yobs)
    xobs <- xobs[ok1]
    yobs <- yobs[ok1]
    ok2 <- !is.na(xfit) & !is.na(yfit)
    xfit <- xfit[ok2]
    yfit <- yfit[ok2]
    xcat <- is.factor(xobs)
    if (xcat) {
        if (!all(levels(xobs) == levels(xfit)))
            stop("xobs and xfit levels must be the same")
        if (any(table(xobs) < 1))
            stop("all levels in xobs must have >0 frequency")
        if (any(table(xfit) < 1))
            stop("all levels in xfit must have >0 frequency")
        if (wtd) {
            tabA <- table(x=xobs, y=yobs)
            tabA <- t(t(tabA) / colSums(tabA))
            snp <- tabA[,"1"] / tabA[,"0"]
            fA <- tabA[,"0"]
            fU <- tabA[,"1"]
        } else {
            snp <- aggregate(data.frame(y=yobs), list(x=xobs), mean)$y
            fA <- NA
            fU <- NA
        }
        sp <- aggregate(data.frame(y=yfit), list(x=xfit), mean)$y
        if (scale == "y")
            tsnp <- mean(sp) * snp / mean(snp)
        if (scale == "x")
            tsnp <- sp[1] * snp / snp[1]
        xx <- xobs[seq_len(nlevels(xobs))]
        xx[] <- levels(xobs)
    } else {
        int <- range(xobs, xfit)
        if (wtd) {
            fA <- density(xobs[yobs==0], bw=bw,
                kernel=kernel, n=n, from=int[1], to=int[2], ...)
            fU <- density(xobs[yobs==1], bw=fA$bw,
                kernel=kernel, n=n, from=int[1], to=int[2], ...)
            snp <- fU$y / fA$y
            sp <- predict(loess(yfit ~ xfit), data.frame(xfit=fA$x))
            xx <- fA$x
            fA <- fA$y
            fU <- fU$y
        } else {
            xx <- seq(int[1], int[2], len=n)
            snp <- predict(loess(yobs ~ xobs), data.frame(xobs=xx))
            sp <- predict(loess(yfit ~ xfit), data.frame(xfit=xx))
            fA <- NA
            fU <- NA
        }
        if (scale == "y")
            tsnp <- mean(sp) * snp / mean(snp)
        if (scale == "x")
            tsnp <- sp[floor(n/2)] * snp / snp[floor(n/2)]
    }
    out <- data.frame(x=xx,
        sobs=tsnp, sfit=sp,
        fA=fA, fU=fU)
    if (plot) {
        if (missing(ylab))
            ylab <- "y"
        if (missing(xlab))
            xlab <- "x"
        rn <- range(out$sobs, out$sfit)
        i <- rep(TRUE, nrow(out))
        if (!xcat && wtd)
            i <- out$fA >= dmin
        plot(out$x, out$sobs + 2*rn[2], type="n",
            ylim=rn, ylab=ylab, xlab=xlab, ...)
        if (!xcat && wtd) {
            polygon(c(out$x, rev(out$x)),
                c(rn[1]+diff(rn)*out$fA/max(out$fA), rep(rn[1],n)),
                col=rgb(0.8,0.8,0.8,0.4), border=NA)
            polygon(c(out$x, rev(out$x)),
                c(rn[1]+diff(rn)*out$fU/max(out$fU), rep(rn[1],n)),
                col=rgb(1,0.75,0.8,0.4), border=NA)
        }
        lines(out$x[i], out$sobs[i], col=4, lwd=2)
        lines(out$x[i], out$sfit[i], col=2, lwd=2)
    }
    invisible(out)
}

.met_binom <- function(yobs, yfit, x, ...)
    .met(yobs, yfit, x, x, wtd=TRUE, ...)
.met_pois <- function(yobs, yfit, x, ...) {
    i <- rep(seq_len(length(yobs)), yobs)
    xfit <- x
    xobs <- c(x[i], x)
    yobs <- c(rep(1, length(i)), rep(0, length(yobs)))
    .met(yobs, yfit, xobs, xfit, wtd=TRUE, ...)
}
.met_gauss <- function(yobs, yfit, x, ...)
    .met(yobs, yfit, x, x, wtd=FALSE, ...)

## grey=fA
## pink=fU
## blue=non-parametric fit
## red=parametric fit

## binomial

n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
x$x3 <- x$x1^2
cfs <- c(0,-1,1,-1)
## add this if you want categorical
#x$x4 <- as.factor(sample(LETTERS[1:4], n, replace=T, prob=c(4,2,1,1)))
#cfs <- c(0,-1,1,-1, 0.2,-0.2,0.05)

dat <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
f <- plogis(model.matrix(~.-status,dat) %*% cfs)
plot(dat$x1,f)
mod <- rspf(status ~ .-status, dat, m=0, B=0)

.met_binom(dat$status, fitted(mod), dat$x1)
.met_binom(dat$status, fitted(mod), dat$x2)
#.met_binom(dat$status, fitted(mod), dat$x4)

## poisson

lam <- exp(model.matrix(~.,x) %*% cfs)
y <- rpois(n, lam)
plot(x$x1, lam)
plot(x$x1, y)
mod <- glm(y ~ ., x, family=poisson)

.met_pois(y, fitted(mod), x$x1)
.met_pois(y, fitted(mod), x$x2)
.met_pois(y, fitted(mod), x$x4)

## gaussian

mu <- model.matrix(~.,x) %*% cfs
y <- rnorm(n, mu, 5)
plot(x$x1, mu)
plot(x$x1, y)
mod <- lm(y ~ ., x)

.met_gauss(y, fitted(mod), x$x1)
.met_gauss(y, fitted(mod), x$x2)
#.met_gauss(y, fitted(mod), x$x4)

