## marginal effects test (met)

library(ResourceSelection)

.get_met <- function(y, x, int,
wtd=TRUE, n=512, kernel="gaussian", bw="nrd0", ...)
{
    xcat <- is.factor(x)
    if (missing(int))
        int <- range(x)
    if (xcat) {
        if (wtd) {
            tabA <- table(x=x, y=y)
            tabA <- t(t(tabA) / colSums(tabA))
            s <- tabA[,"1"] / tabA[,"0"]
            fA <- tabA[,"0"]
            fU <- tabA[,"1"]
        } else {
            s <- aggregate(data.frame(y=y), list(x=x), mean)$y
            fA <- NA
            fU <- NA
        }
        xx <- x[seq_len(nlevels(x))]
        xx[] <- levels(x)
    } else {
        if (wtd) {
            fA <- density(x[y==0], bw=bw,
                kernel=kernel, n=n, from=int[1], to=int[2], ...)
            fU <- density(x[y==1], bw=fA$bw,
                kernel=kernel, n=n, from=int[1], to=int[2], ...)
            fA$y[fA$y == 0] <- .Machine$double.eps
            s <- fU$y / fA$y
            xx <- fA$x
            fA <- fA$y
            fU <- fU$y
        } else {
            xx <- seq(int[1], int[2], len=n)
            Ctrl <- loess.control(statistics="none", # requires R version 3.2.3
                surface = "interpolate", #"direct" # sloooow
                trace.hat="approximate")
            lss <- loess(y ~ x, control=Ctrl)
            s <- predict(lss, data.frame(x=xx))
            fA <- NA
            fU <- NA
        }
    }
    data.frame(x=xx, s=s, fA=fA, fU=fU)
}

.fit_met <-
function(yobs, yfit, xobs, xfit,
B=99, wtd=TRUE,
n=512, kernel="gaussian", bw="nrd0", ...)
{
    if (missing(xfit))
        xfit <- xobs
    int <- range(xobs, xfit, na.rm=TRUE)
    fit <- .get_met(yfit, xfit, int=int,
        wtd=FALSE, n=n, kernel=kernel, bw=bw, ...)
    obs <- .get_met(yobs, xobs, int=int,
        wtd=wtd, n=n, kernel=kernel, bw=bw, ...)
    L <- NULL
    if (B > 0) {
        N <- length(xobs)
        ## need to make sure categories are represented
        if (!is.null(dim(B))) {
            BB <- B
            B <- ncol(BB)
        } else {
            BB <- replicate(B, sample.int(N, N, replace=TRUE))
        }
        if (B < 1)
            stop("bootstrap iterations must be > 0")
        if (is.factor(xobs)) {
            Str <- as.integer(xobs)
            BBstr <- apply(data.matrix(BB), 2, function(z) Str[z])
            nstr <- apply(BBstr, 2, function(z) length(unique(z)))
            if (!all(nstr == length(unique(Str))))
                stop("levels not all represented in bootstrap, provide B")
        }
        L <- sapply(seq_len(B), function(i, ...) {
            .get_met(yobs[BB[,i]], xobs[BB[,i]],
                int=int, wtd=wtd, n=n, kernel=kernel, bw=bw, ...)$s
        })
    }
    ## predict.loess returns NA for out of range predictions
    ## if survece='interpolate', but surface='direct' takes forever
    Q <- t(apply(cbind(obs$s, L), 1, quantile,
        probs=c(0.25, 0.5, 0.75), na.rm=TRUE))
    colnames(Q) <- c("Q1", "Q2", "Q3")
    attr(Q, "B") <- B
    stats <- c(mean(yobs), quantile(yobs, seq(0, 1, by=0.25)))
    names(stats) <- c("Mean", "Min", "Q1", "Q2", "Q3", "Max")
    list(fit=fit[,c("x", "s")], obs=obs, Q=data.frame(Q), stats=stats)
}

.plot_met <- function(x, xlab, ylab, ...) {
    if (missing(ylab))
        ylab <- "response"
    if (missing(xlab))
        xlab <- "predictor"
    x$fit$s <- x$stats["Mean"] * x$fit$s / mean(x$fit$s)
    x$obs$s <- x$stats["Mean"] * x$obs$s / mean(x$obs$s)
    mq2 <- mean(x$Q$Q1)
    x$Q$Q1 <- x$stats["Mean"] * x$Q$Q1 / mq2
    x$Q$Q2 <- x$stats["Mean"] * x$Q$Q2 / mq2
    x$Q$Q3 <- x$stats["Mean"] * x$Q$Q3 / mq2
    Lim <- range(x$fit$s, x$obe$s, x$Q)
    plot(x$fit, type="n", ylim=Lim)
    polygon(c(x$obs$x, rev(x$obs$x)),
        c(x$Q$Q1, rev(x$Q$Q3)),
        border=NA, col="lightblue")
    lines(x$obs$x, x$Q$Q2, col=4, lwd=2, lty=1)
    lines(x$fit, col=2, lwd=2, lty=1)
    invisible(x)
}

.met <-
function(yobs, yfit, xobs, xfit,
B=99, wtd=TRUE, n=512, kernel="gaussian", bw="nrd0",
plot=TRUE, xlab, ylab, ...) {
    x <- .fit_met(yobs, yfit, xobs, xfit,
        B=B, wtd=wtd, n=n, kernel=kernel, bw=bw)
    .plot_met(x, xlab=xlab, ylab=ylab, ...)
}


.met_old <-
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

meptest <- function (object, ...)
    UseMethod("meptest")

meptest.default <-
function(object, which=NULL, ask, ylab, subset=NULL, ...)
{
    mf <- model.frame(object)
    fit <- fitted(object)
    Terms <- attr(mf, "terms")
    i_resp <- attr(Terms, "response")
    vars <- attr(Terms, "dataClasses")[-i_resp]
    y <- mf[,i_resp]
    uv <- unique(y)
    ruv <- round(uv)
    FUN <- .met_gauss
    sum_pos <- sum(y) > 0
    is_count <- isTRUE(all.equal(as.vector(y),
        as.integer(round(y + 0.001))))
    is_nonneg <- all(y >= 0)
    is_binary <- isTRUE(all.equal(as.vector(c(0, 1)),
        sort(as.integer(round(unique(y) + 0.001)))))
    if (is_count && is_nonneg && sum_pos)
        FUN <- .met_pois
    if (is_count && is_nonneg && sum_pos && is_binary)
        FUN <- .met_binom
    if (!is.null(subset)) {
        mf <- mf[subset,,drop=FALSE]
        fit <- fit[subset]
        y <- y[subset]
    }
    if (is.null(which))
        which <- names(vars)
    which <- if (is.character(which)) {
        which[match(names(vars), which)]
    } else {
        names(vars)[which]
    }
    which <- which[!is.na(which)]
    vars <- vars[which]
    np <- length(vars)
    if (np < 1)
        stop("must define at least one variable")
    if (missing(ask))
        ask <- prod(par("mfcol")) < np && dev.interactive()
    if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    out <- list()
    if (missing(ylab))
        ylab <- "response"
    for (i in seq_len(np)) {
        nam <- names(vars)[i]
        out[[nam]] <- FUN(y, fit, mf[,nam],
            ylab=ylab, xlab=nam, ...)
    }
    invisible(out)
}

## default method:
## similar to mep.default: guesses type and goes over x
## specific methods need to do better: rsf, glm, lm

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
meptest(mod)

mod <- glm(status ~ .-status, dat, family=binomial)

## poisson

lam <- exp(model.matrix(~.,x) %*% cfs)
y <- rpois(n, lam)
plot(x$x1, lam)
plot(x$x1, y)
mod <- glm(y ~ ., x, family=poisson)

.met_pois(y, fitted(mod), x$x1)
.met_pois(y, fitted(mod), x$x2)
.met_pois(y, fitted(mod), x$x3)
meptest(mod)

## gaussian

mu <- model.matrix(~.,x) %*% cfs
y <- rnorm(n, mu, 5)
plot(x$x1, mu)
plot(x$x1, y)
mod <- lm(y ~ ., x)

.met_gauss(y, fitted(mod), x$x1, B=99)
.met_gauss(y, fitted(mod), x$x2, B=99)
#.met_gauss(y, fitted(mod), x$x4)
meptest(mod)

## response
model.frame(mod)[,attr(terms(mod), "response")]

## similar functions
dismo::response

