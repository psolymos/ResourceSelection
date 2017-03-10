## marginal effects test (met)

## todo: use loess and predict.loess to smooth fitted values

.met <-
function(yobs, yfit, xobs, xfit, LINKINV,
n=512, kernel="gaussian", bw="nrd0",
plot=TRUE, xlab, ylab, ...)
{
    if (missing(xfit))
        xfit <- xobs
    ok1 <- !is.na(xobs) & !is.na(yobs)
    xobs <- xobs[ok1]
    yobs <- yobs[ok1]
    ok2 <- !is.na(xfit) & !is.na(yfit)
    xfit <- xfit[ok2]
    yfit <- yfit[ok2]
    if (missing(LINKINV))
        LINKINV <- binomial("log")$linkinv

    if (is.factor(xobs)) {
        if (!all(levels(xobs) == levels(xfit)))
            stop("xobs and xfit levels must be the same")
        if (any(table(xobs) < 1))
            stop("all levels in xobs must have >0 frequency")
        if (any(table(xfit) < 1))
            stop("all levels in xfit must have >0 frequency")

        tabA <- table(x=xobs, y=yobs)
        tabA <- t(t(tabA) / colSums(tabA))

        ## mean or median?
        sp <- aggregate(data.frame(y=yfit), list(x=xfit), mean)$y
        snp <- LINKINV(log(tabA[,"1"] / tabA[,"0"]))
        tsnp <- mean(sp) * snp / mean(snp)

        xx <- x[seq_len(nlevels(x))]
        xx[] <- levels(x)
        out <- data.frame(x=xx,
            sobs=tsnp, sfit=sp,
            fA=tabA[,"0"], fU=tabA[,"1"])

    } else {

        int <- range(xobs, xfit)

        fA <- density(xobs[yobs==0], bw=bw,
            kernel=kernel, n=n, from=int[1], to=int[2], ...)
        fU <- density(xobs[yobs==1], bw=fA$bw,
            kernel=kernel, n=n, from=int[1], to=int[2], ...)

        sp <- predict(loess(yfit ~ xfit), data.frame(xfit=fA$x))
        snp <- LINKINV(log(fU$y / fA$y))
        tsnp <- mean(sp) * snp / mean(snp)

        out <- data.frame(x=fA$x,
            sobs=tsnp, sfit=sp,
            fA=fA$y, fU=fU$y)
        attr(out, "bw") <- bw
        attr(out, "kernel") <- kernel

    }
    if (plot) {
        if (missing(ylab))
            ylab <- "y"
        if (missing(xlab))
            xlab <- "x"
        plot(out$x, out$sobs, type="n", col="white",
            ylim=range(out$sobs, out$sfit), ylab=ylab, xlab=xlab, ...)
        lines(out$x, out$sobs, col=4, lwd=2)
        lines(out$x, out$sfit, col=2, lwd=2)
    }
    invisible(out)
}

## todo:
## - figure out Gaussian version
## - implement method for rsf/rspf, Poisson/Binomial
## - think about NB, ZIP/ZINB



.met2 <-
function(x, y, fit, link="log",
n=512, kernel="gaussian", bw="nrd0",
plot=TRUE, xlab, ylab, ...)
{
    ok <- !is.na(x) & !is.na(y) & !is.na(fit)
    x <- x[ok]
    y <- y[ok]
    fit <- fit[ok]

    if (is.factor(x)) {
        if (any(table(x) < 1))
            stop("all levels must have >0 frequency")

        meds <- aggregate(data.frame(fit=fit), list(x=x), median)
        tab1 <- table(x=x, y=y)
        tab1 <- t(t(tab1)/colSums(tab1))

        sp <- meds$fit
        snp <- tab1[,"1"]/tab1[,"0"]
        snp <- binomial(link)$linkinv(log(snp))
        tsnp <- mean(sp) * snp / mean(snp)

        xx <- x[seq_len(nlevels(x))]
        xx[] <- levels(x)
        out <- data.frame(x=xx,
            fA=tab1[,"0"], fU=tab1[,"1"],
            s=snp, sx=tsnp, y=sp, F=NA)

    } else {

        o <- order(x, na.last = TRUE)
        used <- y > 0
        x <- x[o]
        y <- y[o]
        fit <- fit[o]

        int <- range(x)
        p <- rank(x, ties.method="average") / (length(x) + 1)

        fA <- density(x[y==0], bw=bw,
            kernel=kernel, n=n, from=int[1], to=int[2], ...)
        fU <- density(x[y==1], bw=fA$bw,
            kernel=kernel, n=n, from=int[1], to=int[2], ...)

        incr <- (fA$x[length(fA$x)]-fA$x[1])/n
        C <- cut(x, c(fA$x[1]-0.5*incr, fA$x+0.5*incr))

        ## need to smooth the tails
        pp <- aggregate(data.frame(p=p), list(C=C), max)
        F <- pp[match(levels(C), pp[,"C"]),"p"]
        F[is.na(F)] <- approxfun(fA$x[!is.na(F)], F[!is.na(F)])(fA$x[is.na(F)])

        #snp <- (w*fU$y + (1-w)*fA$y) / fA$y

        ## nonparametric
        snp <- fU$y / fA$y
        snp <- binomial(link)$linkinv(log(snp))

        ## parametruc fit medians MEP
#        meds <- aggregate(data.frame(fit=fit), list(C=C), median)
#        sp <- meds[match(levels(C), meds[,"C"]),"fit"]
#        sp[is.na(sp)] <- approxfun(fA$x[!is.na(sp)], sp[!is.na(sp)])(fA$x[is.na(sp)])
        spm <- loess(fit ~ x)
        sp <- predict(spm, data.frame(x=fA$x))

    #    j <- which(pp$p>=0.5)[1]
    #    tsnp <- sp[j] * snp / snp[j]
        tsnp <- mean(sp) * snp / mean(snp)

        out <- data.frame(x=fA$x, fA=fA$y, fU=fU$y,
            s=snp, sx=tsnp, y=sp, F=F)
        attr(out, "bw") <- bw
        attr(out, "kernel") <- kernel

        if (FALSE) {
            if (missing(ylab))
                ylab <- "y"
            if (missing(xlab))
                xlab <- "x"
            fade <- 0.05
            w <- pmin(1/(1+exp(-50*(out$F-fade/2))),
                1/(1+exp(-50*((1-out$F)-fade/2))))

            h <- diff(range(out$y, out$sx)) * 0.01
            d <- sqrt(abs(out$y - out$sx))
            Col <- terrain.colors(100)[floor(99 * d / max(d)) + 1]

            plot(out$x, out$y, type="n", ylim=range(out$y, out$sx),
                ylab=ylab, xlab=ylab, ...)
            for (i in 1:(n-1))
                lines(out$x[c(i,i+1)],
                    out$sx[c(i,i+1)],
                    #col=Col[i],
                    lwd=w[i]*3+0.5)
            lines(out$x, out$y, col=2, lwd=2)
        }
    }
    if (plot) {
        if (missing(ylab))
            ylab <- "y"
        if (missing(xlab))
            xlab <- "x"
        plot(out$x, out$y, type="n",
            ylim=range(out$y, out$sx), ylab=ylab, xlab=xlab, ...)
        lines(out$x, out$sx, col=4, lwd=2)
        lines(out$x, out$y, col=2, lwd=2)
    }
    invisible(out)
}


library(ResourceSelection)
data(goats)

## nonparametric estimate
elev <- goats$ELEVATION
elev <- drop(scale(goats$ELEVATION))
elev2 <- elev^2
elev3 <- elev^3
elev4 <- elev^4
elev5 <- elev^5

link <- "cloglog"
m1 <- rspf(STATUS ~ elev, goats, m=0, B=0, link=link)
m2 <- rspf(STATUS ~ elev + elev2, goats, m=0, B=0, link=link)
m3 <- rspf(STATUS ~ elev + elev2 + elev3, goats, m=0, B=0, link=link)
m4 <- rspf(STATUS ~ elev + elev2 + elev3 + elev4, goats, m=0, B=0, link=link)
m5 <- rspf(STATUS ~ elev + elev2 + elev3 + elev4 + elev5, goats, m=0, B=0, link=link)

m1 <- rsf(STATUS ~ elev, goats, m=0, B=0)
m2 <- rsf(STATUS ~ elev + elev2, goats, m=0, B=0)
m3 <- rsf(STATUS ~ elev + elev2 + elev3, goats, m=0, B=0)
m4 <- rsf(STATUS ~ elev + elev2 + elev3 + elev4, goats, m=0, B=0)
m5 <- rsf(STATUS ~ elev + elev2 + elev3 + elev4 + elev5, goats, m=0, B=0)

x1 <- .met(elev, goats$STATUS, fitted(m1), m1$link)
x2 <- .met(elev, goats$STATUS, fitted(m2), m1$link)
x3 <- .met(elev, goats$STATUS, fitted(m3), m1$link)
x4 <- .met(elev, goats$STATUS, fitted(m4), m1$link)
x5 <- .met(elev, goats$STATUS, fitted(m5), m1$link)

par(mfrow=c(2,2))
plot_met(x1)
plot_met(x2)
plot_met(x3)
plot_met(x4)
par(mfrow=c(1,1))

## transform snp to match range of sp

par(mfrow=c(2,1))
plot(fA, col=4, ylim=c(0, max(fA$y, fU$y)))
lines(fU, col=2)
#rug(x[y==0], side=1, col=4)
#rug(x[y==1], side=3, col=2)
plot(fA$x, sp, col=1, type="l", ylim=range(sp, tsnp, na.rm=TRUE))
lines(fA$x, tsnp, col=3)

par(mfrow=c(1,1))

x <- cut(goats$ELEVATION, c(200, seq(400, 2000, by=200)))
table(x,goats$STATUS,useNA="a")

m1 <- rsf(STATUS ~ x + SLOPE + ASPECT, goats, m=0, B=0)
fit <- fitted(m1)
y <- goats$STATUS

.met(goats$STATUS, fitted(m1), x, LINKINV=binomial(m1$link)$linkinv)
.met(goats$STATUS, fitted(m1), goats$SLOPE, LINKINV=binomial(m1$link)$linkinv)
.met(goats$STATUS, fitted(m1), goats$ASPECT, LINKINV=binomial(m1$link)$linkinv)


meds <- aggregate(data.frame(fit=fit), list(x=x), median)
tab1 <- table(x=x, y=y)
tab1 <- t(t(tab1)/colSums(tab1))

sp <- meds$fit
snp <- tab1[,"1"]/tab1[,"0"]
tsnp <- mean(sp) * snp / mean(snp)

plot(1:nlevels(x), sp, type="b", ylim=range(sp, tsnp))
lines(1:nlevels(x), tsnp, col=2)
