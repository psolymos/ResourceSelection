## marginal effects test (met)

.met <-
function(x, y, fit, link="log",
n=512, kernel="gaussian", bw="nrd0", fade=0.05, ...)
{
    ok <- !is.na(x) & !is.na(y) & !is.na(fit)
    x <- x[ok]
    y <- y[ok]
    fit <- fit[ok]

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
    meds <- aggregate(data.frame(fit=fit), list(C=C), median)
    sp <- meds[match(levels(C), meds[,"C"]),"fit"]
    sp[is.na(sp)] <- approxfun(fA$x[!is.na(sp)], sp[!is.na(sp)])(fA$x[is.na(sp)])

#    j <- which(pp$p>=0.5)[1]
#    tsnp <- sp[j] * snp / snp[j]
    tsnp <- mean(sp) * snp / mean(snp)

    out <- data.frame(x=fA$x, fA=fA$y, fU=fU$y,
        s=snp, sx=tsnp, y=sp, F=F)
    attr(out, "bw") <- bw
    attr(out, "kernel") <- kernel
    out
}

plot_met <- function(x, fade=0.05, ...) {

    w <- pmin(1/(1+exp(-50*(x$F-fade/2))),
        1/(1+exp(-50*((1-x$F)-fade/2))))

    h <- diff(range(x$y, x$sx)) * 0.01
    d <- sqrt(abs(x$y - x$sx))
    Col <- terrain.colors(100)[floor(99 * d / max(d)) + 1]

    plot(x$x, x$y, type="n", ylim=range(x$y, x$sx),
        ylab="y", xlab="x", ...)
    for (i in 1:(n-1))
        lines(x$x[c(i,i+1)],
            x$sx[c(i,i+1)],
            col=Col[i],
            lwd=w[i]*3+0.5)
    lines(x$x, x$y, col=2, lwd=2)
    invisible(x)
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

meds <- aggregate(data.frame(fit=fit), list(x=x), median)
tab1 <- table(x=x, y=y)
tab1 <- t(t(tab1)/colSums(tab1))

sp <- meds$fit
snp <- tab1[,"1"]/tab1[,"0"]
tsnp <- mean(sp) * snp / mean(snp)

plot(1:nlevels(x), sp, type="b", ylim=range(sp, tsnp))
lines(1:nlevels(x), tsnp, col=2)
