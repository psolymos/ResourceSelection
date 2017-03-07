## marginal effects test (met)

library(ResourceSelection)
data(goats)

## nonparametric estimate

m <- rsf(STATUS ~ ELEVATION, goats, m=0, B=0)
fit <- fitted(m)
y <- goats$STATUS
x <- goats$ELEVATION
n <- 512
kernel <- "gaussian"
bw <- "nrd0"
na.rm <- FALSE

int <- range(x, na.rm=na.rm)

fA <- density(x[y==0], bw=bw, kernel=kernel, n=n, from=int[1], to=int[2])
fU <- density(x[y==1], bw=fA$bw, kernel=kernel, n=n, from=int[1], to=int[2])
stopifnot(all(fU$x==fA$x))
snp <- fU$y / fA$y

incr <- (fA$x[length(fA$x)]-fA$x[1])/n
C <- cut(x, c(fA$x[1]-0.5*incr, fA$x+0.5*incr))
nlevels(C)
meds <- aggregate(fit, list(C=C), median)
sp <- meds[match(levels(C), meds[,1]),2]
## interpolate NA values
i <- match(meds[,1], levels(C))
j <- floor(length(x)/2)
k <- which.min(abs(fA$x - x[j]))

## transform snp to match range of sp

par(mfrow=c(2,1))
plot(fA, col=4, ylim=c(0, max(fA$y, fU$y)))
lines(fU, col=2)
rug(x[y==0], side=1, col=4)
rug(x[y==1], side=3, col=2)
plot(fA$x, sp, col=1, type="l", ylim=range(sp, snp, na.rm=TRUE))
lines(fA$x, snp, col=3)

