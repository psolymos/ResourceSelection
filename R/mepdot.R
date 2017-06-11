.mep <-
function(x, y, level=0.95, link=NULL,
type=c("numeric", "unique", "factor"),
n=25, minbucket=5, digits=4, fmax=1, fmin=0.05,
col.points, col.lines=c(4, 4),
pch=19, lty=c(1, 2), lwd=c(2, 2), plot=TRUE, ...)
{
    type <- match.arg(type)
    a <- c((1-level)/2, 1-(1-level)/2)
    x <- if (type == "factor")
        as.factor(x) else as.numeric(x)
    if (missing(col.points))
        ## pink
#        col.points <- rgb(0.9, 0, 0.9, max(1-min(length(y), 100)/100, 0.05))
        ## turquoise
        col.points <- rgb(0.25, 0.87, 0.81, max(1-min(length(y), 100)/100, 0.05))
    jitter_amount <- function (x, factor = 1, amount = NULL) {
        z <- diff(r <- range(x[is.finite(x)]))
        if (z == 0)
            z <- abs(r[1L])
        if (z == 0)
            z <- 1
        if (is.null(amount)) {
            d <- diff(xx <- unique(sort.int(round(x, 3 - floor(log10(z))))))
            d <- if (length(d))
                min(d)
            else if (xx != 0)
                xx/10
            else z/10
            amount <- factor/5 * abs(d)
        }
        else if (amount == 0)
            amount <- factor * (z/50)
        amount
    }
    jfun <- function(tmp, minbucket=5, fmax=1, fmin=0) {
        dd <- 0
        if (length(tmp) < minbucket) {
            if (length(tmp) > 1) {
                d <- dnorm(tmp, mean(tmp, na.rm=TRUE), sd(tmp, na.rm=TRUE))
                d <- d + fmin * max(d)
                dd <- amount * d / max(d)
            }
        } else {
            d <- density(tmp)
            d$y <- d$y + fmin * max(d$y)
            d$y <- amount * d$y / max(d$y)
            dd <- splinefun(d$x, d$y)(tmp)
            dd[dd < fmin] <- fmin
            dd[dd > amount] <- amount
        }
        runif(length(tmp), -dd, dd)
    }
    if (is.null(link)) {
        link <- "identity"
        if (all(y >= 0)) {
            link <- "log"
            if (all(y <= 1))
                link <- "logit"
        }
    }
    fam <- make.link(link)
    if (type == "numeric") {
        bs <- floor(length(x)/n)
        if (bs < minbucket)
            n <- floor(length(x)/minbucket)
        xx <- droplevels(cut(x, n, include.lowes=TRUE))
        if (length(unique(x)) < n || nlevels(xx) < 3)
            type <- "unique"
    }
    if (type == "numeric") {
        xv <- sapply(levels(xx), function(z) mean(x[xx == z]))
        yq <- t(sapply(levels(xx), function(z)
            quantile(fam$linkfun(y)[xx == z], c(0.5, a))))
        l1 <- lowess(xv, yq[,1])
        l2 <- lowess(xv, yq[,2])
        l3 <- lowess(xv, yq[,3])
        if (plot) {
            plot(y ~ x, col=col.points, pch=pch, ...)
            lines(l1$x, fam$linkinv(l1$y),
                 col=col.lines[1], lty=lty[1], lwd=lwd[1])
            lines(l2$x, fam$linkinv(pmin(l1$y, l2$y)),
                 col=col.lines[2], lty=lty[2], lwd=lwd[2])
            lines(l3$x, fam$linkinv(pmax(l1$y, l3$y)),
                 col=col.lines[2], lty=lty[2], lwd=lwd[2])
        }
    }
    if (type == "unique") {
        xp <- round(x, digits)
        xpi <- sort(unique(xp))
        n <- length(xpi)
        xv <- sapply(xpi, function(z) mean(x[xp == z]))
        yq <- t(sapply(xpi, function(z)
            quantile(fam$linkfun(y)[xp == z], c(0.5, a))))
        l1 <- lowess(xv, yq[,1])
        l2 <- lowess(xv, yq[,2])
        l3 <- lowess(xv, yq[,3])
        amount <- jitter_amount(xp, factor=fmax)
        o <- rep(0, length(xp))
        for (i in xpi)
            o[xp == i] <- jfun(y[xp == i], minbucket, fmax, fmin)
        xpj <- xp + o
        if (plot) {
            plot(y ~ xpj, axes=FALSE, col=col.points, pch=pch, ...)
            box()
            axis(1, xpi, xpi)
            axis(2)
            lines(l1$x, fam$linkinv(l1$y),
                 col=col.lines[1], lty=lty[1], lwd=lwd[1])
            lines(l2$x, fam$linkinv(pmin(l1$y, l2$y)),
                 col=col.lines[2], lty=lty[2], lwd=lwd[2])
            lines(l3$x, fam$linkinv(pmax(l1$y, l3$y)),
                 col=col.lines[2], lty=lty[2], lwd=lwd[2])
        }
    }
    if (type == "factor") {
        xv <- levels(x)
        n <- length(xv)
        yq <- t(sapply(xv, function(z)
            quantile(fam$linkfun(y)[x == z], c(0.5, a))))
        xp <- as.integer(x)
        xpi <- sort(unique(xp))
        amount <- jitter_amount(xp, factor=fmax)
        o <- rep(0, length(xp))
        for (i in xpi)
            o[xp == i] <- jfun(y[xp == i], minbucket, fmax, fmin)
        xpj <- xp + o
        if (plot) {
            plot(y ~ xpj, axes=FALSE, col=col.points, pch=pch, ...)
            box()
            axis(1, xpi, xv)
            axis(2)
            if (is.ordered(x)) {
                lines(seq_len(n), fam$linkinv(yq)[,1],
                    col=col.lines[1], lty=lty[1], lwd=lwd[1])
                lines(seq_len(n), fam$linkinv(yq)[,2],
                    col=col.lines[2], lty=lty[2], lwd=lwd[2])
                lines(seq_len(n), fam$linkinv(yq)[,3],
                    col=col.lines[2], lty=lty[2], lwd=lwd[2])
            } else {
                lines(rep(seq_len(n), each=2) + rep(c(-0.5, 0.5), n),
                    rep(fam$linkinv(yq)[,1], each=2),
                    col=col.lines[1], lty=lty[1], lwd=lwd[1])
                lines(rep(seq_len(n), each=2) + rep(c(-0.5, 0.5), n),
                    rep(fam$linkinv(yq)[,2], each=2),
                    col=col.lines[2], lty=lty[2], lwd=lwd[2])
                lines(rep(seq_len(n), each=2) + rep(c(-0.5, 0.5), n),
                    rep(fam$linkinv(yq)[,3], each=2),
                    col=col.lines[2], lty=lty[2], lwd=lwd[2])
            }
        }
    }
    invisible(list(x=xv, y=yq, link=link))
}
