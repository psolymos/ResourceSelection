## functions for exploratory data analysis and model checking

library(ResourceSelection)

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
        col.points <- rgb(0.9, 0, 0.9, max(1-min(length(y), 100)/100, 0.05))
        ## turquoise
#        col.points <- rgb(0.25, 0.87, 0.81, max(1-min(length(y), 100)/100, 0.05))
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

mep <-
function (object, ...)
    UseMethod("mep")

mep.default <-
function(object, which=NULL, link=NULL,
level=0.95, unique=10,
n=25, minbucket=5, digits=4,
col.points, col.lines=c(4, 4),
pch=19, lty=c(1, 2), lwd=c(2,2), ask,
subset=NULL, ylab, ...)
{
    mf <- model.frame(object)
    fit <- fitted(object)
    if (length(fit) != nrow(mf))
        stop("dimension mismatch")
    if (!is.null(subset)) {
        mf <- mf[subset,,drop=FALSE]
        fit <- fit[subset]
    }
    link <- NULL
    fam <- try(family(object), silent=TRUE)
    if (!inherits(fam, "try-error"))
        link <- fam$link
    Terms <- attr(mf, "terms")
    vars <- attr(Terms, "dataClasses")
    allvars <- colnames(get_all_vars(Terms, mf))
    vars <- vars[intersect(names(vars), allvars)]
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
        ylab <- "fitted values"
    for (i in seq_len(np)) {
        type <- vars[i]
        nam <- names(vars)[i]
        type <- if (type %in% c("factor", "ordered"))
            "factor" else "numeric"
        if (type == "numeric") {
            if (length(unique(round(mf[,nam], digits))) <= unique)
                type <- "unique"
        }
        out[[nam]] <- .mep(mf[,nam], fit,
            level=level, link=link, type=type,
            xlab=nam, ylab=ylab,
            n=n, minbucket=minbucket, digits=digits,
            col.points=col.points, col.lines=col.lines,
            pch=pch, lty=lty, lwd=lwd, plot=TRUE, ...)
    }
    invisible(out)
}

avp <-
function (object, ...)
    UseMethod("avp")

avp.default <-
function(object, z, h=0,
level=0.95, unique=10,
n=25, minbucket=5, digits=4,
col.points, col.lines=c(4, 4), col.h="grey",
pch=19, lty=c(1, 2), lwd=c(2,2), ask,
subset=NULL, ylab, ...)
{
    res <- residuals(object)
    z <- as.data.frame(z)
    if (length(res) != nrow(z))
        stop("dimension mismatch")
    if (!is.null(subset)) {
        z <- z[subset,,drop=FALSE]
        res <- res[subset]
    }
    vars <- sapply(z, function(z) class(z)[1])
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
        ylab <- "residuals"
    for (i in seq_len(np)) {
        type <- vars[i]
        nam <- names(vars)[i]
        type <- if (type %in% c("factor", "ordered"))
            "factor" else "numeric"
        if (type == "numeric") {
            if (length(unique(round(z[,nam], digits))) <= unique)
                type <- "unique"
        }
        out[[nam]] <- .mep(z[,nam], res,
            level=level, link="identity", type=type,
            xlab=nam, ylab=ylab,
            n=n, minbucket=minbucket, digits=digits,
            col.points=col.points, col.lines=col.lines,
            pch=pch, lty=lty, lwd=lwd, plot=TRUE, ...)
        abline(h=h, col=col.h)
    }
    invisible(out)
}

.get_met <- function(y, x, int=NULL,
wtd=TRUE, n=512, kernel="gaussian", bw="nrd0",
span=0.75, retry=10, ...)
{
    xcat <- is.factor(x)
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
        if (is.null(int))
            int <- range(x)
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
            lsspfun <- function(span = 0.75) {
                lss <- loess(y ~ x, span=span, control=Ctrl)
                predict(lss, data.frame(x=xx))
            }
            for (i in 1:retry) { # trys
                s <- suppressWarnings(try(lsspfun(span), silent=TRUE))
                if (inherits(s, "try-error")) {
                    span <- span + 0.05
                } else break
            }
            fA <- NA
            fU <- NA
        }
    }
    data.frame(x=xx, s=s, fA=fA, fU=fU)
}

.fit_met <-
function(yobs, xobs, yfit=NULL, xfit=NULL,
B=99, wtd=TRUE,
n=512, kernel="gaussian", bw="nrd0", ...)
{
    if (is.null(xfit))
        xfit <- xobs
    int <- if (is.factor(xobs))
        NULL else range(xobs, xfit, na.rm=TRUE)
    TYPE <- if (is.factor(xobs))
        "factor" else "numeric"
    if (is.ordered(xobs))
        TYPE <- "ordered"
    fit <- if (!is.null(yfit)) { # meptest or siplot
        .get_met(yfit, xfit, int=int, wtd=FALSE, n=n,
            kernel=kernel, bw=bw, ...)[,c("x", "s")]
    } else NULL
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
    ## if surface='interpolate', but surface='direct' takes forever
    Q <- t(apply(cbind(obs$s, L), 1, quantile,
        probs=c(0.25, 0.5, 0.75), na.rm=TRUE))
    colnames(Q) <- c("Q1", "Q2", "Q3")
    attr(Q, "B") <- B
    stats <- c(mean(yobs), quantile(yobs, seq(0, 1, by=0.25)))
    names(stats) <- c("Mean", "Min", "Q1", "Q2", "Q3", "Max")
    ## if yfit=NULL --> siplot, so fit=NULL
    list(fit=fit, obs=obs, Q=data.frame(Q), stats=stats, type=TYPE)
}

.plot_met <- function(x, xlab, ylab, ...) {
    if (missing(ylab))
        ylab <- "response"
    if (missing(xlab))
        xlab <- "predictor"
    if (is.null(x$fit)) {
        tmp <- data.frame(x=x$obs$x, s=x$Q$Q2)
    } else {
        tmp <- x$fit
    }
    tmp$s <- x$stats["Mean"] * tmp$s / mean(tmp$s)
    x$obs$s <- x$stats["Mean"] * x$obs$s / mean(x$obs$s)
    mq2 <- mean(x$Q$Q1)
    x$Q$Q1 <- x$stats["Mean"] * x$Q$Q1 / mq2
    x$Q$Q2 <- x$stats["Mean"] * x$Q$Q2 / mq2
    x$Q$Q3 <- x$stats["Mean"] * x$Q$Q3 / mq2
#    yim <- range(tmp$s, x$Q)
    if (is.null(list(...)$ylim)) {
        ylim <- range(tmp$s, x$Q)
        if (x$type == "numeric") {
            plot(tmp, type="n", xlab=xlab, ylab=ylab, ylim=ylim,
                col=NA, ...)
        } else {
            plot(tmp, type="n", xlab=xlab, ylab=ylab, ylim=lim,
                col=NA, border=NA, ...)
        }
    } else {
        if (x$type == "numeric") {
            plot(tmp, type="n", xlab=xlab, ylab=ylab,
                col=NA, ...)
        } else {
            plot(tmp, type="n", xlab=xlab, ylab=ylab,
                col=NA, border=NA, ...)
        }
    }
    ix <- iy <- seq_len(nrow(tmp))
    o <- 0
    if (x$type == "factor") {
        o <- c(-0.5, 0.5)
        ix <- rep(ix, each=2)
        iy <- rep(iy, each=2)
    }
    polygon(c(as.numeric(x$obs$x[ix])+o, rev(as.numeric(x$obs$x[ix])+o)),
        c(x$Q$Q1[iy], rev(x$Q$Q3[iy])),
        border=NA, col="lightblue")
    lines(as.numeric(x$obs$x[ix])+o, x$Q$Q2[iy], col=4, lwd=2, lty=1)
    if (!is.null(x$fit))
        lines(as.numeric(tmp$x[ix])+o, tmp$s[iy], col=2, lwd=2, lty=1)
    invisible(x)
}

.met <-
function(yobs, yfit, xobs, xfit,
B=99, wtd=TRUE, n=512, kernel="gaussian", bw="nrd0",
plot=TRUE, xlab, ylab, ...) {
    x <- .fit_met(yobs=yobs, xobs=xobs, yfit=yfit, xfit=xfit,
        B=B, wtd=wtd, n=n, kernel=kernel, bw=bw)
    .plot_met(x, xlab=xlab, ylab=ylab, ...)
}

.met_binom <- function(yobs, yfit, x, xlab, ylab, ...)
    .met(yobs, yfit, x, x, wtd=TRUE, xlab=xlab, ylab=ylab, ...)
.met_pois <- function(yobs, yfit, x, xlab, ylab, ...) {
    i <- rep(seq_len(length(yobs)), yobs)
    xfit <- x
    xobs <- c(x[i], x)
    yobs <- c(rep(1, length(i)), rep(0, length(yobs)))
    .met(yobs, yfit, xobs, xfit, wtd=TRUE, xlab=xlab, ylab=ylab, ...)
}
.met_gauss <- function(yobs, yfit, x, xlab, ylab, ...)
    .met(yobs, yfit, x, x, wtd=FALSE, xlab=xlab, ylab=ylab, ...)

.mep_engine <-
function(object, sip=FALSE, which=NULL, ask, ylab, subset=NULL, ...)
{
    mf <- model.frame(object)
    fit <- if (sip)
        NULL else fitted(object)
    Terms <- attr(mf, "terms")
    i_resp <- attr(Terms, "response")
    vars <- attr(Terms, "dataClasses")[-i_resp]
    allvars <- colnames(get_all_vars(Terms, mf))
    vars <- vars[intersect(names(vars), allvars)]
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
        if (!is.null(fit))
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

.get_frame <-
function(formula, data, type="numeric")
{
    if (missing(data))
        data <- parent.frame()
    mf <- match.call(expand.dots = FALSE)
    mm <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, mm)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    ff <- formula
    ff[[2]] <- NULL
    mt <- terms(ff, data = data)
    X <- model.matrix(mt, mf)
    list(
        call=match.call(),
        formula=formula,
        terms=mt,
        levels=.getXlevels(mt, mf),
        contrasts=attr(X, "contrasts"),
        model=mf,
        x=X,
        y=model.response(mf, type))
}

## meptest: selection index and fit

meptest <- function (object, ...)
    UseMethod("meptest")

meptest.default <-
function(object, which=NULL, ask, ylab, subset=NULL, ...)
{
    .mep_engine(object, sip=FALSE,
        which=which, ask=ask, ylab=ylab, subset=subset, ...)
}

## siplot: selection index plot, without model fit

siplot <- function (object, ...)
    UseMethod("siplot")

siplot.default <-
function(y, x, which=NULL, ask, ylab, subset=NULL, ...)
{
    if (NCOL(x) < 2L)
        x <- as.data.frame(x=x)
    object <- .get_frame(formula=y ~ ., data=x, type="numeric")
    .mep_engine(object, sip=TRUE,
        which=which, ask=ask, ylab=ylab, subset=subset, ...)
}

siplot.formula <-
function(formula, data, which=NULL, ask, ylab, subset=NULL, ...)
{
    object <- .get_frame(formula=formula, data=data, type="numeric")
    .mep_engine(object, sip=TRUE,
        which=which, ask=ask, ylab=ylab, subset=subset, ...)
}

## calculate PI for lm/glm

predict_sim <-
function(object, newdata=NULL,
interval = c("none", "confidence", "prediction"),
type=c("asymp", "pboot", "npboot"),
level=0.95, B=99, ...) {
    interval <- match.arg(interval)
    type <- match.arg(type)
    if (is.null(newdata)) {
        x <- model.frame(object)
        X <- model.matrix(object)
    } else {
        x <- model.frame(delete.response(terms(object)), newdata)
        X <- model.matrix(attr(x, "terms"), x)
    }
    n <- nrow(x)
    fun <- switch(family(object)$family,
        "gaussian"=function(x) rnorm(length(x), x, summary(object)$sigma),
        "poisson"= function(x) rpois(length(x), x),
        "binomial"=function(x) rbinom(length(x), 1, x))
    if (interval=="none")
        return(predict(object, newdata))
    if (B < 2)
        stop("Are you kidding? B must be > 1")
    if (type == "asymp") {
        cm <- rbind(coef(object),
            MASS::mvrnorm(B, coef(object), vcov(object)))
        fm <- apply(cm, 1, function(z) X %*% z)
    }
    if (type == "boot") {
        cm <- matrix(0, B+1, length(coef(object)))
        cm[1,] <- coef(object)
        xx <- model.frame(object)
        for (i in 2:B) {
            j <- sample.int(n, n, replace=TRUE)
            cm[i,] <- coef(update(object, data=xx[j,]))
        }
        fm <- apply(cm, 1, function(z) X %*% z)
    }
    if (type == "npboot") {
        cm <- matrix(0, B+1, length(coef(object)))
        cm[1,] <- coef(object)
        xx <- model.frame(object)
        j <- attr(attr(xx, "terms"), "response")
        f <- fitted(object)
        for (i in 2:B) {
            xx[,j] <- fun(f)
            cm[i,] <- coef(update(object, data=xx))
        }
        fm <- apply(cm, 1, function(z) X %*% z)
    }
    fm <- family(fit0)$linkinv(fm)
    if (interval=="prediction") {
        y <- matrix(fun(fm), n, B+1)
    } else {
        y <- fm
    }
    rownames(y) <- rownames(x)
    p <- c(0.5, (1-level)/2, 1-(1-level)/2)
    stat_fun <- function(x)
        c(mean(x), sd(x), quantile(x, p))
    out <- cbind(fm[,1], t(apply(y, 1, stat_fun)))
    colnames(out) <- c("fit", "mean", "se", "median", "lwr", "upr")
    out[,c("fit", "lwr", "upr", "mean", "median", "se")]
}

.r2_fun <-
function(observed, fitted, distr=c("binomial", "poisson"),
size=1, null=NULL, p=0)
{
    distr <- match.arg(distr)
    if (distr == "poisson") {
        if (is.null(null))
            null <- mean(observed)
        ll0 <- sum(dpois(observed, null, log=TRUE))
        lls <- sum(dpois(observed, observed, log=TRUE))
        llf <- sum(dpois(observed, fitted, log=TRUE))
    } else {
        if (is.null(null))
            null <- mean(observed/size)
        ll0 <- sum(dbinom(observed, size, null, log=TRUE))
        lls <- sum(dbinom(observed, size, observed/size, log=TRUE))
        llf <- sum(dbinom(observed, size, fitted, log=TRUE))
    }
    n <- length(observed)
    R2 <- 1 - (lls - llf) / (lls - ll0)
    R2adj <- 1 - (1 - R2) * ((n-1) / (n-(p+1)))
    D0 <- -2 * (ll0 - lls)
    DR <- -2 * (llf - lls)
    p_value <- 1 - pchisq(D0 - DR, p)
    c(R2=R2, R2adj=R2adj, Deviance=D0 - DR, Dev0=D0, DevR=DR, df=p,
        p_value=p_value)
}

R2dev <-
function(object, ...) {
    y <- model.response(model.frame(object), "numeric")
    f <- fitted(object)
    .r2_fun(y, fitted(object),
        distr=family(object)$family, size=1, null=NULL,
        p=length(coef(object))-1)
}
