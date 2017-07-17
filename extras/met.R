## TODO: implement conditioning (i.e. different SI by levels)

## marginal effects test (met)

library(ResourceSelection)

.get_met <- function(y, x, int=NULL,
wtd=TRUE, n=512, kernel="gaussian", bw="nrd0", ...)
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
            lss <- loess(y ~ x, control=Ctrl)
            s <- predict(lss, data.frame(x=xx))
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
    Lim <- range(tmp$s, x$Q)
    if (x$type == "numeric") {
        plot(tmp, type="n", ylim=Lim, xlab=xlab, ylab=ylab,
            col=NA, ...)
    } else {
        plot(tmp, type="n", ylim=Lim, xlab=xlab, ylab=ylab,
            col=NA, border=NA, ...)
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

