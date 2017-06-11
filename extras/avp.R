## added variable plot (avp)
## see also spatstat::lurking()

library(ResourceSelection)

avp <-
function (object, ...)
    UseMethod("avp")

avp.default <-
function(object, z, h=0,
level=0.95, unique=10,
n=25, minbucket=5, digits=4,
col.points, col.lines=c(2, 2), col.h="grey",
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
