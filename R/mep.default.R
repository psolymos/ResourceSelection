mep.default <-
function(object, which=NULL, link=NULL,
level=0.95, unique=10,
n=25, minbucket=5, digits=4,
col.points, col.lines=c(2, 2),
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
    if (inherits(fam, "family"))
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
