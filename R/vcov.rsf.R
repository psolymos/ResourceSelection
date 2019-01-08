vcov.rsf <-
function (object, type, ...)
{
    if (inherits(object, "rsf.null"))
        return(matrix(0, 0, 0))
    boot <- object$bootstrap
    if (missing(type)) {
        type <- if (is.null(boot))
        "mle" else "boot"
    }
    type <- match.arg(type, c("mle", "boot"))
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
    np <- object$np
    if (type == "boot") {
        rval <- cov(t(boot))
    } else {
        rval <- matrix(NA, np, np)
        H <- .solvenear(object$results$hessian)
        if (object$link == "log")
        H <- H[-1,-1,drop=FALSE]
        rval[1:np, 1:np] <- H
    }
    rval <- data.matrix(rval)
    cf <- coef(object)
    colnames(rval) <- rownames(rval) <- names(cf)
    return(rval)
}

