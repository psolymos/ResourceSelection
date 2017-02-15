rsf <-
function(formula, data, m, B = 99,
inits, method = "Nelder-Mead", control,
model = TRUE, x = FALSE, ...)
{
    ## parsing formula
    if (missing(data))
        data <- parent.frame()
    mf <- match.call(expand.dots = FALSE)
    mm <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, mm)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    Y <- model.response(mf, "numeric")
    ff <- formula
    ff[[2]] <- NULL
    mt <- terms(ff, data = data)
    X <- model.matrix(mt, mf)
    Xlevels <- .getXlevels(mt, mf)
    ## check variables
    if (length(Y) < 1)
        stop("empty model")
    if (all(Y > 0))
        stop("invalid indicator variable, no zero value")
    if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y +
        0.001)))))
        stop("invalid indicator variable, non-integer values")
    Y <- as.integer(round(Y + 0.001))
    if (any(Y < 0))
        stop("invalid dependent variable, negative counts")
    if (any(!(Y %in% c(0, 1))))
        stop("invalid indicator variable, not in c(0, 1)")
    if (length(Y) != NROW(X))
        stop("invalid indicator variable, not a vector")
    ## need to have covariates (i.e. not only intercept)
    ## but that covariate can be factor/numeric etc.
    if (identical(as.character(ff[[2]]), "1"))
        stop("invalid formula, no covariates")

    ## fitting
    out1 <- rsf.fit(X=X, Y=Y, m=m, link = "log", B = B,
        inits=inits, method = method, control=control, ...)
    out1$coefficients <- out1$coefficients[-1]
    out1$std.error <- out1$std.error[-1]
    out1$np <- out1$np - 1
    out1$bootstrap <- out1$bootstrap[-1,,drop=FALSE]

    ## return value assembly
    out1$call <- match.call()
    out2 <- list(formula=formula,
        terms=mt,
        levels=Xlevels,
        contrasts=attr(X, "contrasts"),
        model= if (model) mf else NULL,
        x= if (x) X else NULL)
    out <- c(out1, out2)
    out$fitted.values <- exp(drop(X %*% c(0, out1$coefficients)))
    ## defining object class
    class(out) <- "rsf"
    out
}

