rsf.null <-
function(Y, m, inits, ...)
{
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

    X <- data.matrix(rep(1, length(Y)))
    if (missing(inits))
        inits <- log(sum(Y==1)/sum(Y==0))
    out1 <- rsf.fit(X=X, Y=Y, m=m, link = "log", B = 0, inits=inits, ...)
    out1$coefficients <- out1$coefficients[-1]
    out1$std.error <- out1$std.error[-1]
    out1$np <- out1$np - 1
    out1$call <- match.call()
    out2 <- list(formula=NULL,
        terms=NULL,
        levels=NULL,
        contrasts=NULL,
        model=NULL,
        x=NULL)
    out <- c(out1, out2)
    out$fitted.values <- rep(1, length(Y))
    class(out) <- c("rsf.null", "rsf")
    out
}
