rsf.null <-
function(Y, m, ...)
{
    X <- data.matrix(rep(1, length(Y)))
    out <- rsf.fit(X=X, Y=Y, m=m, link = "log", B = 0, ...)
    out$coefficients <- out$coefficients[-1]
    out$std.error <- out$std.error[-1]
    out$np <- out$np - 1
    class(out) <- c("rsf.null", "rsf")
    out
}
