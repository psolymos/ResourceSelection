## this function calculates *raw* residuals
## other types of residuals require an assumption about the variance
residuals.rsf <-
function (object, type=c("all", "used", "avail"), ...)
{
    if (!identical(object$m, 0))
        stop("Residuals are only available for m=0 (global availability).")
    type <- match.arg(type)
    omega <- sum(object$y)/length(object$y)
    alpha <- mean(fitted(object, "avail"))
    fit <- fitted(object, "all")
    p <- (omega * fit) / (omega * fit + (1-omega)*alpha)
    res <- object$y - p
    switch(type,
        all = res,
        used = res[object$y==1],
        avail = res[object$y==0])
}
