hoslem.test <- function (x, y, g = 10) {
    if (g < 2L)
        stop("Argument g must be at least 2.")
    DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), 
        sep = ", ")
    METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
    yhat <- y
    y <- x
    qq <- unique(quantile(yhat, probs = seq(0, 1, 1/g)))
    cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
    observed <- xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
    expected <- xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ 
        cutyhat)
    chisq <- sum((observed - expected)^2/expected)
    G <- length(unique(cutyhat))
    if (G < 2L)
        stop("The number of bins led to negative df.")
    PVAL = 1 - pchisq(chisq, G - 2)
    PARAMETER <- G - 2
    names(chisq) <- "X-squared"
    names(PARAMETER) <- "df"
    if (G < g)
        warning("The data did not allow for the requested number of bins.")
    structure(list(statistic = chisq, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME, observed = observed, 
        expected = expected), class = "htest")
}
