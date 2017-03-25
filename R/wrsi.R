wrsi <-
function(y, x)
{
    Y <- ifelse(y > 0, 1L, 0L)
    X <- data.matrix(x)
    n <- length(Y)
    if (nrow(X) != n)
        stop("Dang! Dimension mismatch in inputs.")
    if (is.null(colnames(X)))
        colnames(X) <- paste0("V", seq_len(ncol(X)))
    res <- data.frame(.wrsi(Y, X))
    rownames(res) <- colnames(X)
    class(res) <- c("wrsi", "data.frame")
    res
}
