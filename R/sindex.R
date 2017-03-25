sindex <-
function(y, x)
{
    Y <- ifelse(as.matrix(y) > 0, 1L, 0L)
    X <- data.matrix(x)
    if (nrow(X) != nrow(Y))
        stop("Dang! Dimension mismatch in inputs.")
    if (is.null(colnames(Y)))
        colnames(Y) <- paste0("S", seq_len(ncol(Y)))
    if (is.null(colnames(X)))
        colnames(X) <- paste0("V", seq_len(ncol(X)))
    res <- data.frame(apply(Y, 2, function(y)
        .wrsi(y, X=X)[,"rWRSI"]))
    rownames(res) <- colnames(X)
    colnames(res) <- colnames(Y)
    class(res) <- c("sindex", "data.frame")
    res
}
