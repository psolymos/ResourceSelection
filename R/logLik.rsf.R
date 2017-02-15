logLik.rsf <-
function (object, ...)
{
    structure(object$loglik,
        df = object$np,
        class = "logLik")
}

