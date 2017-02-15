predict.rsf <-
function(object, newdata = NULL, type = c("link", "response"),
part = c("avail", "used", "all"), se.fit = FALSE, ...){
    if (inherits(object, "rsf.null"))
        stop("predict not available for rsf.null (no selection)")
    type <- match.arg(type)
    part <- match.arg(part)
    if (se.fit) {
        boot <- object$bootstrap
        if (is.null(boot))
            stop("cannot provide prediction Std Error without Bootstrap")
        Bp <- ncol(boot)
        nps <- sapply(object$coefficients, length)
    }
    if (is.null(newdata)) {
        id <- switch(part,
            avail = which(object$y == 0),
            used = which(object$y == 1),
            all = 1:length(object$y))
        rval <- fitted(object)[id]
        ## fitted gives 'response', need to use linkfun for 'link'
        if (type == "link")
            ## binomial("probit")$linkfun is qnorm
            rval <- binomial(object$link)$linkfun(rval)
        if (se.fit)
            X <- model.matrix(object)[id,]
    } else {
        ## response is not needed -- get rid of it
        rhs <- model.frame(delete.response(terms(object)), newdata)
        X <- model.matrix(attr(rhs, "terms"), rhs)
        if (object$link == "log")
            X <- X[,-1L,drop=FALSE]
        rval <- drop(X %*% coef(object))
        if (type == "response") {
            rval <- binomial(object$link)$linkinv(rval)
        }
    }
    if (se.fit) {
        se <- sapply(1:Bp, function(i) drop(X %*% boot[,i]))
        if (type == "response") {
            se <- binomial(object$link)$linkinv(se)
        }
        se <- apply(se, 1, sd)
        rval <- list(fit = rval, se.fit = se)
    }
    rval
}
