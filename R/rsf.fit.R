rsf.fit <-
function(X, Y, m, link = "logit", B = 99,
inits, method = "Nelder-Mead", control, ...)
{
    ## internal function for optim
    nll.fun <- function(parms, boot=id.all) {
        P.used <- drop(linkinvfun(X.used %*% parms))
        P.avail1 <- drop(linkinvfun(X.avail %*% parms))
        if (is.null(m.avail)) {
            P.avail <- rep(sum(P.avail1), N.used)
        } else {
            P.avail <- aggregate(P.avail1, list(m.avail), sum)
            P.avail <- P.avail[match(m.used, P.avail$Group),2]
        }
        ll.vec <- log(pmax(P.used / (P.used + P.avail), .Machine$double.eps))
        -sum(ll.vec[boot])
    }
    if (missing(control))
        control <- getOption("rspf.optim.control")

    ## inverse link function
    linkinvfun <- binomial(link=make.link(link))$linkinv
    if (link=="probit" && !getOption("rspf.robust.probit"))
        linkinvfun <- as.function(stats::pnorm)

    ## initial values from GLM if not defined in call
    if (missing(inits)) {
        inits <- suppressWarnings(glm.fit(X, Y, family=binomial())$coef)
        inits[is.na(inits)] <- 0
    }
    np <- ncol(X)

    ## handling Exponential case
#    if (link == "log" && length(inits) > np-1)
#        inits <- inits[-1]
#    if (link == "log")
#        np <- np - 1

    nam <- colnames(X)[(ncol(X)+1-np):ncol(X)]
    ## objects needed for optim
    X.used <- data.matrix(X[Y==1,])
    X.avail <- data.matrix(X[Y==0,])
    N.used <- nrow(X.used)
    N.avail <- nrow(X.avail)
    id.all <- 1:N.used

    ## this might change if matched point definition changes in the future
#    m <- 0
    if (missing(m))
        stop("'m' must be provided")
    m.avail <- NULL
    if (length(m) == 1) {
        if (m > 0) {
            m.used <- 1:N.used
            m.avail <- rep(1:N.used, each=m)
            if (length(m.avail) != N.avail)
                stop("'m' value incompatible with available points")
        }
    } else {
        if (length(m) != N.used + N.avail)
            stop("inappropriate length for 'm'")
        if (!all(m[Y==1] %in% m[Y==0]))
            stop("each used point must have matched available points in 'm'")
        if (!all(m[Y==0] %in% m[Y==1]))
            stop("matched points without used points detected in 'm'")
        m.used <- m[Y==1]
        m.avail <- m[Y==0]
    }

    ## optimization, point estimates
    results <- suppressWarnings(optim(inits, nll.fun, method = method,
        hessian = TRUE, control = control, boot = id.all))
    ## log likelihood
    ll <- -results$value
    ## point estimates
    cfs <- results$par
    names(cfs) <- nam
    H <- results$hessian
    ## checking Hessian, producing Std Errors
    if (rcond(H) <= 1e-06)
        ses <- rep(NA, np)
    if (rcond(H) > 1e-06) {
        ## due to negLogLik, we take H^-1 and not -H^-1
        opvar <- diag(.solvenear(H))
        if (any(opvar < 0)) {
            opvar[opvar < 0] <- NA
            warning("negative variance values in optim, NAs produced")
        }
        ses <- sqrt(opvar)
    }
    names(ses) <- nam
    ## optimization with bootstrap (this can be used for boostrap CI)
    if (B > 0) {
        id.boot <- lapply(1:B, function(i)
            sample(1:N.used, N.used, replace=TRUE))
        ## parallel computation can be added here
        boot.out <- if (requireNamespace("pbapply")) {
            pbapply::pbsapply(id.boot, function(z) suppressWarnings(optim(cfs, nll.fun,
                hessian = FALSE, method = method, control = control, boot = z))$par)
        } else {
            sapply(id.boot, function(z) suppressWarnings(optim(cfs, nll.fun,
                hessian = FALSE, method = method, control = control, boot = z))$par)
        }
        boot.out <- cbind(cfs, boot.out)
    } else boot.out <- NULL

    ## handling Exponential case: happens in rsf()
#    if (link == "log") {
#        cfs <- cfs[-1]
#        ses <- ses[-1]
#        np <- np - 1
#        boot.out <- boot.out[-1,,drop=FALSE]
#    }

    ## return value assembly
    out <- list(call = match.call(),
        y = Y,
        coefficients = cfs,
        std.error = ses,
        loglik = ll,
        results = results,
        link = link,
        control = control,
        inits = inits,
        m = m,
        np = np,
        nobs = N.used,
        bootstrap = boot.out,
        converged = results$convergence == 0)
#    out$fitted.values <- if (link == "log")
#        exp(drop(X %*% c(0, cfs))) else linkinvfun(drop(X %*% cfs))
    out
}
