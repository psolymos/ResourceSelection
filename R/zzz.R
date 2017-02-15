.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    options("pboptions" = list(
        type = if (interactive()) "timer" else "none",
        char = "[=-]",
        txt.width = 50,
        gui.width = 300,
        style = 6,
        initial = 0,
        title = "R progress bar",
        label = "",
        nout = 100L,
        min_time = 2))
    invisible(NULL)
}

.onLoad <- function(libname, pkgname){
    if (is.null(getOption("rspf.optim.control")))
        options("rspf.optim.control"=list(maxit = 20000))
    if (is.null(getOption("rspf.robust.probit")))
        options("rspf.robust.probit"=FALSE)
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("rspf.optim.control"=NULL)
    options("rspf.robust.probit"=NULL)
    invisible(NULL)
}
