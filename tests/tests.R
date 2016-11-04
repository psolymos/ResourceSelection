#devtools::install_github("psolymos/ResourceSelection")
library(ResourceSelection)

## --- run examples with \dontrun sections ---

help_pages <- c("CAIC", "goats", "hoslem.test", "kdepairs",
    "makeUsedAvail", "rsf", "simulateUsedAvail")

for (i in help_pages) {
    cat("\n\n---------- ResourceSelection example:", i, "----------\n\n")
    eval(parse(text=paste0("example(", i,
        ", package = 'ResourceSelection', run.dontrun = TRUE)")))
}
