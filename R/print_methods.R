#' @export
print.maple.bma <- function(d) {
    cat("Model averaged projections of mortality and life expectancy between ", 
        paste(range(bma$sample.summaries$year), collapse = " and "), ".\n", sep = "")
    cat("Weights of models averaged:\n")
    print(d$model.weights)
    invisible(d)
}

#' @export
print.inla.model <- function(m) {
    cat("Model", m$name, ": ")
    cat(m$desc, "\n")
    cat("INLA formula:\n")
    print(m$fml)
    if (!is.null(m$likelihood.weight.rate))
        cat("Weight rate:", m$likelihood.weight.rate, "\n")
    invisible(m)
}

#' @export
print.lc.model <- function(m) {
    cat("Model", m$name, ": ")
    cat(m$desc, "\n")
    invisible(m)
}
