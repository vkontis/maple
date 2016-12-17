#' @export
print.maple.bma <- function(x, ...) {
    cat("Model averaged projections of mortality and life expectancy between ",
        paste(range(x$sample.summaries$year), collapse = " and "), ".\n", sep = "")
    cat("Weights of models averaged:\n")
    print(x$model.weights)
    invisible(x)
}

#' @export
print.inla.model <- function(x, ...) {
    cat("Model", x$name, ": ")
    cat(x$desc, "\n")
    cat("INLA formula:\n")
    print(x$fml)
    if (!is.null(x$likelihood.weight.rate))
        cat("Weight rate:", x$likelihood.weight.rate, "\n")
    invisible(x)
}

#' @export
print.lc.model <- function(x, ...) {
    cat("Model", x$name, ": ")
    cat(x$desc, "\n")
    invisible(x)
}
