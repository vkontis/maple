print.maple_bma <- function(d) {
    cat("Model averaged projections of mortality and life expectancy between", 
        min(d$fitted.values$year), "and", paste0(max(d$fitted.values$year), "."), "\n")
    cat("Weights of models averaged:\n")
    print(d$model.weights)
    invisible(d)
}
