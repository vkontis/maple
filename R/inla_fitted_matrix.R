#' A convenience function to extract the mortality matrix from a model fit.
#' @param model.fit A model fitted by INLA.
#' @param variable Variable to be extracted from the fitted values matrix; mean, median, etc.
inla_fitted_matrix <- function(model.fit, variable) {
    v <- model.fit$summary.fitted.values[[variable]]
    v <- v[order(model.fit$.args$data$year, model.fit$.args$data$age)]
    m <- matrix(v, nrow = length(unique(model.fit$.args$data$age)), 
                ncol = length(unique(model.fit$.args$data$year)))
    colnames(m) <- unique(model.fit$.args$data$year)
    rownames(m) <- unique(model.fit$.args$data$age)
    m
}
