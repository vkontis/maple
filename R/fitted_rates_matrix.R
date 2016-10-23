#' A convenience function to extract the mortality matrix from a model fit.
#' @export
#' @param model.fit A model fitted by INLA or Lee-Carter methods.
#' @return A matrix containing death rate model fits, with rows corresponding to age groups and columns to years.
#' @examples 
#' x <- maple_fit_model(maple_models()[[20]], deaths = maple.deaths, population = maple.population, forecast.horizon = 5)
#' fitted_rates_matrix(x)
fitted_rates_matrix <- function(model.fit) UseMethod("fitted_rates_matrix")

fitted_rates_matrix.inla <- function(model.fit) inla_fitted_matrix(model.fit, "mean")

fitted_rates_matrix.lc <- function(model.fit) model.fit$rates

inla_fitted_matrix <- function(model.fit, variable) {
    v <- model.fit$summary.fitted.values[[variable]]
    v <- v[order(model.fit$.args$data$year, model.fit$.args$data$age)]
    m <- matrix(v, nrow = length(unique(model.fit$.args$data$age)), 
                ncol = length(unique(model.fit$.args$data$year)))
    colnames(m) <- unique(model.fit$.args$data$year)
    rownames(m) <- unique(model.fit$.args$data$age)
    m
}
