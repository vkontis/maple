#' A convenience function to extract the mortality matrix from a model fit.
#' @param model.fit A model fitted by maple_fit_model.
#' @return A matrix containing the estimated death rates, with rows corresponding to age groups and columns to years.
#' @examples
#' x <- maple_fit_model(maple_models()[[20]], deaths = maple.deaths, population = maple.population, forecast.horizon = 5)
#' @export
fitted_rates_matrix <- function(model.fit) UseMethod("fitted_rates_matrix")

fitted_rates_matrix.inla <- function(model.fit) inla_fitted_matrix(model.fit, "mean")

fitted_rates_matrix.lc <- function(model.fit) model.fit$rates
