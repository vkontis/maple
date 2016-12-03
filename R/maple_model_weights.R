#' Calculate model weights given their projection errors.
#' @param projection.errors A numeric vector of projection errors of individual models.
#' @return A vector of model weights.
#' @examples
#' maple_model_weights(c(0.1, 1, 10))
#' maple_model_weights(c(5, 5))
#' @export
maple_model_weights <- function(projection.errors) {
    exp(- abs(projection.errors)) / sum(exp(- abs(projection.errors)))
}
