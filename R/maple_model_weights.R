#' Calculate model weights given their projection errors.
#' @export
#' @param projection.errors A vector of projection errors of individual models. 
#' @return A vector of model weights.
#' @examples 
#' maple_model_weights(c(0.1, 1, 10))
#' maple_model_weights(c(5, 5))
maple_model_weights <- function(projection.errors) {
    exp(- abs(projection.errors)) / sum(exp(- abs(projection.errors)))
}