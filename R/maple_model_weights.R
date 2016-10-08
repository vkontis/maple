maple_model_weights <- function(projection.errors) {
    exp(- abs(projection.errors)) / sum(exp(- abs(projection.errors)))
}