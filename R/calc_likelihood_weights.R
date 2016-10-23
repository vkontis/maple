#' Calculate likelihood weights.
#' @export
#' @param years A vector of the years that weights must be calculated for.
#' @param weight.rate The rate used to calculate the weights. Each year y is assigned weight proportional to (1 - weight.rate) ^ (Y - year), where Y is the last year.
#' @return A vector of weights. 
#' @examples 
#' calc_likelihood_weights(1980:2010, .05)
calc_likelihood_weights <- function(years, weight.rate) {
    stopifnot(years == sort(years))
    w <- weight.rate * (1 - weight.rate) ^ (max(years) - years)
    w <- w / max(w)
    if (max(w) / min(w) > 1e5)
        warning("Weight rate too large, may cause instability.")
    w
}
