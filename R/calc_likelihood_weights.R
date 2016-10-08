calc_likelihood_weights <- function(years, weight.rate) {
    stopifnot(years == sort(years))
    w <- weight.rate * (1 - weight.rate) ^ (max(years) - years)
    w <- w / max(w)
    if (max(w) / min(w) > 1e5)
        warning("Weight rate too large, may cause instability")
    w
}
