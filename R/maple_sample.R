#' Generate posterior samples of death rates and life tables. For models fitted with INLA, this uses INLA's inla.posterion.sample() function. For Lee Carter models, it samples from the distribution of the fitted parameters, using the estimated mean and standard deviation.
#' @param model.fit A fitted model.
#' @param num.draws The number of samples.
#' @param ax The number of years lived on average by those who die in their current age group. This is used to calculate life table samples from death rates.
#' @return A list of life table draws, calculated using posterior samples of death rates.
#' @export
maple_sample <- function(model.fit, num.draws, ax, ...) {
    UseMethod("maple_sample")
}

maple_sample.inla <- function(model.fit, num.draws, ax = NULL, ...) {
    log.rate.draws <- inla.posterior.sample(num.draws, model.fit)
    rate.draws <- lapply(log.rate.draws, function(draw) {
                    rates <- exp(draw$latent[grep("Predictor", rownames(draw$latent))])
                    rates <- rates[order(model.fit$.args$data$year, model.fit$.args$data$age)]
                    rates.m <- matrix(rates, ncol = length(unique(model.fit$.args$data$year)))
                    rownames(rates.m) <- unique(model.fit$.args$data$age)
                    colnames(rates.m) <- unique(model.fit$.args$data$year)
                    rates.m
                  })
    l <- lapply(rate.draws, maple_plt, ax = ax, full.table = FALSE)
    class(l) <- c(class(l), "inla.samples")
    l
}

maple_sample.lc <- function(model.fit, num.draws, ax = NULL, ...) {
	forecast.horizon <- model.fit$forecast.horizon
    log.rate.draws <- replicate(num.draws,
                       model.fit$alphas + Reduce(`+`, lapply(seq(model.fit$num.pcs),
                           function(i) outer(model.fit$betas[i, ],
                                             model.fit$gammas.pred[[i]]$pred +
                                         cumsum(rnorm(forecast.horizon,
                                                      sd = model.fit$gammas.fit[[i]]$sigma2))))),
                       simplify = FALSE)
    in.sample.fit <- model.fit$rate[, seq_len(ncol(model.fit$gammas))]
    rate.draws <- lapply(log.rate.draws, function(x) {
                        m <- cbind(in.sample.fit, exp(x))
                        colnames(m) <- colnames(model.fit$logrates)
                        m
                    })
    l <- lapply(rate.draws, maple_plt, ax = ax, full.table = FALSE)
    class(l) <- c(class(l), "lc.samples")
    l
}
