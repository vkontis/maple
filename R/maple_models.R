#' Generate a list of forecasting models.
#' @param fixed.prec The precision on the fixed effects (common intercept and slope) normal priors.
#' @param gamma.shape The shape parameter for the loggamma prior on the random effects. See ?INLA::inla.models for more details.
#' @param gamma.rate The rate (inverse scale) parameter for the loggamma prior on the random effects. See ?INLA::inla.models for more details.
#' @return A list of forecasting models. Each model is a list with the following entries
#' \describe{
#'   \item{name}{A short name to identify the model.}
#'   \item{desc}{A description of the model.}
#'   \item{fml}{(only for models fitted in INLA) The formula passed to INLA when running the model.}
#'   \item{likelihood.weight.rate}{(only for weighted likelihood models) The rate used when calculating the likelihood weights. See ?calc_likelihood_weights for more details.}
#'   \item{num.pcs}{(only for Lee-Carter models) The number of principal components.}
#' }
#' @examples
#' maple_models()[c(1, 20)]
#' maple_models(fixed.prec = 1e-5, gamma.shape = .1, gamma.rate = .1)
#' @export
maple_models <- function(fixed.prec = 0.001,
                        gamma.shape = 1,
                        gamma.rate = 1e-3) {

    hpr <- paste0('list(prec = list(prior = "loggamma", param = c(', 
                  gamma.shape, ', ', gamma.rate, ')))')

    models <- list(
        list(
            name = "IIDAGE",
            desc = "Age-period model with iid age intercepts and slopes.",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc1, model = "iid", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "RW1AGE",
            desc = "Age-period model with first order random walk age intercepts and slopes.",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc1, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "IIDAGE_RW1COH",
            desc = "Age-period-cohort model with iid age intercepts and slopes and first order random walk cohort slopes.",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc1, model = "iid", hyper = hpr) +
            f(cohort.id1, yearc1, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "RW1AGE_RW1COH",
            desc = "Age-period-cohort model with first order random walk age intercepts and slopes and first order random walk cohort slopes.",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc, model = "rw1", hyper = hpr) +
            f(cohort.id1, yearc, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_IIDAGE",
            desc = "Age-period model with iid age intercepts and slopes, using piecewise linear slopes with knot 10 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "iid", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_RW1AGE",
            desc = "Age-period model with first order random walk age intercepts and slopes, using piecewise linear slopes with knot 10 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_IIDAGE",
            desc = "Age-period model with iid age intercepts and slopes, using piecewise linear slopes with knot 6 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "iid", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_RW1AGE",
            desc = "Age-period model with first order random walk age intercepts and slopes, using piecewise linear slopes with knot 6 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_IIDAGE_RW1COH",
            desc = "Age-period-cohort model with iid age intercepts and slopes and first order random walk cohort slopes, using piecewise linear slopes with knot 10 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "iid", hyper = hpr) +
            f(cohort.id1, yearc3.10a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.10b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_RW1AGE_RW1COH",
            desc = "Age-period-cohort model with first order random walk age intercepts and slopes and first order random walk cohort slopes, using piecewise linear slopes with knot 10 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "rw1", hyper = hpr) +
            f(cohort.id1, yearc3.10a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.10b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_IIDAGE_RW1COH",
            desc = "Age-period-cohort model with iid age intercepts and slopes and first order random walk cohort slopes, using piecewise linear slopes with knot 6 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "iid", hyper = hpr) +
            f(cohort.id1, yearc3.6a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.6b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_RW1AGE_RW1COH",
            desc = "Age-period-cohort model with first order random walk age intercepts and slopes and first order random walk cohort slopes, using piecewise linear slopes with knot 6 years before the end of data.",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "rw1", hyper = hpr) +
            f(cohort.id1, yearc3.6a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.6b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        )
        )

    models <- lapply(models, function(m) {
        class(m) <- "inla.model"
        m$fml <- gsub("hpr", hpr, m$fml)
        m$fml <- gsub("fixed.prec", fixed.prec, m$fml)
        m$fml <- as.formula(m$fml)
        m
    })
    wl.models <- Filter(
        function(x) x$name %in% c("IIDAGE", "RW1AGE", "IIDAGE_RW1COH", "RW1AGE_RW1COH"),
        models)
    wl.models <- lapply(wl.models, function(x) {
        x$name <- paste0("WL0.05_", x$name)
        x$likelihood.weight.rate <- 0.05
        x$desc <- paste0("Weighted likelihood ", tolower(substr(x$desc, 1, 1)), substr(x$desc, 2, nchar(x$desc)))
        x
    })

    lc.models <- lapply(seq(5), function(n) {
        m <- list(
            name = paste0("LC_", n, "PC"),
            desc = paste0("Lee-Carter model with ", n, " principal component",
                    ifelse(n > 1, "s", ""), "."),
            num.pcs = n
        )
        class(m) <- "lc.model"
        m
    })
    l <- c(models, wl.models, lc.models)
    setNames(l, sapply(l, `[[`, "name"))
}
