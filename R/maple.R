#' Produce model averaged projections of death rates and life expectancy.
#' @param deaths A matrix of death counts, with 18 rows, one for each 5-year age group 0-4, ..., 80-84, 85+ and one column for each year of available data. The column names of the matrix must be the years of data.
#' @param population A matrix of mid-year population numbers, in the same row/column format as deaths.
#' @param forecast.horizon The number of years to produce projections for.
#' @param holdout The number of years of data to hold out to calculate model weights.
#' @param models The individual models to be run and averaged; see ?maple_models for more details.
#' @param num.draws The number of posterior draws to sample and use for calculating statistical summaries.
#' @param ax The number of years lived on average by those who die in their current age group. See ?maple_plt for more details. 
#' @param num.threads The number of threads to use when running the models. This is passed to the INLA methods. If not specified, then all available threads are used.
#' @param verbose If TRUE (the default), print some information on progress fitting models, etc.
#' @return A list with the following entries
#' \describe{
#'   \item{model.weights:}{The weights used to combine models into the model average.}
#'   \item{sample.summaries:}{A data frame with holding statistical summary information for age-specific death rates, life expectancy at birth and age 65, and probability of dying before age 70, calculated from the posterior draws.}
#'   \item{samples:}{List containing posterior draws for death rates and life tables.}
#' }
#' @examples
#' data(maple.deaths)
#' data(maple.population)
#' data(maple.ax)
#' models <- maple_models()[c(1, 20)]
#' bma <- maple(deaths = maple.deaths, population = maple.population, forecast.horizon = 20, holdout = 13, models = models, ax = maple.ax)
#' @export
maple <- function(deaths, population, forecast.horizon, holdout, models = maple_models(),
                  num.draws = 1000, ax = NULL, num.threads = inla.getOption("num.threads"),
                  verbose = TRUE) {

    check_maple_data_format(deaths, population, ax)

    if (is.null(rownames(deaths))) {
        message("Death rates matrix row names are missing; assuming they match age groups 0-4, 5-9, ..., 80-84, 85+.")
        rownames(deaths) <- seq(0, 85, 5)
    }
    if (is.null(rownames(population))) {
        message("Population matrix row names are missing; assuming they match age groups 0-4, 5-9, ..., 80-84, 85+.")
        rownames(population) <- seq(0, 85, 5)
    }
    if (!is.null(ax) && is.null(rownames(ax))) {
        message("5ax values matrix row names are missing; assuming they match age groups 0-4, 5-9, ..., 80-84, 85+.")
        rownames(ax) <- seq(0, 85, 5)
    }

    if (holdout < 13) warning("Holdout period too short, some models may fail to run.")

    holdout.cols <- ncol(deaths) + seq(-holdout + 1, 0)

    if (verbose) message("Fitting models for model weight calculation...")
    weight.run.fits <- maple_fit_ensemble(
                            deaths = deaths[, -holdout.cols],
                            population = population[, -holdout.cols],
                            forecast.horizon = holdout,
                            models = models,
                            num.draws = num.draws,
                            ax = ax[, -holdout.cols],
                            num.threads = num.threads,
                            verbose = verbose)
    if (verbose) message("Forecasting with individual  models...")
    forecast.run.fits <- maple_fit_ensemble(
                            deaths = deaths,
                            population = population,
                            forecast.horizon = forecast.horizon,
                            models = models,
                            num.draws = num.draws,
                            ax = ax,
                            num.threads = num.threads,
                            verbose = verbose)

    if (verbose) message("Calculating projection errors...")
    projection.errors <- sapply(weight.run.fits$model.fits, function(m) {
        maple_projection_error(
            deaths = deaths[, holdout.cols],
            population = population[, holdout.cols],
            ax = ax[, holdout.cols],
            fitted.rates = fitted_rates_matrix(m)[, holdout.cols]
        )
    })
    if (verbose) message("Computing model average...")
    model.weights <- maple_model_weights(projection.errors)

    # Rates
    fitted.values.list <- split(
        forecast.run.fits$fitted.values[-grep("model|year|age", names(forecast.run.fits$fitted.values))],
        forecast.run.fits$fitted.values$model
    )
    stopifnot(names(fitted.values.list) == names(model.weights))

    bma.fitted.values <- Reduce(`+`, Map(`*`, fitted.values.list, model.weights))
    bma.fitted.values <- data.frame(subset(forecast.run.fits$fitted.values,
                                           model == models[[1]]$name,
                                           select = c("year", "age")),
                                    bma.fitted.values)

    if (sum(round(model.weights * num.draws)) == num.draws) {
        model.draws <- round(model.weights * num.draws)
    } else {
        model.draws <- floor(model.weights * num.draws)

        if (sum(model.draws) < num.draws) {
            rem.draws <- sample(seq_along(model.weights), size = num.draws - sum(model.draws),
                                replace = TRUE, prob = model.weights)
            for (j in rem.draws) model.draws[j] <- model.draws[j] + 1
        }
    }
    draw.idx <- lapply(seq_along(model.draws), function(i) {
        sample(num.draws, model.draws[i], replace = FALSE)
    })
    bma.samples <- unlist(
        lapply(seq_along(forecast.run.fits$samples),
               function(i) forecast.run.fits$samples[[i]][draw.idx[[i]]]),
        recursive = FALSE)

    bma.sample.summaries <- maple_sample_summaries(bma.samples)

    bma <- structure(list(model.weights = model.weights,
                          fitted.values = bma.fitted.values,
                          sample.summaries = bma.sample.summaries,
                          samples = bma.samples),
                     class = "maple.bma")
    if (verbose) message("Done.")
    bma
}

