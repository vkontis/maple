#' Produce model averaged projections of death rates and life expectancy.
#' @param deaths A matrix of death counts, with 18 rows, one for each 5-year age group 0-4, ..., 80-84, 85+ and one column for each year of available data. The column names of the matrix must be the years of data.
#' @param population A matrix of mid-year population numbers, in the same row/column format as deaths.
#' @param forecast.horizon The number of years to produce projections for.
#' @param holdout The number of years of data to hold out to calculate model weights.
#' @param models The individual models to be run and averaged; see ?maple_models for more details.
#' @param num.draws The number of posterior samples from individual model fits to use for producing the BMA estimates.
#' @param ax The number of years lived on average by those who die in their current age group. See ?maple_plt for more details.
#' @param num.threads The number of threads to use when running the models. This is passed to the INLA methods. If not specified, then all available threads are used.
#' @param verbose If TRUE (the default), print some information on progress fitting models, etc.
#' @return A list with the following entries
#' \describe{
#'   \item{model.weights}{The weights used to combine models into the model average.}
#'   \item{sample.summaries}{A data frame holding statistical summary information for age-specific death rates, life expectancy and probability of dying, calculated from the posterior draws.}
#'   \item{samples}{A list of life table draws, calculated using posterior samples of death rates.}
#'   \item{individual.model.sample.summaries}{A data frame containing predictions under individual models.}
#' }
#' @note The maximum possible number of BMA samples is taken, depending on the model weights. For example if num.draws == 1000 and there are 5 models with weights 0.25, 0.2, 0.2, 0.2, and 0.15, the code will try to use all draws from the first model (with largest weight) and a number of draws from the remaining models inversely proportional to their weights (800, 800, 800, 600).
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
        message("ax values matrix row names are missing; assuming they match age groups 0-4, 5-9, ..., 80-84, 85+.")
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

    model.draws <- floor(model.weights * floor(min(num.draws / model.weights)))

    draw.idx <- lapply(seq_along(model.draws), function(i) {
        sample(num.draws, model.draws[i], replace = FALSE)
    })

    bma.samples <- unlist(
        lapply(seq_along(forecast.run.fits$samples),
               function(i) forecast.run.fits$samples[[i]][draw.idx[[i]]]),
        recursive = FALSE)

    bma.sample.summaries <- maple_sample_summaries(bma.samples)

    bma <- structure(list(model.weights = model.weights,
                          sample.summaries = bma.sample.summaries,
                          samples = bma.samples,
                          individual.model.sample.summaries = forecast.run.fits$sample.summaries),
                     class = "maple.bma")
    if (verbose) message("Done.")
    bma
}
