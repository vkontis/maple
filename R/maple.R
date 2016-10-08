maple <- function(deaths, population, forecast.horizon, holdout, models = maple_models(),
                  num.draws = 1000, ax = NULL, num.threads = parallel::detectCores(), 
                  verbose = TRUE) {
    
    # TODO check holdout large enough to fit models
    
    holdout.cols <- ncol(deaths) + seq(-holdout + 1, 0)
    
    if (verbose) 
        cat("Fitting models for model weight calculation...\n")
    weight.run.fits <- maple_fit_ensemble(
                        deaths = deaths[, -holdout.cols],
                        population = population[, -holdout.cols],
                        forecast.horizon = holdout, 
                        models = models, 
                        num.draws = num.draws, 
                        ax = ax, 
                        num.threads = num.threads, 
                        verbose = verbose
                    )
    if (verbose) 
        cat("Forecasting with individual  models...\n")
    forecast.run.fits <- maple_fit_ensemble(
        deaths = deaths,
        population = population,
        forecast.horizon = forecast.horizon, 
        models = models, 
        num.draws = num.draws, 
        ax = ax, 
        num.threads = num.threads, 
        verbose = verbose
    )
    if (verbose) 
        cat("Computing model average...\n")
    bma <- maple_bma(weight.run.fits, forecast.run.fits, num.draws = num.draws, ax = ax)
    
    bma
}

