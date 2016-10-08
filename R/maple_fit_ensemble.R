maple_fit_ensemble <- function(deaths, population, forecast.horizon, models = maple_models(),
                          num.draws = 1000, ax = NULL, num.threads = parallel::detectCores(), 
                          verbose = TRUE) {

    # TODO add error handling for rownames deaths rownames pop etc
    # check they are sorted check dim names actually exist
    # force ages to be 0, .. , 85
    model.fits <- lapply(seq_along(models), function(m) {
        if (verbose) {
            cat("Fitting model ", models[[m]]$name, "...\n")
        }
        tryCatch(
            maple_fit_model(model = models[[m]], 
                            deaths = deaths,
                            population = population,
                            forecast.horizon = forecast.horizon, 
                            num.threads = num.threads),
            error = function(e) {cat("Error:", e$message, "\n"); return(e)}
        )
    })
    
    fitted.rates <- lapply(seq_along(models), function(i) fitted_rates_matrix(model.fits[[i]]))
    names(model.fits) <- names(fitted.rates) <- names(models)
        
    samples <- summaries <- NULL
    if (num.draws > 0) {
        samples <- lapply(seq_along(models), function(m) {
            if (verbose) {
                cat("Sampling draws for model ", models[[m]]$name, "...\n")
            }
            tryCatch(
                maple_sample(model.fit = model.fits[[m]], 
                             num.draws = num.draws, 
                             forecast.horizon = forecast.horizon, 
                             ax = ax),
                error = function(e) {cat("Error:", e$message, "\n"); return(e)}
            )
        })
        sample.summaries <- lapply(seq_along(models), function(m) {
            if (verbose) {
                cat("Calculating summary statistics for model ", models[[m]]$name, "...\n")
            }
            tryCatch(
                maple_sample_summaries(samples[[m]]),
                error = function(e) {cat("Error:", e$message, "\n"); return(e)}
            )
        })
        names(samples) <- names(sample.summaries) <- names(models)
    }
    list(
        deaths = deaths, 
        population = population, 
        forecast.horizon = forecast.horizon,
        models = models,
        fitted.rates = fitted.rates, 
        model.fits = model.fits,
        samples = samples,
        sample.summaries = sample.summaries,
        num.draws = num.draws
    )
}
