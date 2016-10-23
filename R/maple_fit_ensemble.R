#' @export
maple_fit_ensemble <- function(deaths, population, forecast.horizon, models = maple_models(),
                          num.draws = 1000, ax = NULL, num.threads = parallel::detectCores(), 
                          verbose = TRUE) {

    model.fits <- lapply(seq_along(models), function(m) {
        if (verbose) message("Fitting model ", models[[m]]$name, "...")
        tryCatch(
            maple_fit_model(model = models[[m]], 
                            deaths = deaths,
                            population = population,
                            forecast.horizon = forecast.horizon, 
                            num.threads = num.threads),
            error = function(e) {message("Error:", e$message); return(e)}
        )
    })
    names(model.fits) <- names(models)
    
    fitted.values <- lapply(seq_along(model.fits), function(m) {
            model.fit <- model.fits[[m]]
            rates <- fitted_rates_matrix(model.fit)
            d <- data.frame(model = names(model.fits)[m], year = as.numeric(colnames(rates)))
            d[, paste0("rate_", seq(0, 85, 5))] <- t(rates)
            plt <- maple_plt(rates, ax, full.table = FALSE)
            d$e0 <- plt_ex(plt, 0)
            d$e65 <- plt_ex(plt, 65)
            d$q70 <- plt_qx(plt, 70)
            d
        })
    fitted.values <- do.call(rbind, fitted.values)

    samples <- summaries <- NULL
    if (num.draws > 0) {
        samples <- lapply(seq_along(models), function(m) {
            if (verbose) message("Sampling draws for model ", models[[m]]$name, "...")
            tryCatch(
                maple_sample(model.fit = model.fits[[m]], 
                             num.draws = num.draws, 
                             forecast.horizon = forecast.horizon, 
                             ax = ax),
                error = function(e) {message("Error:", e$message); return(e)}
            )
        })
        names(samples) <- names(models)
        
        sample.summaries <- lapply(seq_along(models), function(m) {
            if (verbose) message("Calculating summary statistics for model ", models[[m]]$name, "...")
            tryCatch(
                data.frame(model = names(models)[m], maple_sample_summaries(samples[[m]])),
                error = function(e) {message("Error:", e$message); return(e)}
            )
        })
        sample.summaries <- do.call(rbind, sample.summaries)
    }
    list(
        fitted.values = fitted.values, 
        sample.summaries = sample.summaries,
        samples = samples,
        model.fits = model.fits
    )
}
