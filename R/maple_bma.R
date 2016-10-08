maple_bma <- function(weight.run.fits, forecast.run.fits, num.draws = 1000, ax = NULL) {
    
    holdout <- weight.run.fits$forecast.horizon
    holdout.cols <- ncol(forecast.run.fits$deaths) + seq(-holdout + 1, 0)
    
    projection.errors <- sapply(weight.run.fits$fitted.rates, function(m) {
        maple_projection_error(
            deaths = forecast.run.fits$deaths[, holdout.cols],
            population = forecast.run.fits$population[, holdout.cols],
            ax = ax,
            fitted.rates = m[, holdout.cols]
        )
    })
    
    model.weights <- maple_model_weights(projection.errors)
    
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
        total.draws <- length(forecast.run.fits$samples[[i]][[1]])
        stopifnot(total.draws >=  model.draws[i])
        sample(total.draws, model.draws[i], replace = FALSE)
    })
    
    bma.fitted.rates <- Reduce(`+`, Map(`*`, forecast.run.fits$fitted.rates, model.weights))
        
    bma.samples <- list(death.rates = NULL, plts = NULL) 
    
    bma.samples$death.rates <- unlist(
        lapply(seq_along(forecast.run.fits$samples),
            function(i) forecast.run.fits$samples[[i]]$death.rates[draw.idx[[i]]]), 
        recursive = FALSE)
    bma.samples$plts <- unlist(
        lapply(seq_along(forecast.run.fits$samples),
               function(i) forecast.run.fits$samples[[i]]$plts[draw.idx[[i]]]), 
        recursive = FALSE)
    
    bma.sample.summaries <- maple_sample_summaries(bma.samples)
    
    l <- list(
        deaths = forecast.run.fits$deaths,
        population = forecast.run.fits$population, 
        forecast.horizon = forecast.run.fits$forecast.horizon,
        models = forecast.run.fits$models,
        model.weights = model.weights,
        model.draws = model.draws,
        fitted.rates = bma.fitted.rates,
        samples = bma.samples, 
        sample.summaries = bma.sample.summaries, 
        num.draws = num.draws
    )
    class(l) <- c(class(l), "maple_bma")
    l
}