#' @export
maple_fit_model <- function(model, deaths, population, forecast.horizon, num.threads, ...) {
    UseMethod("maple_fit_model")
}

maple_fit_model.inla.model <- function(model, deaths, population, forecast.horizon,
                                       num.threads = parallel::detectCores(), ...) {
    require(INLA)
    inla.dat <- prep_inla_dat(deaths = deaths, 
                              population = population,
                              model = model,
                              forecast.horizon = forecast.horizon)
    lhd.wght <- NULL
    if (!is.null(model$likelihood.weight.rate)) {
        inla.setOption(enable.inla.argument.weights = TRUE)
        lhd.wght <- calc_likelihood_weights(inla.dat$year, model$likelihood.weight.rate)
    }
    fit <- inla(model$fml,
                family = "poisson",
                data = inla.dat,
                E = population,
                control.predictor = list(link = 1),
                control.compute = list(dic = TRUE, config = TRUE),
                control.inla = list(strategy = "laplace", npoints = 50,
                                    numint.maxfeval = 8e7),
                weights = lhd.wght,
                num.threads = num.threads
            )
    if (!is.null(model$likelihood.weight.rate)) {
        inla.setOption(enable.inla.argument.weights = FALSE)
    }
    fit
}

maple_fit_model.lc.model <- function(model, deaths, population, forecast.horizon, ...) {
    ages <- as.numeric(rownames(deaths))
    years <- as.numeric(colnames(deaths))
    forecast.years <- ncol(deaths) + seq(forecast.horizon)
    
    rates <- deaths / population
    
    if (any(is.na(rates))) stop("Cannot run Lee-Carter model beacause of NAs in data.")
    
    logrates <- log(rates)
    alphas <- rowMeans(logrates)
    clogrates <- logrates - alphas
    clogrates.svd <- svd(clogrates)
    betas <- t(clogrates.svd$u[, seq_len(model$num.pcs)])
    gammas <- betas %*% clogrates
    # Normalise so that each beta sums to 1
    gammas <- gammas * rowSums(betas)
    betas <- betas / rowSums(betas)
    gammas.fit <- apply(gammas, 1, arima, order = c(0, 1, 0), xreg = seq_len(ncol(gammas)))
    # In-sample fits
    logrates.fit <- alphas + Reduce(`+`, lapply(seq_len(nrow(gammas)),
                                                function(i) outer(betas[i, ], gammas[i, ])))
    # Forecasts
    gammas.pred <- lapply(gammas.fit, predict,
                          newxreg = ncol(gammas) + seq(forecast.horizon),
                          n.ahead = forecast.horizon)
    gammas.pred.mean <- lapply(gammas.pred, function(x) as.numeric(x$pred))
    gammas.pred.se <- lapply(gammas.pred, function(x) as.numeric(x$se))
    logrates.pred <- alphas + Reduce(`+`, lapply(seq_along(gammas.pred.mean),
                                                 function(i) outer(betas[i, ], gammas.pred.mean[[i]])))
    
    logfit <- cbind(logrates.fit, logrates.pred)
    rownames(logfit) <- ages
    colnames(logfit) <- c(years, years[length(years)] + seq(forecast.horizon))
    fit <- exp(logfit)
    
    l <- list(
        logrates = logfit,
        rates = fit,
        alphas = alphas,
        betas = betas,
        gammas = gammas,
        gammas.fit = gammas.fit,
        gammas.pred = gammas.pred,
        pct.var = sum(clogrates.svd$d[seq_len(model$num.pcs)] ^ 2 / sum(clogrates.svd$d ^ 2)),
        num.pcs = model$num.pcs
    )
    class(l) <- c(class(l), "lc")
    l
}
    