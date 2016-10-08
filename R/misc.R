inla_fitted_matrix <- function(model.fit, variable) {
    v <- model.fit$summary.fitted.values[[variable]]
    v <- v[order(model.fit$.args$data$year, model.fit$.args$data$age)]
    m <- matrix(v, nrow = length(unique(model.fit$.args$data$age)), 
                ncol = length(unique(model.fit$.args$data$year)))
    colnames(m) <- unique(model.fit$.args$data$year)
    rownames(m) <- unique(model.fit$.args$data$age)
    m
}

fitted_rates_matrix <- function(model.fit) UseMethod("fitted_rates_matrix")

fitted_rates_matrix.inla <- function(model.fit) inla_fitted_matrix(model.fit, "mean")

fitted_rates_matrix.lc <- function(model.fit) model.fit$rates

calculate_row_summaries <- function(m) {
    qs <- apply(m, 1, quantile, probs = c(0.025, 0.5, 0.975))
    data.frame(
        mean = rowMeans(m), 
        median = qs[2, ],
        sd = apply(m, 1, sd),
        lb = qs[1, ],
        ub = qs[3, ]
    )
}

plt_ex <- function(plt, x = 0) {
    if (!all(c("age", "year", "ex") %in% names(plt))) {
        stop("Life table must include 'age', 'year', 'ex' columns.")
    }
    setNames(plt[plt$age == x, ]$ex, plt[plt$age == x, ]$year)
}
