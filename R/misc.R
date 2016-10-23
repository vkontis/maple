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

plt_qx <- function(plt, x = 70) { 
    if (!all(c("age", "year", "ex") %in% names(plt))) {
        stop("Life table must include 'age', 'year', 'ex' columns.")
    }
    plt1 <- plt[plt$age < x, ]
    qx <- plt1$qx[order(plt1$year, plt1$age)]
    m <- matrix(qx, ncol = length(unique(plt1$year)))
    setNames(calculate_pod(m), plt[plt$age == x, ]$year)
}

check_maple_data_format <- function(...) {
    l <- list(...)
    l <- Filter(Negate(is.null), l)
    if (!all(sapply(l, is.matrix))) {
        stop("Data must be provided in matrix form.")
    }
    
    if (!all(sapply(l, function(x) dim(x) == dim(l[[1]])))) {
        stop("Deaths, population and 5ax values must have the same dimensions.")
    }
    
    for (m in l) {
        if (nrow(m) != 18) {
            stop("Data matrices must consist of 18 rows, corresponding to age groups 0, 5,..., 80, 85+.")
        }
        if (!is.null(rownames(m))  && !identical(rownames(m), as.character(seq(0, 85, 5)))) {
            stop("Row names of data matrices must be set to age groups 0, 5,..., 80, 85+.")
        }
        if (is.null(colnames(m)) || !all(as.integer(colnames(m)) == colnames(m))) {
            stop("Column names of data matrices data must be set to the years of data.")
        }
    }
}



