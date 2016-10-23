#' @export
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

#' @export
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
        if (!identical(rownames(m), as.character(seq(0, 85, 5)))) {
            stop("Row names of data matrices must be set to age groups 0, 5,..., 80, 85+.")
        }
        if (!all(as.integer(colnames(m)) == colnames(m))) {
            stop("Column names of data matrices data must be set to the years of data.")
        }
    }
}



