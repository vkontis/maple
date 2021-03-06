#' Given a matrix, calculate row summaries
#' @param m Numeric matrix
#' @return Data frame containing row means, medians, standard deviations, 2.5th and 97.5th percentiles
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


