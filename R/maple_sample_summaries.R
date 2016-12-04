#' Calculate summaries for death rates, life expectancy and probability of dying.
#' @param samples Posterior sample draws produced with maple_sample().
#' @return A data frame containing the mean, median, sd, 2.5th and 97.5th percentiles of age-specific death rate, life expectancy and probability of dying in each year.
#' @export
maple_sample_summaries <- function(samples) {
    rate.samples <- do.call(cbind, lapply(samples, `[[`, "mx"))
    ex.samples <- do.call(cbind, lapply(samples, `[[`, "ex"))
    qx.samples <- do.call(cbind, lapply(samples, `[[`, "qx"))
    
    rbind(data.frame(samples[[1]][c("year", "age")], 
                     metric = "rate", 
                     calculate_row_summaries(rate.samples)),
          data.frame(samples[[1]][c("year", "age")], 
                     metric = "ex", 
                     calculate_row_summaries(ex.samples)),
          data.frame(samples[[1]][c("year", "age")], 
                     metric = "qx", 
                     calculate_row_summaries(qx.samples)))
}

