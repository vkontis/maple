#' Given a list of posterior samples of death rates and life tables, generate summaries for death rates, life expectancy at birth and at age 65, and probability of dying before age 70.
#' @param samples Posterior sample draws produced with maple_sample().
#' @return A data frame containing the mean, median, sd and 95% credible interval, for the following metrics
#' \describe{
#'   \item{rate_0, ..., rate_85:}{Age-specific death rates for age groups 0-4, ..., 80-84 and 85+.}
#'   \item{e0:}{Life expectancy at birth.}
#'   \item{e65:}{Life expectancy at age 65.}
#'   \item{q70:}{Probability of dying before age 70.}
#' }
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

