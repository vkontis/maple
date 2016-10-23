#' @export
maple_sample_summaries <- function(samples) {

    rate.samples <- do.call(cbind, lapply(samples$death.rates, as.vector))
    rate.summaries <- data.frame(
        year = rep(as.numeric(colnames(samples$death.rates[[1]])),
                   each = nrow(samples$death.rates[[1]])), 
        metric = paste0("rate_", seq(0, 85, 5)), 
        calculate_row_summaries(rate.samples))
    
    e0.samples <- do.call(cbind, lapply(samples$plts, plt_ex, x = 0))
    e0.summaries <- data.frame(
        year = unique(samples$plts[[1]]$year),
        metric = "e0", 
        calculate_row_summaries(e0.samples))
    
    e65.samples <- do.call(cbind, lapply(samples$plts, plt_ex, x = 0))
    e65.summaries <- data.frame(
        year = unique(samples$plts[[1]]$year),
        metric = "e65", 
        calculate_row_summaries(e65.samples))
    
    q70.samples <- do.call(cbind, lapply(samples$plts, plt_qx, x = 70))
    q70.summaries <- data.frame(
        year = unique(samples$plts[[1]]$year),
        metric = "q70", 
        calculate_row_summaries(q70.samples))
        
    rbind(rate.summaries, e0.summaries, e65.summaries, q70.summaries)
}

