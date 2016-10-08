maple_sample_summaries <- function(samples) {

    rate.samples <- do.call(cbind, lapply(samples$death.rates, as.vector))
    rate.summaries <- calculate_row_summaries(rate.samples)
    
    e0.samples <- do.call(cbind, lapply(samples$plts, function(plt) plt[plt$age == 0, ]$ex))
    e0.summaries <- calculate_row_summaries(e0.samples)
    
    e65.samples <- do.call(cbind, lapply(samples$plts, function(plt) plt[plt$age == 65, ]$ex))
    e65.summaries <- calculate_row_summaries(e65.samples)
    
    q70.samples <- do.call(cbind, lapply(samples$plts, function(plt) {
                        plt1 <- plt[plt$age <= 65, ]
                        qx <- plt1$qx[order(plt1$year, plt1$age)]
                        m <- matrix(qx, ncol = length(unique(plt1$year)))
                        calculate_pod(m)
                    }))
    q70.summaries <- calculate_row_summaries(q70.samples)

    list(death.rates = 
             data.frame(
                year = rep(colnames(samples$death.rates[[1]]), each = nrow(samples$death.rates[[1]])),
                age = rep(rownames(samples$death.rates[[1]]), ncol(samples$death.rates[[1]])),
                rate.summaries
                ), 
          e0 = data.frame(
                year = unique(samples$plts[[1]]$year),
                e0.summaries),
          e65 = data.frame(
                year = unique(samples$plts[[1]]$year),
                e65.summaries),
          q70 = data.frame(
                year = unique(samples$plts[[1]]$year),
                q70.summaries)
    )
}

