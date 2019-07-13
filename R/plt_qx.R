#' Extract the probability of dying by a given age from a life table.
#' @param plt A life table calculated by maple_plt().
#' @param x The age group for which to calculate probability of dying by. 
#' @return A named vector containing the probability of dying between 0 and age x in each year.
#' @export
plt_qx <- function(plt, x = 70) { 
    if (!all(c("age", "year", "qx") %in% names(plt))) {
        stop("Life table must include 'age', 'year', 'qx' columns.")
    }
    
    calculate_pod <- function(m) {
        # m matrix of age x year qx
        1 - Reduce(`*`, as.data.frame(1 - t(m)))
    }
    
    plt1 <- plt[plt$age < x, ]
    qx <- plt1$qx[order(plt1$year, plt1$age)]
    m <- matrix(qx, ncol = length(unique(plt1$year)))
    setNames(calculate_pod(m), plt[plt$age == x, ]$year)
}
