#' Check arguments are in the desired format: 18xN matrices with column names corresponding to years
#' @param ... Objects to be checked
check_maple_data_format <- function(...) {
    l <- list(...)
    l <- Filter(Negate(is.null), l)
    if (!all(sapply(l, is.matrix))) {
        stop("Data must be provided in matrix form.")
    }
    if (!all(sapply(l, function(x) dim(x) == dim(l[[1]])))) {
        stop("Deaths, population and ax values must have the same dimensions.")
    }
    for (m in l) {
        if (nrow(m) != 18) {
            stop("Data matrices must consist of 18 rows, corresponding to age groups 0-4, 5-9,..., 80-84 and 85+.")
        }
        if (!all(as.integer(colnames(m)) == colnames(m))) {
            stop("Column names of data matrices data must be set to the years of data.")
        }
        if (!all(colnames(m) == sort(as.integer(colnames(m))))) {
            stop("Columns of data matrices must be sorted in increasing order of years.")
        }
    }
}

