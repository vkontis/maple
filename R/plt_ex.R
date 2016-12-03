#' Extract the life expectancy values for a given age from a life table.
#' @param plt A life table calculated by maple_plt().
#' @param x The age group for which to extract life expectancy values.
#' @return A named vector containing life expectancy values for each year.
#' @export
plt_ex <- function(plt, x = 0) {
    if (!all(c("age", "year", "ex") %in% names(plt))) {
        stop("Life table must include 'age', 'year', 'ex' columns.")
    }
    setNames(plt[plt$age == x, ]$ex, plt[plt$age == x, ]$year)
}
