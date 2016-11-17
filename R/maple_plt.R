#' Generate a period life table for the supplied inputs. The ax for the open-ended age group 85+ is calculated using the Kannisto-Thatcher method [Thatcher et al, The survivor ratio method for estimating numbers at high ages, Demographic Research (6), 2002]. For age groups 5-9 to 80-84 the ax are calibrated using an iterative procedure described on p.47 of [Preston et al, Demography: measuring and modeling population processes, 2001].
#' @export
#' @param death.rates A matrix of death rates, with 18 rows, one for each 5-year age group 0-4, ..., 80-84, 85+.
#' @param ax The number of years lived on average by those who die in their current age group. For example, if all deaths in the age group 60-64 happened exactly at the middle of the age group, this would be equal to 2.5.
#' @param check.conv If TRUE, it will test that the iterative procedure to estimate ax (see above) has converged.
#' @param full.table If TRUE, returns all the columns of the period life table (see below).
#' @return A period life table with death rates, life expectancies and probability of dying.
#' @examples
#' data(maple.deaths)
#' data(maple.population)
#' data(maple.ax)
#' plt <- maple_plt(maple.deaths / maple.population, maple.ax)
maple_plt <- function(death.rates, ax = NULL, check.conv = FALSE, full.table = FALSE) {
    year <- rep(as.numeric(colnames(death.rates)), each = nrow(death.rates))
    age <- rep(as.numeric(rownames(death.rates)), ncol(death.rates))
    mx <- as.vector(death.rates)
    
    if (is.null(ax)) {
        ax <- ifelse(age == 0, .5, 2.5)
    } else if (ncol(ax) < ncol(death.rates)) {
        ax <- ax[, c(seq(1, ncol(ax)), rep(ncol(ax), ncol(death.rates) - ncol(ax)))]
    }
    ax <- as.vector(ax)

    # Remove NA rates (this is useful when eg a given year has no mortality data;
    # the removed entries are re-inserted as NAs at the end of the function call)
    na.idx <- NULL
    if (any(is.na(mx))) {
        na.idx <- which(is.na(mx))
        stopifnot(age[na.idx] == seq(0, 85, 5))
        age0 <- age
        ax0 <- ax
        mx0 <- mx
        age <- age[-na.idx]
        mx <- mx[-na.idx]
        ax <- ax[-na.idx]
    }

    # Replace zero rates by a small number for numerical reasons
    mx[mx == 0] <- 1e-10
    # Probability of dying between age x and x+4
    qx <- 5 * mx / (1 + (5 - ax) * mx)
    # If probability of dying is >1, set it to "almost" 1
    qx[qx > 1] <- 0.99999
    qx[age == 85] <- 1 # by definition
    px <- 1 - qx # probability of surviving to next age group
    lx <- rep(NA, length(px))
    lx[age == 0] <- 100000
    for (k in seq(5, 85, 5))
        lx[age == k] <- lx[age == k - 5] * px[age == k - 5]
    dx <- lx * qx
    ax[age >= 70] <- kt_extension(matrix(lx[age >= 70], nrow = 4))

    num.iter <- 4 # Number of iterations - see Preston et al. p.47
    iter.dat <- vector("list", num.iter + 1)
    iter.dat[[1]] <- list(ax = ax, qx = qx, lx = lx, dx = dx)
    for (i in seq(num.iter)) {
        ax.new <- ax
        for (k in seq(5, 80, 5))
            ax.new[age == k] <- (-5 / 24 * dx[age == k - 5] +
                                     2.5 * dx[age == k] + 5 / 24 * dx[age == k + 5]) / dx[age == k]
        ax.new[age <= 10 | age >= 70] <- ax[age <= 10 | age >= 70]
        ax <- ax.new
        qx <- 5 * mx / (1 + (5 - ax) * mx)
        qx[qx > 1] <- 0.99999
        qx[age == 85] <- 1
        px <- 1 - qx
        lx <- rep(NA, length(px))
        lx[age == 0] <- 100000
        for (k in seq(5, 85, 5))
            lx[age == k] <- lx[age == k - 5] * px[age == k - 5]
        dx <- lx * qx
        # save result of current iteration
        iter.dat[[i + 1]] <- list(ax = ax, qx = qx, lx = lx, dx = dx)
    }

    if (check.conv) {
        ax.iter <- sapply(iter.dat, `[[`, "ax")
        stopifnot(ax.iter[, num.iter] - ax.iter[, num.iter - 1] < 0.01)
    }
    iter.result <- iter.dat[[num.iter + 1]]
    ax <- iter.result$ax
    qx <- iter.result$qx
    lx <- iter.result$lx
    dx <- iter.result$dx

    Lx <- rep(NA, length(age))
    for (k in seq(0, 80, 5))
        Lx[age == k] <- 5 * lx[age == k + 5] + ax[age == k] * dx[age == k]
    Lx[age == 85] <- lx[age == 85] / mx[age == 85]
    Tx <- rep(NA, length(age))
    Tx[age == 85] <- Lx[age == 85]
    for (k in rev(seq(0, 80, 5)))
        Tx[age == k] <- Tx[age == k + 5] + Lx[age == k]
    ex <- Tx / lx

    # Re-insert missing values
    if(!is.null(na.idx)) {
        mx1 <- mx0
        mx1[-na.idx] <- mx
        mx <- mx1
        ax1 <- ax0
        ax1[-na.idx] <- ax
        ax <- ax1
        age <- age0
        qx1 <- rep(NA, length(mx0))
        qx1[-na.idx] <- qx
        qx <- qx1
        ex1 <- rep(NA, length(mx0))
        ex1[-na.idx] <- ex
        ex <- ex1
        Tx1 <- rep(NA, length(mx0))
        Tx1[-na.idx] <- Tx
        Tx <- Tx1
        Lx1 <- rep(NA, length(mx0))
        Lx1[-na.idx] <- Lx
        Lx <- Lx1
        lx1 <- rep(NA, length(mx0))
        lx1[-na.idx] <- lx
        lx <- lx1
    }
    if (full.table) return(data.frame(year = year, age = age, ax = ax, mx = mx, qx = qx,
                                      ex = ex, Tx = Tx, Lx = Lx, lx = lx))
    data.frame(age = age, year = year, mx = mx, qx = qx, ex = ex)
}
