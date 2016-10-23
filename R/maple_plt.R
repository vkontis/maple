kt_extension <- function(lx70) {
    # lx70: lx for 70 and older (should have 4 rows - one for each age group
    # 70-74, 75-79, 80-84, 85+ and as many columns as years)
    # This function calculates the average number of years lived by
    # those who die in each age group 70-74, 75-79, 80-84, 85+.
    # Returns a matrix of same dimensions as lx70.

    # For age groups >= 70, calculate hazard rate using the approximation
    # mu(x+1/2) ~ -log(1 - q) = -log(p) where p is the probability of
    # survival to the next age group and equals l(x+5) / lx
    mux <- (log(lx70[-nrow(lx70), , drop = FALSE]) - log(lx70)[-1, , drop = FALSE]) / 5

    # Calculate lx for 1-year age groups from 70 to 85. For 70, 75, 80, 85
    # use known values. For the rest use interpolation l71 = l70 * exp(-mu70),
    # l72 = l70 * exp(-2 * mu70),..., l84 = l80 * exp(-4 * mu80)
    lx70 <- rbind(
        lx70[rep(seq(3), each = 5), , drop = FALSE] *
            exp(-seq(0, 4) * mux[rep(seq(3), each = 5), , drop = FALSE]),
        lx70[4, , drop = FALSE]
    )

    # Calculate dx and qx for 1-year age groups using lx, for ages >= 70
    dx70 <- rbind(lx70[-nrow(lx70), , drop = FALSE] - lx70[-1, , drop = FALSE],
                  lx70[nrow(lx70), , drop = FALSE]
    )
    qx70 <- dx70 / lx70

    # Run regression on logit of probability of dying
    logitqx70 <- log(qx70 / (1 - qx70))
    logitqx70[nrow(logitqx70), ] <- NA # Not defined for 85+
    y <- as.vector(logitqx70)
    x <- rep(70:85 + .5, length.out = length(y))
    num.yr <- length(y) / 16
    yr <- as.factor(rep(seq_len(num.yr), each = 16))
    w <- as.vector(dx70)
    if (nlevels(yr) == 1) {
        mod <- lm(y ~ x, weights = x)
        logA <- mod$coefficients[1] # intercept
        B <- mod$coefficients[2] # slope
    } else {
        mod <- lm(y ~ 0 + yr + yr:x, weights = w)
        logA <- mod$coefficients[paste0("yr", seq(num.yr))] # intercepts
        B <- mod$coefficients[paste0("yr", seq(num.yr), ":x")] # slopes
    }

    # Calculate qx for age x >= 85
    logitqx85 <- t(logA + outer(B, (85:129 + .5))) # predict logit qx for age >= 85
    qx85 <- exp(logitqx85) / (1 + exp(logitqx85)) # invert logit transform

    # Calculate lx values for age x >= 85
    lx85 <- matrix(nrow = nrow(logitqx85), ncol = ncol(logitqx85))
    lx85[1, ] <- lx70[nrow(lx70), ] # last entry of vector holding lx70-85
    for (k in seq(2, nrow(lx85)))
        lx85[k, ] <- lx85[k - 1, ] * (1 - qx85[k - 1, ])

    # Calculate dx for age x >= 85
    dx85 <- lx85 * qx85

    # Join lx70 (holding lx for 1-year age groups from 70 to 85) with
    # lx85 (holding lx for 1-year age groups from 85 to 129)
    # For the intersecting point, corresponding to age 85, we keep
    # the value in lx85, estimated via the method above
    lx70 <- rbind(lx70[-nrow(lx70), , drop = FALSE], lx85)
    dx70 <- rbind(dx70[-nrow(dx70), , drop = FALSE], dx85)

    # Collapse back to 5-year age groups 70-74, 75-79, 80-84 and
    # 85+, calculating average number of years lived by those who
    # die in each age group
    # We assume that deaths occur at the midpoint of each 1-year
    # age group so the number of years lived in the age group
    # 70-74 by someone who dies at age 73 is 3.5; similarly, the
    # number of years lived in the age group 85+ by someone who dies
    # at age 100 is 15.5, etc.
    # yl is the years lived in the (current) age group at the time of death
    yl <- 70:129 - c(rep(c(70, 75, 80), each = 5), rep(85, length(85:129))) + 0.5

    # 5 year age group that each individual age from 70 to 129 belongs to
    x5y <- seq(70, 85, 5)[findInterval(70:129, seq(70, 85, 5))]

    ax70 <- as.vector(t(sapply(split(seq(nrow(dx70)), x5y),
                               function(v) colSums(dx70[v, , drop = FALSE] * yl[v]) /
                                   colSums(dx70[v, , drop = FALSE]))
    ))
    ax70
}

maple_plt <- function(death.rates, ax = NULL, check.conv = FALSE, full.table = FALSE) {
    # Generate a period life table for the supplied inputs
    # age: vector of age groups, 0, 5, 10, ..., 80, 85 (may be repeated
    #     if more than 1 years of data are available)
    # mx: mortality rates corresponding to ages in "age"
    # ax: average number of years lived by those who die in each age group
    #     NA values are replaced by 2.5. The ax for the open-ended age group
    #     85+ is calculated using the Kannisto-Thatcher method [Thatcher et al,
    #     The survivor ratio method for estimating numbers at high ages,
    #     Demographic Research (6) 2002]. For age groups 5-9 to 80-84 the ax
    #     are calibrated using an iterative procedure described on p.47 of
    #     Preston et al, Demography: measuring and modeling population processes,
    #     2001.
    # check.conv: If TRUE, it will test that the iterative procedure to estimate
    #     ax (see above) has converged
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

plt_ex <- function(plt, x = 0) {
    if (!all(c("age", "year", "ex") %in% names(plt))) {
        stop("Life table must include 'age', 'year', 'ex' columns.")
    }
    setNames(plt[plt$age == x, ]$ex, plt[plt$age == x, ]$year)
}

plt_qx <- function(plt, x = 70) { 
    if (!all(c("age", "year", "ex") %in% names(plt))) {
        stop("Life table must include 'age', 'year', 'ex' columns.")
    }
    plt1 <- plt[plt$age < x, ]
    qx <- plt1$qx[order(plt1$year, plt1$age)]
    m <- matrix(qx, ncol = length(unique(plt1$year)))
    setNames(calculate_pod(m), plt[plt$age == x, ]$year)
}

calculate_pod <- function(m) {
    # m matrix of age x year qx
    1 - Reduce(`*`, as.data.frame(1 - t(m)))
}

