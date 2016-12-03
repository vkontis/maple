# Kanisto Thatcher method
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
