prep_inla_dat <- function(deaths, population, model, forecast.horizon) {

    dat <- data.frame(
        year = rep(as.numeric(colnames(deaths)), each = nrow(deaths)),
        age = rep(as.numeric(rownames(deaths)), ncol(deaths)),
        deaths = as.vector(deaths),
        population = as.vector(population)
    )

    dat$death.rate <- dat$deaths / dat$population
    dat$forecast.period <- FALSE
    forecast.years <- max(dat$year) + seq(forecast.horizon)
    forecast.dat <- lapply(forecast.years, function(y) {
        d <- dat[dat$year == min(dat$year), ]
        d$deaths <- d$death.rate <- NA
        d$year <- y
        d$forecast.period <- TRUE
        d
    })

    forecast.dat <- do.call(rbind, forecast.dat)
    inla.dat <- rbind(dat, forecast.dat)
    inla.dat$cohort <- inla.dat$year - inla.dat$age

    # Parse model formula and create variables
    fmlstr <- as.character(model$fml[3])
    inla.dat$epsilon <- seq_len(nrow(inla.dat))

    idx.vars <- as.list(inla.dat[c("age", "cohort", "year", "epsilon")])
    idx.dat <- lapply(seq_along(idx.vars), function(i) {
        var <- idx.vars[[i]]
        s <- paste0(names(idx.vars)[i], ".id")
        num.fx <- max(sapply(regmatches(fmlstr, gregexpr(s, fmlstr)), length))
        if (num.fx == 0) return(NULL)
        setNames(
            data.frame(replicate(num.fx, as.integer(factor(var)), simplify = FALSE)),
            paste0(s, seq(num.fx)))
    })
    idx.dat <- do.call(cbind, Filter(Negate(is.null), idx.dat))
    inla.dat <- cbind(inla.dat, idx.dat)

    time.vars <- unique(unlist(regmatches(fmlstr,
                    gregexpr("yearc[0-9]*(\\.[0-9]*[ab])?", fmlstr))))
    lin.vars <- unique(unlist(regmatches(time.vars,
                    gregexpr("^yearc[0-9]*$", time.vars))))
    pw.vars <- unique(unlist(regmatches(time.vars,
                    gregexpr("^yearc[0-9]*\\.[0-9]*[ab]$", time.vars))))
    knot.pos <- unique(as.numeric(gsub("[ab]", "",
                    unlist(lapply(strsplit(pw.vars, "\\."), `[[`, 2)))))

    inla.dat$yearc1 <- inla.dat$year - mean(inla.dat$year)
    for (var in setdiff(lin.vars, "yearc1")) inla.dat[[var]] <- inla.dat$yearc1

    for (kp in knot.pos) {
        vars.a <- unlist(regmatches(pw.vars,
                    gregexpr(paste0("^yearc[0-9]*\\.", kp, "a$"), pw.vars)))
        vars.b <- unlist(regmatches(pw.vars,
                    gregexpr(paste0("^yearc[0-9]*\\.", kp, "b$"), pw.vars)))
        maxyear <- max(inla.dat[!inla.dat$forecast.period, ]$year) - kp
        maxyearc <- max(inla.dat[inla.dat$year <= maxyear, ]$yearc1)
        inla.dat[vars.a] <- ifelse(inla.dat$year <= maxyear, inla.dat$yearc1, maxyearc)
        inla.dat[vars.b] <- ifelse(inla.dat$year <= maxyear, 0, inla.dat$yearc1 - maxyearc)
    }

    # inla.dat <- left_join(inla.dat, .Read5a0Data(), by = c("iso3", "sex", "year", "age"))
    inla.dat
}

