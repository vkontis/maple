# TODO model desc, name
maple_models <- function(fixed.prec = 0.001,
                        gamma.shape = 1,
                        gamma.rate = 1e-3) {

    hpr <- paste0('list(prec = list(prior = "loggamma", param = c(', gamma.shape, ', ', gamma.rate, ')))')

    models <- list(
        list(
            name = "IIDAGE",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc1, model = "iid", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "RW1AGE",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc1, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "IIDAGE_RW1COH",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc1, model = "iid", hyper = hpr) +
            f(cohort.id1, yearc1, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "RW1AGE_RW1COH",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc, model = "rw1", hyper = hpr) +
            f(cohort.id1, yearc, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id3, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_IIDAGE",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "iid", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_RW1AGE",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_IIDAGE",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "iid", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_RW1AGE",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_IIDAGE_RW1COH",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "iid", hyper = hpr) +
            f(cohort.id1, yearc3.10a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.10b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL10_RW1AGE_RW1COH",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.10a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.10b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.10a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.10b, model = "rw1", hyper = hpr) +
            f(cohort.id1, yearc3.10a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.10b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_IIDAGE_RW1COH",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "iid", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "iid", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "iid", hyper = hpr) +
            f(cohort.id1, yearc3.6a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.6b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        ),
        list(
            name = "PWL6_RW1AGE_RW1COH",
            desc = "",
            fml = 'deaths ~
            f(age.id1, model = "rw1", hyper = hpr) +
            f(yearc1.6a, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(yearc1.6b, model = "linear", mean.linear = 0, prec.linear = fixed.prec) +
            f(age.id2, yearc2.6a, model = "rw1", hyper = hpr) +
            f(age.id3, yearc2.6b, model = "rw1", hyper = hpr) +
            f(cohort.id1, yearc3.6a, model = "rw1", hyper = hpr) +
            f(cohort.id2, yearc3.6b, model = "rw1", hyper = hpr) +
            f(year.id1, model = "rw1", hyper = hpr, group = age.id4, control.group = list(model = "exchangeable")) +
            f(epsilon.id1, model = "iid", hyper = hpr)'
        )
        )

    models <- lapply(models, function(m) {
        class(m) <- "inla.model"
        m$fml <- gsub("hpr", hpr, m$fml)
        m$fml <- gsub("fixed.prec", fixed.prec, m$fml)
        m$fml <- as.formula(m$fml)
        m
    })
    wl.models <- Filter(
        function(x) x$name %in% c("IIDAGE", "RW1AGE", "IIDAGE_RW1COH", "RW1AGE_RW1COH"),
        models)
    wl.models <- lapply(wl.models, function(x) {
        x$name <- paste0("WL0.05_", x$name)
        x$likelihood.weight.rate <- 0.05
        x
    })
    lc.models <- lapply(seq(5), function(n) {
        m <- list(
            name = paste0("LC_", n, "PC"),
            desc = paste0("Lee-Carter model with ", n, " principal components."),
            num.pcs = n
        )
        class(m) <- "lc.model"
        m
    })
    l <- c(models, wl.models, lc.models)
    setNames(l, sapply(l, `[[`, "name"))
}
