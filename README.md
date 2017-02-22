# maple

Welcome to __maple__: model averaged projections of life expectancy.

__maple__ is an R package for fitting statistical models on demographic data to produce projections of death rates and life expectancy. It contains the code used in [V. Kontis, J.E. Bennett, C.D. Mathers, G. Li, K. Foreman, M. Ezzati, Future life expectancy in 35 industrialised countries: projections with a Bayesian model ensemble, Lancet (2017), doi: 10.1016/S0140-6736(16)32381-9](http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(16)32381-9/fulltext).  

## Prerequisites

The code uses the [INLA](http://www.r-inla.org/) algorithm to fit most of the models, implemented in the R package __INLA__. More information on how to download and install it can be found on the [INLA website](http://www.r-inla.org/download). Note that you may need to install package __sp__ first by running `install.packages("sp")`.

## Installation

The easiest way to install __maple__ is using the [__devtools__](https://github.com/hadley/devtools) package:

    devtools::install_github("vkontis/maple")

If you do not have __devtools__, you can install it from CRAN, by typing `install.packages("devtools")`.

Alternatively, clone or download the repository to a local directory and install it by running `install.packages(<local_dir>, repos = NULL, type = "source")`.

## Running the models

#### Getting some data

You will need some data on death counts and population. These need to be formatted into matrices, with one row for each age group and one column for each year of data. For now, __maple__ only handles 5-year age groups 0-4, 5-9, ..., 80-84 and 85+ years, so it expects matrices with 18 rows.
The column names of these matrices must be set to the years of data. The package comes with example data, stored in `maple.deaths` and `maple.population`:

    > data(maple.deaths)
    > maple.deaths[1:5, 49:53]
             2008       2009       2010       2011       2012
    0   1717.9084  1669.4182  1613.4061  1587.3184  1516.6746
    5    144.5131   127.9170   128.8043   112.0376   129.0808
    10   148.1691   161.2160   146.1746   127.1724   131.4167
    15   509.2995   485.2724   435.0701   368.0076   351.2908
    20   729.2682   666.7927   649.6085   604.0687   564.8684

There is another matrix, `maple.ax`, which can optionally be provided to the model fitting methods, and holds the number of years lived on average in each age group, by those who die in that age group (denoted  *<sub>n</sub>a<sub>x</sub>* in demography). We will not worry about this for now, more info can be found in the help file `?maple_plt`.

#### Choosing which models to fit

The models contained in the package can be obtained by typing `models <- maple_models()`. This will return a list of models, all or a subset of which can be used in the model fitting methods. More details on the models can be found in the published paper and its appendix. To choose a subset of models, run `models.to.run <- maple_models()[c("RW1AGE", "LC_1PC")]`.

#### Running individual models

###### 1) Running a single model

    fit <- maple_fit_model(models.to.run$LC_1PC,
                           deaths = maple.deaths,
                           population = maple.population,
                           forecast.horizon = 10)

This will fit a Lee-Carter model to the data and return an object containing information about the fit, including the estimated model parameters and the estimated death rates

    >fit$rates[1:5, 49:53]
               2008         2009         2010         2011         2012
    0  9.465266e-04 8.793542e-04 8.290460e-04 7.423325e-04 7.234237e-04
    5  8.532998e-05 7.960949e-05 7.530884e-05 6.786028e-05 6.622960e-05
    10 1.052803e-04 1.000591e-04 9.606762e-05 8.900666e-05 8.743379e-05
    15 2.839088e-04 2.739918e-04 2.663051e-04 2.524669e-04 2.493400e-04
    20 3.881237e-04 3.787097e-04 3.713404e-04 3.579050e-04 3.548378e-04

###### 2) Running an ensemble of models and calculating posterior samples

    fits <- maple_fit_ensemble(deaths = maple.deaths,
                               population = maple.population,
                               forecast.horizon = 10,
                               models = models.to.run)

This will fit the models currently listed in `models.to.run` to calculate some posterior samples for age-specific death rates, life expectancy and probability of dying, and statistical summaries from these samples. Samples for each individual model are stored in `fits$samples`. For example, `fits$samples$RW1AGE` is a list of length 1000 (the default number of samples) each element of which holds a life table draw, for example the 12<sup>th</sup> element:

    > head(fits$samples$RW1AGE[[12]])
      age year           mx          qx       ex
    1   0 1960 0.0050361105 0.024622544 73.29968
    2   5 1960 0.0003788656 0.001892535 70.13744
    3  10 1960 0.0002911676 0.001454779 65.26569
    4  15 1960 0.0005034493 0.002514267 60.35713
    5  20 1960 0.0006476626 0.003233193 55.50260
    6  25 1960 0.0006784103 0.003386501 50.67434

Statistical summaries calculated from these samples are stored as a data frame under `fits$sample.summaries`, including the mean, median, standard deviation and 95% credible interval:

    > head(fits$sample.summaries)
       model year age metric         mean       median           sd           lb           ub
    1 RW1AGE 1960   0   rate 0.0049698784 0.0049693898 4.805665e-05 0.0048756943 0.0050636170
    2 RW1AGE 1960   5   rate 0.0003692761 0.0003690454 9.588001e-06 0.0003510777 0.0003873282
    3 RW1AGE 1960  10   rate 0.0002847784 0.0002848100 7.571953e-06 0.0002696875 0.0002997572
    4 RW1AGE 1960  15   rate 0.0005147145 0.0005144855 1.246367e-05 0.0004909577 0.0005385888
    5 RW1AGE 1960  20   rate 0.0006396528 0.0006395553 1.376471e-05 0.0006151523 0.0006686139
    6 RW1AGE 1960  25   rate 0.0006925368 0.0006929623 1.591788e-05 0.0006625961 0.0007242749

The values in the age column 0, 5, ..., 80, 85 correspond to age groups 0-4, 5-9, ..., 80-84, 85+. The metrics are:

 - "rate": The death rate.
 - "ex": Life expectancy at the beginning of the age group in the age column; e.g. when `age == 0` this is life expectancy at birth, and when `age == 65` life expectancy at age 65 years.
 - "qx": Probability of dying by the end of the age group in the age column; e.g. when `age == 65` (which corresponds to the age group 65-69 years), this is the probability of dying between birth and age 70 years (this probability is always 1 for the open ended age group 85+).

The individual model fits are stored under `fits$model.fits`.

#### Forecasting and model averaging

    bma <- maple(deaths = maple.deaths, population = maple.population,
                  forecast.horizon = 10, holdout = 13, models = models.to.run,
                  num.draws = 5000)

This will fit all selected models twice: once to calculate model weights and once to produce the projections. The first run uses all but the last 13 years of data (specified in the `holdout` argument) to estimate model parameters and project them forward. The projection error over these 13 years is used to calculate model weights. The second run uses all the data to fit the models and produce forecasts for the next 10 years (specified in the `forecast.horizon` argument). The weights from the first run are then used to combine the models into a model average.

Due to the multimodality and the usually large variance of the posterior model average, a large number of samples (at least 1,000 and preferably 5,000 as specified in the `num.draws` argument) is recommended.

Model weights are stored under `bma$model.weights`:

    > bma$model.weights
      RW1AGE    LC_1PC
    0.5320914 0.4679086

Model average posterior samples for death rates, life expectancy and probability of dying are stored under `bma$sample.summaries`, while those of individual models are stored under `bma$individual.model.sample.summaries`.

## Questions

Please feel free to open an issue with any questions you may have.

## License

The __maple__ code is available under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html). __INLA__ uses external libraries which come with different software licenses (more info on the [INLA website](https://www.math.ntnu.no/~hrue/r-inla.org/license/license.txt)).
