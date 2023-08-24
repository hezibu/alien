
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alien: an R package for estimating alien introduction rates

<!-- badges: start -->
<!-- badges: end -->

alien is a package dedicated to easily estimate the introduction rates
of alien species given first records data. It specializes in addressing
the role of sampling on the pattern of discoveries, thus providing
better estimates than using Generalized Linear Models which assume
perfect immediate detection of newly introduced species.

## Installation

You can install the development version of alien from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("REDACTED/alien")
```

## Basic Usage

For the most basic demonstration, let’s look at the data provided in
Solow and Costello (2004) which describes discoveries of introduced
species in the San Francisco estuary (California, USA) between the years
1850–1995. We’ll plot it in a cumulative form, replicating the plot from
Solow and Costello (2004):

``` r
library(alien)
library(ggplot2)

data("sfestuary")
years <- seq_along(sfestuary) + 1850

ggplot()+
  aes(x = years, y = cumsum(sfestuary))+
  geom_line() + 
  coord_cartesian(ylim = c(0,150))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1860, 1980, 20)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 150, 50)) + 
  ylab("Cumulative discoveries") + theme(axis.title.x = element_blank())
```

<img src="man/figures/README-plotting data-1.png" width="80%" height="80%" style="display: block; margin: auto;" />

### Model Fitting

As described thoroughly, these discoveries also entail trends in the
probability of detecting new alien species. To estimate the introduction
rate, ${\beta_1}$, from these data, we will fit the Solow and Costello
model using the `snc` function. We can use the `control` argument to
pass a list of options to `optim` which does the Maximum-Likelihood
Estimation[^1]:

``` r
model <- snc(y = sfestuary, control = list(maxit = 1e4))
#> ! no data supplied, using time as independent variable
```

When only a vector describing discoveries is supplied, `snc` warns users
that it uses the time as the independent variable, similar to the
original S&C model.

The result is a list containing several objects:

``` r
names(model)
#> [1] "records"        "convergence"    "log-likelihood" "coefficients"  
#> [5] "predict"
```

We’ll go over each.

#### Records

Shows the supplied records data.

``` r
model$records
#>   [1] 0 0 1 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 2 0 1 5 1 0 0 1 1 0 0 0 0 0 0 0 0
#>  [38] 0 0 1 1 0 2 2 2 1 1 1 0 0 2 0 0 3 0 1 1 2 0 0 0 1 1 0 0 0 0 0 0 2 0 0 0 0
#>  [75] 0 0 1 0 1 1 1 1 0 0 1 1 2 4 0 0 0 0 2 0 4 2 1 1 1 0 3 0 1 1 4 0 1 1 0 0 1
#> [112] 2 4 0 1 1 0 1 1 1 2 3 4 1 0 3 5 4 5 1 0 4 2 0 1 4 1 1 2 0 1 7 4 0 0
```

#### Convergence

Did the optimation algorithm converge? This prints out the convergence
code from `optim`:

``` r
model$convergence
#> [1] 0
```

| Code | Meaning/Troubleshooting                                                                                              |
|------|----------------------------------------------------------------------------------------------------------------------|
| 0    | Successful convergence                                                                                               |
| 1    | Iteration limit `maxit` had been reached (increase `maxit` using `control = list(maxit = number))`                   |
| 10   | Degeneracy of the Nelder-Mead simplex                                                                                |
| 51   | Warning from the `"L-BFGS-B"`method; Use `debug(snc)` and check the `optim` component `message` for further details. |
| 52   | Error from the `"L-BFGS-B"`method; Use `debug(snc)` and check the `optim` component `message` for further details.   |

#### log-likelihood

The log-likelihood at the end point of the algorithm (preferably at
convergence). Can be used for model selection if needed:

``` r
model$`log-likelihood`
#> [1] 118.7776
```

#### coefficients

The parameter estimates.

- `beta0` signifies ${\beta_0}$ - the intercept for ${\mu}$.
- `gamma0` signifies ${\gamma_0}$ - the intercept for ${\Pi}$.
- `gamma2` signifies ${\gamma_2}$ - and will only appear when the `snc`
  argument `growth` is set to `TRUE` (the default).

``` r
model$coefficients
#>   coefficient      Estimate      Std.Err
#> 1       beta0   -1.12739745     1.835403
#> 2        time    0.01401579     1.835403
#> 3      gamma0 -185.89484996 15630.676343
#> 4        time  -79.80040427  7235.922667
#> 5      gamma2   76.23985293  6339.859143
```

#### predict

The fitted ${\lambda_t}$ values of the model. The mean of the Poisson
distribution from which the records are assumed to derive.

``` r
model$predict
#>   [1] 7.943124e-49 3.284464e-01 3.330822e-01 3.377835e-01 3.425512e-01
#>   [6] 3.473861e-01 3.522893e-01 3.572616e-01 3.623042e-01 3.674179e-01
#>  [11] 3.726038e-01 3.778629e-01 3.831963e-01 3.886049e-01 3.940898e-01
#>  [16] 3.996522e-01 4.052931e-01 4.110136e-01 4.168148e-01 4.226980e-01
#>  [21] 4.286641e-01 4.347145e-01 4.408502e-01 4.470726e-01 4.533828e-01
#>  [26] 4.597821e-01 4.662716e-01 4.728528e-01 4.795269e-01 4.862952e-01
#>  [31] 4.931590e-01 5.001196e-01 5.071786e-01 5.143371e-01 5.215967e-01
#>  [36] 5.289588e-01 5.364248e-01 5.439961e-01 5.516743e-01 5.594609e-01
#>  [41] 5.673574e-01 5.753654e-01 5.834864e-01 5.917220e-01 6.000738e-01
#>  [46] 6.085435e-01 6.171328e-01 6.258433e-01 6.346767e-01 6.436349e-01
#>  [51] 6.527194e-01 6.619322e-01 6.712751e-01 6.807498e-01 6.903582e-01
#>  [56] 7.001022e-01 7.099838e-01 7.200048e-01 7.301673e-01 7.404733e-01
#>  [61] 7.509246e-01 7.615235e-01 7.722721e-01 7.831723e-01 7.942263e-01
#>  [66] 8.054364e-01 8.168047e-01 8.283335e-01 8.400250e-01 8.518815e-01
#>  [71] 8.639054e-01 8.760989e-01 8.884646e-01 9.010048e-01 9.137220e-01
#>  [76] 9.266187e-01 9.396975e-01 9.529608e-01 9.664113e-01 9.800517e-01
#>  [81] 9.938846e-01 1.007913e+00 1.022139e+00 1.036566e+00 1.051196e+00
#>  [86] 1.066034e+00 1.081080e+00 1.096339e+00 1.111813e+00 1.127506e+00
#>  [91] 1.143420e+00 1.159559e+00 1.175925e+00 1.192523e+00 1.209355e+00
#>  [96] 1.226424e+00 1.243734e+00 1.261289e+00 1.279092e+00 1.297145e+00
#> [101] 1.315454e+00 1.334021e+00 1.352850e+00 1.371944e+00 1.391309e+00
#> [106] 1.410946e+00 1.430861e+00 1.451057e+00 1.471538e+00 1.492308e+00
#> [111] 1.513371e+00 1.534731e+00 1.556393e+00 1.578361e+00 1.600639e+00
#> [116] 1.623231e+00 1.646142e+00 1.669376e+00 1.692939e+00 1.716834e+00
#> [121] 1.741066e+00 1.765640e+00 1.790561e+00 1.815834e+00 1.841464e+00
#> [126] 1.867455e+00 1.893813e+00 1.920543e+00 1.947651e+00 1.975141e+00
#> [131] 2.003019e+00 2.031290e+00 2.059961e+00 2.089036e+00 2.118522e+00
#> [136] 2.148424e+00 2.178747e+00 2.209499e+00 2.240685e+00 2.272311e+00
#> [141] 2.304384e+00 2.336909e+00 2.369893e+00 2.403343e+00 2.437265e+00
```

### Plotting

Once we’ve fitted the model, we can use its fit to easily plot
${\lambda_t}$ along with the first records using the function
`plot_snc`. Users can choose either `annual` or `cumulative` plots.
Because the output is a `ggplot` object, it can easily be customized
further:

``` r

plot_snc(model, type = "cumulative") +
  coord_cartesian(ylim = c(0,150))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1860, 1980, 20)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 150, 50)) + 
  ylab("Cumulative discoveries") + theme(axis.title.x = element_blank())
```

<img src="man/figures/README-plotting fit-1.png" width="80%" height="80%" style="display: block; margin: auto;" />

[^1]: In this case we increase `maxiter` so the algorithm will converge
