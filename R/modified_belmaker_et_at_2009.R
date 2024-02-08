#' Fit an example native discovery model
#'
#' @description
#' Fit an example Bayesian heirarchical model to data, using the proportion of
#' undiscovered alien species to the total number of undiscovered species (natives and aliens)
#' as the probability of detecting a new alien species. The model is described in full in Belmaker et al (2009),
#' with some modifications described in REDACTED ().
#'
#'
#' @param data a data.frame containing columns for alien and native discoveries
#' @param time_col the name of time since invasion column in the data
#' @param alien_col the name of alien discovery column in the data
#' @param native_col the name of native discovery column in the data
#' @param native_pool_size an integer of the assumed number of native species
#' @param priors a named numeric vector containing the elements:  \tabular{ll}{
#' \code{b0_mu} \tab prior for the b0 mean.\cr
#' \code{b1_mu} \tab prior for the b1 mean.\cr
#' \code{b0_sd} \tab prior for the b0 sd.\cr
#' \code{b1_sd} \tab prior for the b1 sd.\cr
#' }
#' @param ... additional arguments passed to cmdstanr's `sample`.
#'
#' @details
#' This function is mainly used to demonstrate how a Bayesian model can be implemented when aiming
#' to estimate the underlying introduction rates.
#' It can be used for user data if data on both alien and native discoveries are present and
#' ,model assumptions are met.
#'
#' @seealso [snc()] in which other external data can be used as dependent variables for either \eqn{\mu_t} or \eqn{\Pi_{st}}.
#'
#'
#' @return the fit value
#' @export
#'
#' @examples
#' data(medfish)
#' example_priors <- c(b0_mu = 0, b1_mu = 0, b0_sd = 0.01, b1_sd = 0.001)
#' native_discovery(data = medfish,
#'    time_col = "time",
#'    alien_col = "aliens",
#'    native_col = "natives",
#'    native_pool_size = 600,
#'    priors = example_priors,
#'    chains = 3, parallel_chains = 3)
native_discovery <- function(data, time_col, alien_col, native_col, native_pool_size, priors, ...){

  stan_file <- system.file("modified_belmaker_et_al_2009_model.stan", package = "alien")

  data_for_stan <- list(
    native_total = native_pool_size,  # Estimated number of native species
    N = nrow(data),             # Number of samples - number of rows for sampling data
    dI = data[[alien_col]],  # Number of newly discovered invasive species
    dN = data[[native_col]],    # Number of newly discovered native species
    t = data[[time_col]],         # Sampling events time
    # alpha_mu = b0_guess,
    b0_mu = priors[["b0_mu"]], b0_sd = priors[["b0_sd"]],
    b1_mu = priors[["b1_mu"]], b1_sd = priors[["b1_sd"]]
  )

  mod <- cmdstanr::cmdstan_model(stan_file)

  fit <- mod$sample(data = data_for_stan, ...)

  return(fit)
}
