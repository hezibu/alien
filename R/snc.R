#' Fit  Solow and Costello (2004) model
#'
#' @description
#' Fit the model described in Solow and Costello (2004) to data.
#' It can use external data on either \eqn{\mu_t} or \eqn{\Pi_{st}}.
#'
#' @param y a time series of the annual number of discovered alien and invasive species (IAS)
#' @param mu a formula defining the predictors for \eqn{\mu_t}, the annual introduction rate. Formulas should be provided in the syntax `~ x1 + x2 + ... + xn`.
#' @param pi a formula defining the predictors for \eqn{\Pi_{st}}, the annual probability of detection. Formulas should be provided in the syntax `~ x1 + x2 + ... + xn`.
#' @param data a data frame containing the variables in the model(s).
#' @param growth logical. Should the population growth parameter \eqn{\gamma_2} be included in the model for \eqn{\Pi_{st}}?. Note that values for `init`, if provided, need to include an initial value for the growth parameter, when `growth = TRUE`.
#' @param ... Further arguments passed to `optim`.
#' @param init Optional. Initial values supplied to `optim`. Must be same length as the total number of parameters.
#'
#' @details
#' This function expands on the model described in Solow and Costello (2004) by facilitating the
#' inclusion of external data to describe either \eqn{\mu_t} or \eqn{\Pi_{st}}.
#' The model with external data is described fully in REDACTED ().
#' When no formula is defined for either, the function automatically fits the original Solow and Costello (2004)
#' model using the length of the vector data as the independent variable \eqn{t}.
#' The original model uses Rcpp for shorter run time.
#' When numerous estimations are required for a more elaborate model (i.e, for simulation studies or bootstrapping),
#' users may be benefit from building upon the function described in `filename.cpp`
#'
#' @return `snc` returns an object of class "snc" containing: \tabular{ll}{
#' \code{records} \tab the supplied first records data \cr
#' \code{convergence} \tab the `optim` convergence code.\cr
#' \code{log-likelihood} \tab the maximum log-likelihood.\cr
#' \code{coefficients} \tab a named vector of the ML estimates of the   coefficients.\cr
#' \code{fitted.values} \tab the fitted mean \eqn{\lambda} values.\cr
#' }
#'
#' @references Solow, A. R., & Costello, C. J. (2004). Estimating the rate of species introductions from the discovery record. Ecology, 85(7), 1822–1825. https://doi.org/10.1890/03-3102
#' @export
#'
#' @examples
#' data(sfestuary)
#' example_model <- snc(sfestuary)
#' print(example_model)
snc <- function(y, mu = NULL, pi = NULL, data = NULL, init = NULL, growth = T, type = "exponential", ...){

  if (missing(data)){
    if(missing(pi)&missing(mu)){
      cli::cli_alert_warning("no data supplied, using time as independent variable")
      time <- seq_along(y)
      data <- cbind.data.frame(y, time)
      mu <- stats::formula(~ time)
      pi <- stats::formula(~ time)
    } else {
      cli::cli_abort("Please supply a dataframe containing independent variables for mu or pi")
    }
  }

  if (missing(mu)){
    cli::cli_alert_warning("No formula defined for mu, using time as independent variable")
    time <- seq_along(y)
    mu <- stats::formula(~ time)
  }

  if (missing(pi)){
    cli::cli_alert_warning("No formula defined for pi, using time as independent variable")
    time <- seq_along(y)
    pi <- stats::formula(~ time)
  }

  predictors_mu <- stats::model.matrix(object = mu, data = data)
  predictors_pi <- stats::model.matrix(object = pi, data = data)

  n_predictors <- ncol(predictors_mu) +  ncol(predictors_pi) + growth

  if (!is.null(init) & length(init) != (n_predictors)){
    cli::cli_abort("Supplied {length(init)} initial value{?s} to {n_predictors} predictor{?s}")
  }

  names_mu <- colnames(predictors_mu)
  if ("(Intercept)" %in% names_mu) names_mu[[1]] <- "beta0"
  if ("time" %in% names_mu) names_mu[which(names(names_mu) == "time")] <- "beta1"
  names_pi <- colnames(predictors_pi)
  if ("(Intercept)" %in% names_pi) names_pi[[1]] <- "gamma0"
  if ("time" %in% names_pi) names_pi[which(names(names_pi) == "time")] <- "gamma1"

  if (is.null(init)){
    n_init <- length(c(names_mu, names_pi)) + growth
    init <-  rep(0, n_init)
    if (type == "linear") init[1:2] <- c(1,1)
  }

  optim_out <-  stats::optim(snc_ll_function,
                             y = y,
                             par = init,
                             mu = mu,
                             pi = pi,
                             data = data,
                             growth = growth,
                             hessian = T,
                             type = type,
                             ...)

  out <- list()

  coefficients <- optim_out[["par"]]
  names(coefficients) <- c(names_mu, names_pi, if (growth) "gamma2")

  coefs_se <- HelpersMG::SEfromHessian(a = optim_out[["hessian"]])
  names(coefs_se) <- names(coefficients)

  coef_table <- data.frame(
    coefficient = names(coefficients),
    Estimate = coefficients,
    Std.Err = coefs_se
  )

  out$records <- y
  out$convergence <- optim_out$convergence
  out$`log-likelihood` <-  optim_out$value
  out$coefficients <- coef_table
  out$predict <- calculate_lambda(mu = mu,
                                  pi = pi,
                                  data = data,
                                  beta = coefficients[names_mu],
                                  gamma = coefficients[names_pi],
                                  growth_param = ifelse(growth, coefficients["gamma2"], 0),
                                  type = type)
  class(out) <- "snc"
  return(out)
}

#' Plot an introduction record and the fitted Solow and Costello (2004) values
#'
#' @param object an object of class "snc", usually, a result of a call to `snc`
#' @param cumulative logical - should plot be annual or cumulative number of IAS.
#'
#' @return
#' A `ggplot` plot with the corresponding type of plot.
#'
#' @export
#'
#' @examples
#' data(sfestuary)
#' example_model <- snc(sfestuary)
#' plot_snc(example_model, "cumulative")
plot_snc <- function(object, cumulative = F){

  if (!inherits(object, "snc")){
    cli::cli_abort("Attempt to plot a wrong type of object")
  }

  plot_data <- dplyr::tibble(observed = object[["records"]],
                             time = seq_along(object[["records"]]),
                             predict = object[["predict"]])

  lab <- "Number of IAS"

  if (cumulative) {
    plot_data <- plot_data |>
      dplyr::mutate(dplyr::across(c("observed","predict"), .fn = cumsum))
    lab <- paste0("Cumulative ", lab)
  }

  plot_data <- plot_data |>
    tidyr::pivot_longer(cols = c("observed","predict"), names_to = "name", values_to = "y")

  p <- ggplot2::ggplot(plot_data)+
    ggplot2::aes_string(x = "time", y = "y", linetype = "name") +
    ggplot2::geom_line()+
    ggplot2::scale_linetype_manual(values = c("observed" = 2, "predict" = 1),
                                   labels = c("observed" = "First Records", "predict" = expression("\U03bb"[t]))) +
    ggplot2::ylab(lab) +
    ggplot2::theme(legend.box.just = "left", legend.title = ggplot2::element_blank(), legend.position = "bottom")

  return(p)
}

#' Summarize a Solow and Costello Model Fit
#'
#' @param object an object of class "snc", usually, a result of a call to `snc`
#'
#' @return A data.frame containing the model estimates, standard error, and the probability of the true value being 0
#' under the given estimates and errors.
#' @export
#'
#' @examples
#' data(sfestuary)
#' example_model <- snc(sfestuary)
#' summary(example_model)
summary_snc <- function(object) {

  if (!inherits(object, "snc")){
    cli::cli_abort("Attempt to summarize a wrong type of object")
  }

  coefficients <- object[["coefficients"]]

  prob <- apply(coefficients, MARGIN = 1, FUN = function(row){
    return(stats::pnorm(q = 0, mean = row[1], sd = row[2]))
  })

  summary_table <- cbind(coefficients, Prob.Zero = prob)

  return(summary_table)

}
