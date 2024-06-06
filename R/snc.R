#' Fit a model to discovery records
#'
#' @description
#' Fit a model to a time series describing first records of alien species.
#' If no external data is provided, the function fits the model described in Solow and Costello (2004).
#' When external data on either \eqn{\mu_t} or \eqn{\Pi_{st}} is provided, the function fits the
#' modified model (sampling-proxy model in the case of \eqn{\Pi_{st}}) as described in Buba et al (2024).
#'
#' @param y either a vector describing the number of discovered alien and invasive species (IAS) over a given time period, or the name (quoted or unquoted) of the corresponding column in the provided data.
#' @param mu a formula defining the predictors for \eqn{\mu_t}, the annual introduction rate. Formulas should be provided in the syntax `~ x1 + x2 + ... + xn`. Use `~ 1` for an intercept only model.
#' @param pi a formula defining the predictors for \eqn{\Pi_{st}}, the annual probability of detection. Formulas should be provided in the syntax `~ x1 + x2 + ... + xn`. Use `~ 1` for an intercept only model.
#' @param data a data frame containing the variables in the model(s).
#' @param init Optional. Initial values supplied to `optim`. Must be same length as the total number of parameters.
#' @param growth logical. Should the population growth parameter \eqn{\gamma_2} be included in the model for \eqn{\Pi_{st}}?. Note that values for `init`, if provided, need to include an initial value for the growth parameter, when `growth = TRUE`.
#' @param type Define whether the mu function should be on "linear" or "exponential" scale. Defaults to "exponential".
#' @param ... Further arguments passed to `optim`.
#'
#' @details
#' This function expands on the model described in Solow and Costello (2004) by facilitating the
#' inclusion of external data to describe either introduction rate \eqn{\mu_t} or detection probability \eqn{\Pi_{st}}.
#' The model with external data for detection probability (sampling-proxy model) is described fully in Buba et al (2024).
#' The use of external data to describe the introduction rate has not been thoroughly evaluated.
#' When no formula is defined for either, the function automatically fits the original Solow and Costello (2004)
#' model using the length of the vector data as the independent variable \eqn{t}.
#' All models return estimated parameters for both the introduction rate and the detection probability,
#' with the parameter of interest usually being the change of introduction rate - \eqn{\beta_1}.
#' For more details see the Basic Usage vignette:
#' \code{vignette("basic_usage", package = "alien")}
#'
#' @return `snc` returns an object of class "snc" containing: \tabular{ll}{
#' \code{records} \tab the supplied first records data \cr
#' \code{convergence} \tab the `optim` convergence code.\cr
#' \code{log-likelihood} \tab the maximum log-likelihood.\cr
#' \code{coefficients} \tab a named vector of the ML estimates of the   coefficients.\cr
#' \code{fitted.values} \tab the fitted mean \eqn{\lambda} values.\cr
#' \code{predict} \tab a data frame containing the estimated mean +- Standard error \eqn{\mu} values.\cr
#' }
#'
#' @references Solow, A. R., & Costello, C. J. (2004). Estimating the rate of species introductions from the discovery record. Ecology, 85(7), 1822–1825. https://doi.org/10.1890/03-3102
#' @references Buba, Y., Kiflwai, M., McGeoch, M. A., Belmaker, J. (2024) Evaluating models for estimating introduction rates of alien species from discovery records. https://doi.org/10.1111/geb.13859
#' @export
#'
#' @examples
#' \donttest{
#' # Solow and Costello (2004) model:
#' data(sfestuary)
#' example_model <- snc(sfestuary)
#' print(example_model)
#'
#' # Buba et al (2024) sampling-proxy model:
#' data(medfish)
#' example_buba <- snc(y = aliens, pi = ~ natives, data = medfish)
#' print(example_buba)
#' }
snc <- function(y, mu = NULL, pi = NULL, data = NULL, init = NULL, growth = TRUE, type = "exponential", ...){

  if (missing(data)){
    # if data is not supplied, meaning only y (first records) is supplied
    time <- seq_along(y)
    data <- cbind.data.frame(y, time)
    if (missing(pi)&missing(mu)){
      # if no covariates supplied only the length of the first records data is used
      cli::cli_alert_warning("no data supplied, using time as independent variable")
      mu <- stats::formula(~ time)
      pi <- stats::formula(~ time)
    } else
      if (isTRUE(pi != stats::formula(~1)) | isTRUE(mu != stats::formula(~1))) {
        # if no data is supplied but covariates are specified, throw an error
        cli::cli_abort("Please supply a dataframe containing independent variables for mu or pi")
      }
  } else {
    y_col <- substitute(y)
    if (inherits(y_col, "call")) {
      y <- eval(y_col)
    } else if (inherits(y_col, "name")) {
      if (!exists(y_col)) {
        y_col <- deparse(y_col)
        if (!y_col %in% colnames(data)) {
          cli::cli_abort("Column {y_col} missing from data!")
        } else {
          y <- data[[y_col]]
        }
      }
    } else if (inherits(y_col, "character")) {
      if (!y_col %in% colnames(data)) {
        cli::cli_abort("Column {y_col} missing from data!")
      } else {
        y <- data[[y_col]]
      }
    }
  }

  if (missing(mu)){
    # if no covariates supplied only the length of the first records data is used
    cli::cli_alert_warning("No formula defined for mu, using time as independent variable")
    time <- seq_along(y)
    mu <- stats::formula(~ time)
  }

  if (missing(pi)){
    # if no covariates supplied only the length of the first records data is used
    cli::cli_alert_warning("No formula defined for pi, using time as independent variable")
    time <- seq_along(y)
    pi <- stats::formula(~ time)
  }

  # create a model matrix using the covariates
  predictors_mu <- stats::model.matrix(object = mu, data = data)
  predictors_pi <- stats::model.matrix(object = pi, data = data)

  # count number of predictors
  n_predictors <- ncol(predictors_mu) +  ncol(predictors_pi) + growth

  # check if number of predictors match number of initial values for stats::optim
  if (!is.null(init) & length(init) != (n_predictors)){
    cli::cli_abort("Supplied {length(init)} initial value{?s} to {n_predictors} predictor{?s}")
  }

  # name the covariates parameters
  names_mu <- colnames(predictors_mu)
  if ("(Intercept)" %in% names_mu) names_mu[[1]] <- "beta0"                 # rename intercept to beta0
  if ("time" %in% names_mu) names_mu[which(names_mu == "time")] <- "beta1"  # define beta1 as change with time
  names_pi <- colnames(predictors_pi)
  if ("(Intercept)" %in% names_pi) names_pi[[1]] <- "gamma0"                # rename intercept to gamma0
  if ("time" %in% names_pi) names_pi[which(names_pi == "time")] <- "gamma1" # define gamma1 as change with time

  if (is.null(init)){
    # if no initial values supplied, use 0
    n_init <- length(c(names_mu, names_pi)) + growth
    init <-  rep(0, n_init)
    if (type == "linear") init[1:2] <- c(1,1) # use as 1 as initial value for linear mu functions (uses 0 for exponential)
  }

  # run the optimization function
  optim_out <-  stats::optim(snc_ll_function,
                             y = y,
                             par = init,
                             mu_formula = mu,
                             pi_formula = pi,
                             data = data,
                             growth = growth,
                             hessian = TRUE,
                             type = type,
                             ...)

  # define output
  out <- list()

  coefficients <- optim_out[["par"]]
  names(coefficients) <- c(names_mu, names_pi, if (growth) "gamma2")

  coefs_se <- HelpersMG::SEfromHessian(a = optim_out[["hessian"]]) # use hessian to get estimate standard error
  names(coefs_se) <- names(coefficients)

  coef_table <- data.frame(
    Estimate = coefficients,
    Std.Err = coefs_se
  )

  out$records <- y
  out$convergence <- optim_out$convergence
  out$`log-likelihood` <-  optim_out$value
  out$coefficients <- coef_table
  out$type <- type
  out$fitted.values <- calculate_lambda(mu = mu,
                                        pi = pi,
                                        data = data,
                                        beta = coefficients[names_mu],
                                        gamma = coefficients[names_pi],
                                        growth_param = ifelse(growth, coefficients["gamma2"], 0),
                                        type = type)
  out$predict <- predict_mu(formula =  mu,
                            data = data,
                            beta = coefficients[names_mu],
                            error = coefs_se[names_mu],
                            type = type)
  class(out) <- "snc"
  return(out)
}

#' Plot an introduction record and the fitted model values
#'
#' @param object an object of class "snc", usually a result of a call to `snc`
#' @param cumulative logical - should plot be annual or cumulative number of IAS.
#'
#' @return
#' A `ggplot` plot with the corresponding type of plot.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(sfestuary)
#' example_model <- snc(sfestuary)
#' plot_snc(example_model, cumulative = TRUE)
#' }
plot_snc <- function(object, cumulative = FALSE){

  if (!inherits(object, "snc")){
    cli::cli_abort("Attempt to plot a wrong type of object")
  }

  plot_data <- dplyr::tibble(observed = object[["records"]],
                             time = seq_along(object[["records"]]),
                             fitted = object[["fitted.values"]])

  lab <- "Number of IAS"

  if (cumulative) {
    plot_data <- plot_data |>
      dplyr::mutate(dplyr::across(c("observed","fitted"), .fn = cumsum))
    lab <- paste0("Cumulative ", lab)
  }

  plot_data <- plot_data |>
    tidyr::pivot_longer(cols = c("observed","fitted"), names_to = "name", values_to = "y")

  p <- ggplot2::ggplot(plot_data)+
    ggplot2::aes(x = .data$time, y = .data$y, linetype = .data$name) +
    ggplot2::geom_line()+
    ggplot2::scale_linetype_manual(values = c("observed" = 2, "fitted" = 1),
                                   labels = c("observed" = "First Records", "fitted" = "Fitted Values")) +
    ggplot2::ylab(lab) +
    ggplot2::theme(legend.box.just = "left", legend.title = ggplot2::element_blank(), legend.position = "bottom")

  return(p)
}

#' Summarize a Solow and Costello Model Fit
#'
#' @param object an object of class "snc", usually a result of a call to `snc`
#'
#' @return A data.frame containing the model estimates, standard error, and the probability of the true value being 0
#' under the given estimates and errors.
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(sfestuary)
#' example_model <- snc(sfestuary)
#' summary(example_model)
#' }
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
