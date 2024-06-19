# Function to create a sequence from its center value
#  Input: center value, proportion of lower and upper values in relation to center,
#         and number of steps from the center.
# Output: A sequence of length steps_from_center + 1

seq_from_center <- function(center, range, steps_from_center){
  first_part <- seq(from = center, to = center + center * range, length.out = steps_from_center + 1)
  second_part <- seq(from = center - (center * range), to =  center , length.out = steps_from_center + 1)

  return(sort(unique(c(first_part,second_part))))
}


# Function to create a dataframe containing the parameter values
get_parameter_space <- function(parameter_list, growth, centers, ranges, steps_from_centers) {
  parameter_space <- mapply(seq_from_center, centers, ranges, steps_from_centers,
                            SIMPLIFY = F) |>
    expand.grid()

  if (growth) {
    parameter_list <- c(parameter_list, "gamma2")
  }

  colnames(parameter_space) <- parameter_list


  parameter_space
}

# function to check if number of values provided matches number of parameters
check_repair_values <- function(vector_value, value_name, n_parameters){
  if (!length(vector_value) %in% c(1, n_parameters)){
    cli::cli_abort("Supplied {length(vector_value)} {value_name} value{?s} to {n_parameters} predictor{?s}, please supply either {n_parameters} values or 1 value.")
  }
  if (length(vector_value) == 1){
    cli::cli_alert("Supplied one {value_name} value to {n_parameters} predictor{?s}, setting all {value_name} values to {vector_value}")
    vector_value <- rep(vector_value, n_parameters)
  }
  return(vector_value)
}


#' Calculate log-likelihood space for a given time series and data
#'
#' @description
#' This function allows to calculate the log-likelihood values for a given set of parameter values.
#' It can be used for diagnostics of the likelihood space and check - among others - for parameter contribution to log-likelihood.
#' passing `FALSE` for the `calculate` argument allows to experiment with different parameter values sets before calculating their log-likelihoods.
#' Please note: This functions may take a lot of time, depending on the number of parameters, and steps!
#'
#' @param y either a vector describing the number of discovered alien and invasive species (IAS) over a given time period, or the name (quoted or unquoted) of the corresponding column in the provided data.
#' @param mu a formula defining the predictors for \eqn{\mu_t}, the annual introduction rate. Formulas should be provided in the syntax `~ x1 + x2 + ... + xn`. Use `~ 1` for an intercept only model.
#' @param pi a formula defining the predictors for \eqn{\Pi_{st}}, the annual probability of detection. Formulas should be provided in the syntax `~ x1 + x2 + ... + xn`. Use `~ 1` for an intercept only model.
#' @param data a data frame containing the variables in the model(s).
#' @param growth logical. Should the population growth parameter \eqn{\gamma_2} be included in the model for \eqn{\Pi_{st}}?. Note that values for `init`, if provided, need to include an initial value for the growth parameter, when `growth = TRUE`.
#' @param type Define whether the mu function should be on "linear" or "exponential" scale. Defaults to "exponential".
#' @param centers the values used as the center-points for the parameter values. Usually the maximum likelihood estimation from a call to `snc`. Length should be either 1 or correspond to number of parameters (including the growth parameter).
#' @param ranges the range of the parameter values in term of the center. The values will be withing `center - (center * range)` and `center + (center * range)`. Length should be either 1 or correspond to number of parameters (including the growth parameter).
#' @param steps_from_centers How many values in the range between `center` and `center + (center * range)` should be included in the set. Length should be either 1 or correspond to number of parameters (including the growth parameter).
#' @param calculate Whether to calculate the log-likelihoods (TRUE) or just view the parameter values (FALSE).
#'
#' @return A data frame containing the parameter values, and if `calculate = TRUE` including the log-likelihood values for each parameter combination.
#' @export
#'
#' @examples
#' \donttest{
#' if (FALSE){ # this takes a while...
#' example_model <- snc(sfestuary)
#' estimates <- example_model$coefficients$Estimate
#'
#' likelihood_space <- likelihood_space(y = sfestuary, centers = estimates,
#'                                      ranges = 1.5, steps_from_center = 10,
#'                                      calculate = TRUE)
#'
#' # with specified formula:
#' example_buba <- snc(y = aliens, pi = ~ natives, data = medfish)
#' estimates <- example_buba$coefficients$Estimate
#'
#' likelihood_space <- likelihood_space(y = aliens, pi = ~ natives,
#'                                      data = medfish, centers = estimates,
#'                                      ranges = 1.5, steps_from_center = 10,
#'                                      calculate = TRUE)
#'  }
#' }
likelihood_space <- function(y, mu = NULL, pi = NULL, data = NULL, growth = TRUE, type = "exponential", centers, ranges, steps_from_centers, calculate = TRUE){
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

  centers <- check_repair_values(centers, "center", n_predictors)
  ranges <- check_repair_values(ranges, "range", n_predictors)
  steps_from_centers <- check_repair_values(steps_from_centers, "steps", n_predictors)

  # name the covariates parametersload
  names_mu <- colnames(predictors_mu)
  if ("(Intercept)" %in% names_mu) names_mu[[1]] <- "beta0"                 # rename intercept to beta0
  if ("time" %in% names_mu) names_mu[which(names_mu == "time")] <- "beta1"  # define beta1 as change with time
  names_pi <- colnames(predictors_pi)
  if ("(Intercept)" %in% names_pi) names_pi[[1]] <- "gamma0"                # rename intercept to gamma0
  if ("time" %in% names_pi) names_pi[which(names_pi == "time")] <- "gamma1" # define gamma1 as change with time

  parameter_values <- get_parameter_space(parameter_list = c(names_mu, names_pi),
                                          growth = growth,
                                          centers = centers,
                                          ranges = ranges,
                                          steps_from_centers = steps_from_centers)


  if (!calculate){
    return (parameter_values)
  } else {
    log_likelihoods <- apply(parameter_values, MARGIN = 1, function(x)
      snc_ll_function(y,
                      mu_formula = mu,
                      pi_formula = pi,
                      data = data,
                      growth = growth,
                      type = type,
                      x = x))

    likelihood_space <- cbind(parameter_values, log_likelihoods)

    return(likelihood_space)
  }
}

#' Plot log-likelihood space
#'
#' @description
#' Plot a filled-contour heatmap displaying the log-likelihood space calculated using `likelihood_space`.
#'
#'
#' @param likelihood_space a data frame resulting from a call to `likelihood_space`, where columns represent parameter values and an extra column `log_likelihoods` with the corresponding log-likelihood for each parameter value combination.
#' @param x a string specifying the parameter to be plotted on the x-axis.
#' @param y a string specifying the parameter to be plotted on the y-axis.
#' @param delta the range of log-likelihood values of which different color codes will be plotted. Defaults to 10.
#'
#' @return
#' #' A `ggplot` plot with the corresponding type of plot.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (FALSE){ # takes a while to finish
#' example_model <- snc(sfestuary)
#' estimates <- example_model$coefficients$Estimate
#'
#' likelihood_space <- likelihood_space(y = sfestuary, centers = estimates,
#'                                      ranges = 1.5, steps_from_center = 10,
#'                                      calculate = TRUE)
#'
#' likelihood_map(likelihood_space, "beta0", "beta1", 10)
#'  }
#' }
likelihood_map <- function(likelihood_space, x, y, delta = 10) {

  if (!x %in% colnames(likelihood_space)){
    cli::cli_abort("Parameter {x} does not exist in this log-likelihood space table")
  }
  if (!y %in% colnames(likelihood_space)){
    cli::cli_abort("Parameter {y} does not exist in this log-likelihood space table")
  }

  min_ll <- likelihood_space |> dplyr::filter(.data$log_likelihoods == min(.data$log_likelihoods))

  colnames <- setdiff(colnames(likelihood_space), c(x,y, "log_likelihoods"))

  min_ll_value <- min_ll$log_likelihoods

  breaks <-  c(seq(min_ll_value-1, min_ll_value + delta, length.out = delta), Inf)

  gg <- dplyr::semi_join(likelihood_space, min_ll, by = colnames) |>
    ggplot2::ggplot()+
    ggplot2::aes(x = .data[[x]], y = .data[[y]], z = .data$log_likelihoods)+
    ggplot2::geom_contour_filled(breaks = breaks) +
    ggplot2::geom_point(data = min_ll, size = 5, shape = 10, color = "red", show.legend = FALSE)+
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete(expand = c(0,0))+
    ggplot2::scale_y_discrete(expand = c(0,0))+
    ggplot2::scale_fill_viridis_d(direction = -1,
                                  # trans = "log10",
                                  guide = ggplot2::guide_colorsteps(
                                    even.steps = T,
                                    frame.colour = "black",
                                    ticks.colour = NA,
                                    barwidth=0.5,
                                    barheight = 15)) +
    ggplot2::labs(fill = "Log-Likelihood")+
    ggplot2::theme(aspect.ratio = 1, legend.position = "left", legend.text = ggplot2::element_text(size = 16))

  return(gg)

}


