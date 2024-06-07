# Function to create a sequence from its center value
#  Input: center value, proportion of lower and upper values in relation to center,
#         and number of steps from the center.
# Output: A sequence of length steps_from_center + 1

seq_from_center <- function(center, range, steps_from_center){
  first_part <- seq(from = center, to = center + center * range, length.out = steps_from_center + 1)
  second_part <- seq(from = center - (center * range), to =  center , length.out = steps_from_center + 1)

  return(sort(unique(c(first_part,second_part))))
}

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

split_mu_pi <- function(vector_values, names_mu){
  vector_mu <- vector_values[1:length(names_mu)]
  vector_pi <-  vector_values[(length(names_mu) + 1):length(vector_values)]

  return(list(mu = vector_mu,
              pi = vector_pi))
}


likelihood_space <- function(y, mu = NULL, pi = NULL, data = NULL, growth = TRUE, type = "exponential", centers, ranges, steps_from_centers, ...){
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

  # centers_split <- split_mu_pi(centers, names_mu)
  # ranges_split <- split_mu_pi(ranges, names_mu)
  # steps_split <- split_mu_pi(steps_from_centers, names_mu)

  parameter_values <- get_parameter_space(parameter_list = c(names_mu, names_pi),
                                          growth = growth,
                                          centers = centers,
                                          ranges = ranges,
                                          steps_from_centers = steps_from_centers)


  log_likelihoods <- apply(parameter_values, MARGIN = 1, function(x)
    alien:::snc_ll_function(y,
                            mu_formula = mu,
                            pi_formula = pi,
                            data = data,
                            growth = growth,
                            type = type,
                            x = x))

  likelihood_space <- cbind(parameter_values, log_likelihoods)

  return(likelihood_space)
}
