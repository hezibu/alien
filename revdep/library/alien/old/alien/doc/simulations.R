## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(alien)

## -----------------------------------------------------------------------------
list_tsl <- list(
  b0  = c(-1,      # mean introduction rate of 0.3 sp/y
          0,       # mean introduction rate of  1  sp/y
          1),      # mean introduction rate of 2.7 sp/y
  b1  = c(-0.02,   # annual decrease of  5%
          -0.01,   # annual decrease of 10%
          -0.005,  # annual decrease of 20%
          0,       # constant introduction rate
          0.005,   # annual increase of  5% 
          0.01,    # annual increase of 10% 
          0.02),   # annual increase of 20% 
  pi0 = c(-4,      # annual observation probability of 0.017,   ~55 years delay
          -2,      # annual observation probability of 0.119,   ~8 years delay
          0,       # annual observation probability of   0.5,   ~2 years delay
          2,       # annual observation probability of   0.88,  ~0.13 years delay
          4),      # annual observation probability of   0.98,  ~0.02 year delay
  pi1 = c( 0,      # annual increase of     0% in probability of detection
           0.5,   # annual increase of  0.02% in probability of detection
           1,  # annual increase of   0.2% in probability of detection
           2), # effect of population size on detection probability
  pi2 = c( 0,      # no effect of population size on detection probability
           1e-7,   # normal effect of population size on detection probability
           0.001), # effect of population size on detection probability
  length = seq(30,200,15)
)

df_tsl <- expand.grid(list_tsl)

est_names <- c("b0_est", "b1_est", "pi0_est", "pi1_est", "pi2_est")
ses_names <- c("b0_se", "b1_se", "pi0_se", "pi1_se", "pi2_se")

## ----subsetting data----------------------------------------------------------
set.seed(3333) # Set a seed for replication
df_tsl <- df_tsl |> dplyr::sample_n(1)

## ----theme setup, echo = FALSE------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()+
    ggplot2::theme(axis.title = ggplot2::element_text(size = 20),
                   axis.text = ggplot2::element_text(size = 18),
                   panel.grid = ggplot2::element_blank())
)

## ----show lambda, fig.width = 8, fig.height= 4.5, fig.align='center'----------
params <- as.numeric(df_tsl[1:5]) # parameters
names(params) <- c("b0","b1","pi0","pi1", "pi2") # set names
time_span <- as.numeric(df_tsl[6]) # time series length.

sampling_lambda <- alien:::calculate_lambda(mu = ~ time,
                                            pi = ~ time,
                                            data = data.frame(time = seq_len(time_span)),
                                            beta = params[c("b0","b1")],
                                            gamma = params[c("pi0","pi1")],
                                            growth_param = params["pi2"],
                                            type = "exponential")
ggplot2::ggplot()+
  ggplot2::aes(x = seq_len(time_span), y = sampling_lambda)+
  ggplot2::geom_line(linewidth = 1.1) + 
  ggplot2::labs(x = "Time", y = expression("\U03BB"[t]))


## ----simulation---------------------------------------------------------------
list <- apply(df_tsl,
                MARGIN = 1, FUN = function(row) {


                  # Defining variables and parameters:
                  params <- as.numeric(row[1:5])
                  names(params) <- c("b0","b1","pi0","pi1", "pi2")
                  time_span <- row[6] # time series length.

                  # Create a faux sampling vector based on the time span to be used for our sampling_proxy
                  sampling_vector <- scale(1:time_span) |> as.numeric()

                  # Create lambda vector
                  sampling_lambda <- alien:::calculate_lambda(mu = ~ time,
                                            pi = ~ time,
                                            data = data.frame(time = seq_len(time_span)),
                                            beta = params[c("b0","b1")],
                                            gamma = params[c("pi0","pi1")],
                                            growth_param = params["pi2"],
                                            type = "exponential")
                  
                  number_of_repetitions <- 1
                  
                  result <- lapply(seq_len(number_of_repetitions), function(iteration) {

                    # Creating an introduction
                    sampling_data <- rpois(time_span, sampling_lambda)

                    # model fitting start
                    naive_model <- glm(formula = sampling_data ~ seq_len(time_span), family = "poisson")

                    guess_all <- c(b0 = 0, b1 = 0, pi0 = 0, pi1 = 0, pi2 = 0)

                    snc_model <- alien::snc(y = sampling_lambda, type = "exponential",
                                            mu = ~ time,
                                            pi = ~ time, growth = T,
                                            init = guess_all,
                                            data = data.frame(time = seq_len(time_span)),
                                            control = list(maxit = 10000))


                    constant_detection_model <- alien::snc(y = sampling_lambda, type = "exponential",
                                                           mu = ~ time,
                                                           pi = ~ 1, growth = F,
                                                           init = guess_all[1:3],
                                                           data = data.frame(time = seq_len(time_span)),
                                                           control = list(maxit = 10000))


                    sampling_proxy_model <- alien::snc(y = sampling_lambda, type = "exponential",
                                                       mu = ~ time,
                                                       pi = ~ sampling_vector, growth = F,
                                                       init = guess_all[1:4],
                                                       data = data.frame(time = seq_len(time_span), sampling_vector = sampling_vector),
                                                       control = list(maxit = 10000))

                    # model fitting end
                    
                    # Summarize results start
                    naive_par <-  coefficients(summary(naive_model))[,1] |>  dplyr::bind_rows() |> `colnames<-`(est_names[1:2])
                    naive_se <-  coefficients(summary(naive_model))[,2] |> dplyr::bind_rows() |> `colnames<-`(ses_names[1:2])
                    naive_convergence <- tibble::tibble(converged = naive_model$converged)

                    naive_row <- dplyr::bind_cols(naive_par, naive_se, naive_convergence) |>
                      tibble::add_column(method = "naive")

                    list_of_rows <- lapply(list(snc_model, constant_detection_model, sampling_proxy_model), function(est) {

                      method <- dplyr::case_when(
                        identical(est, snc_model) ~ "snc",
                        identical(est, constant_detection_model) ~ "snc_3p",
                        identical(est, sampling_proxy_model) ~ "snc_vec"
                      )

                      if (any(class(est) == "try-error")){
                        est_row <- rep(NA,11)
                        names(est_row) <- c("b0_est", "b1_est", "pi0_est",
                                            "pi1_est", "pi2_est", "b0_se", "b1_se",
                                            "pi0_se", "pi1_se", "pi2_se", "converged")
                        est_row <- dplyr::bind_rows(est_row) |> tibble::add_column(method = method)
                      } else {

                        par <- est$coefficients$Estimate |> `names<-`(est_names[1:nrow(est$coefficients)]) |> dplyr::bind_rows()
                        ses <- est$coefficients$`Std.Err` |> `names<-`(ses_names[1:nrow(est$coefficients)])  |> dplyr::bind_rows()
                        convergence <- tibble::tibble(converged = est$convergence == 0)
                        est_row <- dplyr::bind_cols(par, ses, convergence) |> tibble::add_column(method = method)
                      }
                    }
                    )

                    params <- dplyr::bind_rows(naive_row,
                                        dplyr::bind_rows(list_of_rows)) |>
                      tibble::add_column(iteration = iteration)

                    # summarize results end
                    return(list(params = params,
                                full = list(simulation = sampling_data,
                                            estimates = snc_model,
                                            estimates_3p = constant_detection_model,
                                            estimates_vec = sampling_proxy_model,
                                            perfect_detection = coefficients(summary(naive_model))
                                )
                    )
                    )
                  })

                  simulation_estimates <- dplyr::bind_rows(purrr::map(result, 1))
                  full <- purrr::map(result, 2)

                  return(list(sim_params = row,
                              estimates = simulation_estimates,
                              full = full))
                })


## ----view results-------------------------------------------------------------

purrr::map(list, 2) |> dplyr::bind_rows() # for the summary of each iteration


## ----fig.width = 7------------------------------------------------------------
t <- seq(0, 4*pi, length.out = 100)
a <- 3
b <- 0.6
set.seed(100) # This is done to maintain reproducability within time series lengths
c.norm <- rnorm(100)
amp <- 2

sampling_vector <- 3 * a*sin(b*t)+c.norm*amp + 0.3 * 1:100

sampling_vector <- scale(sampling_vector) |> as.numeric()


ggplot2::ggplot()+
  ggplot2::aes(x = 1:100, y = sampling_vector)+
  ggplot2::geom_line(linewidth = 1.1) + 
  ggplot2::labs(x = "Time", y = "Sampling proxy value")

## ----lambda_with_proxy, fig.width = 8, fig.height= 4.5, fig.align='center'----
params["pi1"] <- 0.2
time_span <- 100
lambda_with_proxy <- alien:::calculate_lambda(mu = ~ time,
                                            pi = ~ sampling_vector,
                                            data = data.frame(time = seq_len(time_span),
                                                              sampling_vector = sampling_vector),
                                            beta = params[c("b0","b1")],
                                            gamma = params[c("pi0","pi1")],
                                            growth_param = params["pi2"],
                                            type = "exponential")

ggplot2::ggplot()+
  ggplot2::aes(x = 1:100, y = lambda_with_proxy)+
  ggplot2::geom_line(linewidth = 1.1) + 
  ggplot2::labs(x = "Time", y = expression("\U03BB"[t]))

