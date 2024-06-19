## ----include = FALSE----------------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ----setup, results='hide'----------------------------------------------------
  library(alien)
  library(rstan)

## ----theme setup, echo = FALSE------------------------------------------------
  ggplot2::theme_set(
  ggplot2::theme_bw()+
  ggplot2::theme(axis.title = ggplot2::element_text(size = 20),
  axis.text = ggplot2::element_text(size = 18),
  panel.grid = ggplot2::element_blank())
  )

## ----reading_model_file-------------------------------------------------------
model_file <- system.file("stan/modified_belmaker_et_al_2009_model.stan", package = "alien")
readLines(model_file)

## ----medfish_data-------------------------------------------------------------
data(medfish)

head(medfish)

## ----medfish_plot, fig.width = 8, fig.height= 4.5, fig.align='center'---------
ggplot2::ggplot(medfish)+
  ggplot2::aes(x = year) + 
  ggplot2::geom_point(ggplot2::aes(y = cumsum(natives)), shape = 21, size = 2, fill = "#377EB8") +
  ggplot2::geom_point(ggplot2::aes(y = cumsum(aliens)), shape = 21,  size = 2, fill = "#E41A1C")

## ----setting_model------------------------------------------------------------
native_discoveries_model <- rstan::stan_model(model_file)

## ----show_data_list-----------------------------------------------------------
readLines(model_file)[1:11]

## ----set_data_list------------------------------------------------------------
data_for_stan <- list(
  N = nrow(medfish),
  native_total = 600,
  dI = medfish$aliens,
  dN = medfish$natives,
  t = medfish$t
)

## ----show_priors--------------------------------------------------------------
readLines(model_file)[7:10]

## ----naive_model--------------------------------------------------------------
naive_model <- stats::glm(aliens ~ time, data = medfish, family = "poisson")
stats::summary.glm(naive_model)

## ----priors-------------------------------------------------------------------
coef_table <- summary(naive_model)$coefficients
priors <- c(
  b0_mu = coef_table[1,1],
  b0_sd = coef_table[1,2],
  b1_mu = coef_table[2,1],
  b1_sd = coef_table[2,2]
)
priors

## ----join_data----------------------------------------------------------------
data_for_stan <- c(data_for_stan, priors)

## ----include=FALSE------------------------------------------------------------
set.seed(1)

## ----sampling, message=FALSE, warning=FALSE, results='hide'-------------------
model_output <- rstan::sampling(native_discoveries_model, data_for_stan)

## ----plotting_results, fig.width = 8, fig.height= 4.5, fig.align='center'-----
stan_summary <- summary(model_output, pars = "unrecorded_I")$summary |>
  tibble::as_tibble() |> 
  tibble::add_column(year = medfish$year)

ggplot2::ggplot(medfish)+
  ggplot2::aes(x = year) + 
  ggplot2::geom_point(ggplot2::aes(y = cumsum(natives)), shape = 21, size = 2, fill = "#377EB8") +
  ggplot2::geom_point(ggplot2::aes(y = cumsum(aliens)), shape = 21,  size = 2, fill = "#E41A1C") +
  ggplot2::geom_ribbon(data = stan_summary, ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2) +
  ggplot2::geom_line(data = stan_summary, ggplot2::aes(y = mean)) + 
  ggplot2::labs(x = "Year", y = "Cumulative number\nof discovered species")

