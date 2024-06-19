test_that("likelihood_space function returns error when number of center/range/steps values does not match number of parameters", {
  testthat::skip_if_not_installed("cli")
  testthat::skip_if_not_installed("stats")
  expect_error(likelihood_space(y = sfestuary, mu = ~ time, pi = ~ time, centers = rnorm(3), ranges = rnorm(5), steps_from_center = rep(2,5), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary)), calculate = FALSE))
  expect_error(likelihood_space(y = sfestuary, mu = ~ time, pi = ~ time, centers = rnorm(5), ranges = rnorm(3), steps_from_center = rep(2,5), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary)), calculate = FALSE))
  expect_error(likelihood_space(y = sfestuary, mu = ~ time, pi = ~ time, centers = rnorm(5), ranges = rnorm(5), steps_from_center = rep(2,3), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary)), calculate = FALSE))
  expect_error(likelihood_space(y = aliens, mu = ~ time, pi = ~ natives, data = medfish, growth = F, centers = rnorm(5), ranges = 1, steps_from_center = 4, calculate = FALSE))
  expect_no_error(likelihood_space(y = sfestuary, mu = ~ time, pi = ~ time, centers = rnorm(5), ranges = rnorm(5), steps_from_center = rep(2,5), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary)), calculate = FALSE))
  expect_no_error(likelihood_space(y = sfestuary, mu = ~ time, pi = ~ time, centers = 1, ranges = 1.5, steps_from_center = 5, growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary)), calculate = FALSE))
})

testthat::skip_if_not_installed("cli")
testthat::skip_if_not_installed("stats")
testthat::skip_if_not_installed("HelpersMG")

model_ouput <- snc(y = sfestuary, mu = ~ time, pi  = ~time, data = data.frame(time = seq_along(sfestuary)))
estimates <- model_ouput$coefficients$Estimate

likelihood_space <- likelihood_space(y = sfestuary, centers = estimates, ranges = 1.5, steps_from_center = 2, calculate = TRUE)


test_that("likelihood_map needs column names to be passed as arguments", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  expect_error(likelihood_map(likelihood_space, "a", "b", 20))
})

test_that("likelihood_map return a ggplot object", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  expect_s3_class(likelihood_map(likelihood_space, "beta0", "beta1", 20), c("gg","ggplot"))
})

