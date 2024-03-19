test_that("snc function returns error when number of init values does not match number of parameters to estimate", {
  testthat::skip_if_not_installed("cli")
  testthat::skip_if_not_installed("stats")
  testthat::skip_if_not_installed("HelpersMG")
  expect_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, init = rnorm(3), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, init = rnorm(4), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, init = rnorm(5), growth = F, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_no_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, init = rep(0,5), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_no_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
})

test_that("snc function warns when no forumlas are given", {
  testthat::skip_if_not_installed("cli")
  testthat::skip_if_not_installed("stats")
  testthat::skip_if_not_installed("HelpersMG")
  expect_message(snc(y = sfestuary, data = data.frame(y = sfestuary, time = seq_along(sfestuary)))) |> suppressMessages()
  expect_message(snc(y = sfestuary, mu = ~ time,  data = data.frame(y = sfestuary, time = seq_along(sfestuary)))) |> suppressMessages()
  expect_message(snc(y = sfestuary, pi = ~ time, data = data.frame(y = sfestuary, time = seq_along(sfestuary)))) |> suppressMessages()
})

test_that("snc function stops when no data is given", {
  expect_error(snc(y = sfestuary, mu = ~ time, pi  = ~ time))
  expect_no_error(snc(y = sfestuary, mu = ~ time, pi  = ~time, data = data.frame(time = seq_along(sfestuary))))
})

testthat::skip_if_not_installed("cli")
testthat::skip_if_not_installed("stats")
testthat::skip_if_not_installed("HelpersMG")

model_ouput <- snc(y = sfestuary, mu = ~ time, pi  = ~time, data = data.frame(time = seq_along(sfestuary)))

test_that("plot_snc return a ggplot object", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  expect_s3_class(plot_snc(model_ouput, cumulative = F), c("gg","ggplot"))
  expect_s3_class(plot_snc(model_ouput, cumulative = T), c("gg","ggplot"))
})

test_that("plot_snc does not work for other object", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  expect_error(plot_snc(sfestuary, cumulative = F))
  expect_no_error(plot_snc(model_ouput, cumulative = F))
})

test_that("summary_snc does not work for other object", {
  expect_error(summary_snc(sfestuary))
  expect_no_error(summary_snc(model_ouput))
})
