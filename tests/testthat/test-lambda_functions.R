test_that("mu function returns expected values", {
  testthat::skip_if_not_installed("stats")
  testthat::skip_if_not_installed("cli")
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-2,0), type = "exponential"),
               rep(exp(-2), 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-1,0), type = "exponential"),
               rep(exp(-1), 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,0), type = "exponential"),
               rep(exp(0), 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,-2), type = "exponential"),
               exp(-2*(1:10)))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,-1), type = "exponential"),
               exp(-1*(1:10)))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,1), type = "exponential"),
               exp(1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,2), type = "exponential"),
               exp(2*(1:10)))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-1,1), type = "exponential"),
               exp(-1 + 1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(1,1), type = "exponential"),
               exp(1 + 1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(1,2), type = "exponential"),
               exp(1 + 2 * 1:10))

  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-2,0), type = "linear"),
               rep(-2, 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-1,0), type = "linear"),
               rep(-1, 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,0), type = "linear"),
               rep(0, 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,-2), type = "linear"),
               -2*(1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,-1), type = "linear"),
               -1*(1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,1), type = "linear"),
               1:10)
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,2), type = "linear"),
               2*(1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-1,1), type = "linear"),
               -1 + 1:10)
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(1,1), type = "linear"),
               1 + 1:10)
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(1,2), type = "linear"),
               1 + 2 * 1:10)

})

test_that("mu function returns error when less beta than needed values are supplied", {
  testthat::skip_if_not_installed("stats")
  testthat::skip_if_not_installed("cli")
  expect_error(build_mu_function(formula = ~ a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2), type = "exponential"))
  expect_error(build_mu_function(formula = ~ a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3), type = "exponential"))
  expect_no_error(build_mu_function(formula = ~ 0 + a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3), type = "exponential"))
})

test_that("mu function returns error when more beta than needed values are supplied", {
  testthat::skip_if_not_installed("stats")
  testthat::skip_if_not_installed("cli")
  expect_error(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(2,3,4), type = "exponential"))
  expect_error(build_mu_function(formula = ~ t + 0, data.frame(y = rep(1,10), t = 1:10), beta = c(2,2), type = "linear"))
  expect_no_error(build_mu_function(formula = ~ 0 + a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3), type = "exponential"))
  expect_no_error(build_mu_function(formula = ~ 0 + a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3), type = "linear"))
})
