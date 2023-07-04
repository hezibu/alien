test_that("mu function returns expected values", {
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-2,0)),
               rep(exp(-2), 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-1,0)),
               rep(exp(-1), 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,0)),
               rep(exp(0), 10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,-2)),
               exp(-2*(1:10)))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,-1)),
               exp(-1*(1:10)))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,1)),
               exp(1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(0,2)),
               exp(2*(1:10)))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(-1,1)),
               exp(-1 + 1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(1,1)),
               exp(1 + 1:10))
  expect_equal(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(1,2)),
               exp(1 + 2 * 1:10))
})

test_that("mu function returns error when less beta than needed values are supplied", {
  expect_error(build_mu_function(formula = ~ a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2)))
  expect_error(build_mu_function(formula = ~ a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3)))
  expect_no_error(build_mu_function(formula = ~ 0 + a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3)))
})

test_that("mu function returns error when more beta than needed values are supplied", {
  expect_error(build_mu_function(formula = ~ t, data.frame(y = rep(1,10), t = 1:10), beta = c(2,3,4)))
  expect_error(build_mu_function(formula = ~ t + 0, data.frame(y = rep(1,10), t = 1:10), beta = c(2,2)))
  expect_no_error(build_mu_function(formula = ~ 0 + a + b, data.frame(y = rep(1,10), a = 1:10, b = rep(3,10)), beta = c(2,3)))
})
