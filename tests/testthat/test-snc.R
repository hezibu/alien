test_that("snc function returns error when number of init values does not match number of parameters to estimate", {
  expect_error(snc(y = sfestuary, mu = ~ time, y = ~ time, init = rnorm(3), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_error(snc(y = sfestuary, mu = ~ time, y = ~ time, init = rnorm(4), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_error(snc(y = sfestuary, mu = ~ time, y = ~ time, init = rnorm(5), growth = F, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_no_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, init = rep(0,5), growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_no_error(snc(y = sfestuary, mu = ~ time, pi = ~ time, growth = T, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
})

test_that("snc function warns when no forumlas are given", {
  expect_message(snc(y = sfestuary, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_message(snc(y = sfestuary, mu = ~ time,  data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
  expect_message(snc(y = sfestuary, pi = ~ time, data = data.frame(y = sfestuary, time = seq_along(sfestuary))))
})

test_that("snc function stops when no data is given", {
  expect_error(snc(y = sfestuary, mu = ~ time, pi  = ~time))
  expect_no_error(snc(y = sfestuary))
  expect_no_error(snc(y = sfestuary, mu = ~ time, pi  = ~time, data = data.frame(time = seq_along(sfestuary))))
})


test_that("plot_snc return a ggplot object", {
  expect_equal(class(plot_snc(snc(sfestuary), cumulative = F)), c("gg","ggplot"))
  expect_equal(class(plot_snc(snc(sfestuary), cumulative = T)), c("gg","ggplot"))
})

test_that("plot_snc does not work for other object", {
  expect_error(plot_snc(sfestuary, cumulative = F))
  expect_no_error(plot_snc(snc(sfestuary), cumulative = F))
})

test_that("summary_snc does not work for other object", {
  expect_error(summary_snc(sfestuary))
  expect_no_error(summary_snc(snc(sfestuary)))
})
