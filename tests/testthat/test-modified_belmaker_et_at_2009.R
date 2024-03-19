test_that("bayesian_example returns appropriate class object", {
  example_priors <- c(b0_mu = 0, b1_mu = 0, b0_sd = 0.01, b1_sd = 0.001)
  invisible(
    capture.output(
      mod_output <-
        suppressMessages(
          native_discovery(data = medfish,
                           time_col = "time",
                           alien_col = "aliens",
                           native_col = "natives",
                           native_pool_size = 600,
                           priors = example_priors,
                           chains = 1, parallel_chains = 3)
        )
    )
  )
  expect_s3_class(mod_output, c("CmdStanMCMC", "CmdStanFit", "R6"))
})

test_that("native_discovery needs all argument", {
  expect_error(native_discovery(time_col = "time",
                                alien_col = "aliens",
                                native_col = "natives",
                                native_pool_size = 600,
                                priors = example_priors,
                                chains = 3, parallel_chains = 3))
  expect_error(native_discovery(data = medfish,
                                alien_col = "aliens",
                                native_col = "natives",
                                native_pool_size = 600,
                                priors = example_priors,
                                chains = 3, parallel_chains = 3))
  expect_error(native_discovery(data = medfish,
                                time_col = "time",
                                native_col = "natives",
                                native_pool_size = 600,
                                priors = example_priors,
                                chains = 3, parallel_chains = 3))
  expect_error(native_discovery(data = medfish,
                                time_col = "time",
                                alien_col = "aliens",
                                native_pool_size = 600,
                                priors = example_priors,
                                chains = 3, parallel_chains = 3))
  expect_error(native_discovery(data = medfish,
                                time_col = "time",
                                alien_col = "aliens",
                                native_col = "natives",
                                priors = example_priors,
                                chains = 3, parallel_chains = 3))
  expect_error(native_discovery(data = medfish,
                                time_col = "time",
                                alien_col = "aliens",
                                native_col = "natives",
                                native_pool_size = 600,
                                chains = 3, parallel_chains = 3))
})
