#' The 'alien' package.
#'
#' @description Easily estimate the introduction rates of alien species given first records data.
#' It specializes in addressing the role of sampling on the pattern of discoveries,
#' thus providing better estimates than using Generalized Linear Models which assume
#' perfect immediate detection of newly introduced species.
#'
#' @docType package
#' @name alien-package
#' @aliases alien
#' @useDynLib alien, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version 2.32.6. https://mc-stan.org
#'
NULL
