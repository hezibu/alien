### Internal function for getting a matrix of values of
### exp(b0 + b1*x1 + b2x2 + ... + bn*xn)

build_mu_function <- function(formula, data, beta){
  x <- stats::model.matrix(object = formula, data = data)

  if (length(beta) != ncol(x))
    cli::cli_abort("Supplied {length(beta)} initial value{?s} to {ncol(x)} predictor{?s}")
  N <- nrow(data)

  mu_response <- apply(x, MARGIN = 1, function(x_row) exp(sum(x_row * beta))) |>
    `names<-`(NULL)

  return(mu_response)

}

build_pi_function <- function(formula, data, gamma, growth_param = 0){
  x <- stats::model.matrix(object = formula, data = data)

  if (length(gamma) != ncol(x))
    cli::cli_abort("Supplied {length(gamma)} initial value{?s} to {ncol(x)} predictor{?s}")

  N <- nrow(data)

  pi_response_vectors <- apply(x, MARGIN = 1, function(x_row) sum(x_row * gamma)) |>
    `names<-`(NULL) |>
    rep(N)

  pi_response_vectors_m <- matrix(pi_response_vectors, nrow = N, ncol = N, byrow = T)

  dat <- rep(1:N, N)
  t_mat <- matrix(dat, nrow = N, ncol = N, byrow = T)
  t_minus_s_mat <- t_mat - t(t_mat)

  pi_st_matrix <- stats::plogis(pi_response_vectors_m + growth_param*exp(t_minus_s_mat))
  pi_st_matrix[lower.tri(pi_st_matrix, diag = F)] <- 0
  one_minus_pi_st_matrix <- 1 - pi_st_matrix
  products <- t(apply(one_minus_pi_st_matrix, MARGIN = 1, cumprod))

  pst_matrix <- matrix(0,
                       nrow = N,
                       ncol = N)
  for (t in 1:N){
    for (s in 1:t){
      pst_matrix[s, t] <- prod(pi_st_matrix[s,t],products[s,t-1])
    }
  }

  return(pst_matrix)

}

calculate_lambda <- function(mu, pi, data, beta, gamma, growth_param){

  mu_vector <- build_mu_function(formula = mu,
                                 data = data,
                                 beta = beta)
  pst_matrix <- build_pi_function(formula = pi,
                                  data = data,
                                  gamma = gamma,
                                  growth_param = growth_param)

  colSums(mu_vector * pst_matrix, na.rm = T)
}

snc_ll_function <- function(y, mu_formula, pi_formula, data, growth = T, x) {

  n_beta <- length(all.vars(mu_formula)) + 1
  n_gamma <- length(all.vars(pi_formula)) + 1

  beta <- x[1:n_beta]

  if (growth) {
    growth_param = x[length(x)]
    gamma <- x[(n_beta+1):(length(x)-1)]
  } else {
    growth_param = 0
    gamma <- x[(n_beta+1):length(x)]
  }

  lambda <-  calculate_lambda(mu = mu_formula,
                              pi = pi_formula,
                              data = data,
                              beta = beta,
                              gamma = gamma,
                              growth_param = growth_param)

  summand <- y*log(lambda) - lambda
  return(-sum(summand))
}
