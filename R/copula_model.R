# ============================================================
# Copula Model Functions
# ============================================================

prepare_copula_data <- function(model_data) {
  
  copula_data <- model_data %>%
    select(
      WTI_Return,
      Brent_Return,
      USD_Return,
      CPI_Inflation,
      GPR_Change
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(if_all(everything(), ~ is.finite(.))) %>%
    na.omit()
  
  sds <- sapply(copula_data, sd, na.rm = TRUE)
  copula_data <- copula_data[, sds > 1e-8]
  
  u_data <- pobs(as.matrix(copula_data))
  u_data <- pmin(pmax(u_data, EPSILON), 1 - EPSILON)
  
  stopifnot(all(is.finite(u_data)))
  stopifnot(all(u_data > 0 & u_data < 1))
  
  list(
    copula_data = copula_data,
    u_data = u_data,
    dim_copula = ncol(u_data)
  )
}

fit_gaussian_start <- function(u_data) {
  
  dim_copula <- ncol(u_data)
  
  normal_copula_model <- normalCopula(
    dim = dim_copula,
    dispstr = "un"
  )
  
  normal_fit <- fitCopula(
    normal_copula_model,
    data = u_data,
    method = "itau"
  )
  
  rho_start <- getSigma(normal_fit@copula)
  rho_start <- as.matrix(nearPD(rho_start, corr = TRUE)$mat)
  
  start_params <- P2p(rho_start)
  
  list(
    normal_fit = normal_fit,
    rho_start = rho_start,
    start_params = start_params
  )
}

fit_student_t_copula <- function(u_data, start_params, df_start = COPULA_DF_START) {
  
  dim_copula <- ncol(u_data)
  
  t_copula_model <- tCopula(
    dim = dim_copula,
    dispstr = "un",
    df = df_start,
    df.fixed = FALSE
  )
  
  t_fit <- fitCopula(
    t_copula_model,
    data = u_data,
    method = "mpl",
    start = c(start_params, df_start),
    optim.method = "BFGS"
  )
  
  return(t_fit)
}

simulate_copula_scenarios <- function(t_fit, copula_data, n_sims = N_SIM, seed = SEED) {
  
  set.seed(seed)
  
  u_sim <- rCopula(
    n_sims,
    t_fit@copula
  )
  
  sim_matrix <- matrix(
    NA_real_,
    nrow = n_sims,
    ncol = ncol(copula_data)
  )
  
  colnames(sim_matrix) <- colnames(copula_data)
  
  for (j in seq_along(copula_data)) {
    sim_matrix[, j] <- quantile(
      copula_data[[j]],
      probs = u_sim[, j],
      na.rm = TRUE,
      type = 8
    )
  }
  
  as.data.frame(sim_matrix)
}

copula_inputs <- prepare_copula_data(model_data)

copula_data <- copula_inputs$copula_data
u_data <- copula_inputs$u_data

gaussian_start <- fit_gaussian_start(u_data)

t_fit <- fit_student_t_copula(
  u_data = u_data,
  start_params = gaussian_start$start_params
)

summary(t_fit)

joint_sim_data <- simulate_copula_scenarios(
  t_fit = t_fit,
  copula_data = copula_data,
  n_sims = N_SIM,
  seed = SEED
)