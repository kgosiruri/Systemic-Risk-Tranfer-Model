# ============================================================
# Copula Model Functions
# ============================================================

prepare_copula_data <- function(model_data) {
  
  required_cols <- c(
    "WTI_Return",
    "Brent_Return",
    "USD_Return",
    "CPI_Inflation",
    "GPR_Change"
  )
  
  missing_cols <- setdiff(required_cols, names(model_data))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  copula_data <- model_data %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ is.finite(.))) %>%
    tidyr::drop_na()
  
  if (nrow(copula_data) < 30) {
    stop(
      paste(
        "Not enough valid observations for copula fitting.",
        "Rows available:",
        nrow(copula_data)
      )
    )
  }
  
  sds <- sapply(copula_data, stats::sd, na.rm = TRUE)
  
  valid_cols <- is.finite(sds) & !is.na(sds) & sds > 1e-8
  
  if (!any(valid_cols)) {
    stop(
      "No valid columns left after standard deviation check. Check model_data."
    )
  }
  
  copula_data <- copula_data[, valid_cols, drop = FALSE]
  
  u_data <- copula::pobs(as.matrix(copula_data))
  u_data <- pmin(pmax(u_data, EPSILON), 1 - EPSILON)
  
  stopifnot(all(is.finite(u_data)))
  stopifnot(all(u_data > 0 & u_data < 1))
  
  list(
    copula_data = copula_data,
    u_data = u_data,
    dim_copula = ncol(u_data),
    retained_variables = colnames(copula_data)
  )
}

fit_gaussian_start <- function(u_data) {
  
  dim_copula <- ncol(u_data)
  
  normal_copula_model <- copula::normalCopula(
    dim = dim_copula,
    dispstr = "un"
  )
  
  normal_fit <- copula::fitCopula(
    normal_copula_model,
    data = u_data,
    method = "itau"
  )
  
  rho_start <- copula::getSigma(normal_fit@copula)
  rho_start <- as.matrix(Matrix::nearPD(rho_start, corr = TRUE)$mat)
  
  start_params <- copula::P2p(rho_start)
  
  list(
    normal_fit = normal_fit,
    rho_start = rho_start,
    start_params = start_params
  )
}

fit_student_t_copula <- function(u_data,
                                 start_params,
                                 df_start = COPULA_DF_START) {
  
  dim_copula <- ncol(u_data)
  
  t_copula_model <- copula::tCopula(
    dim = dim_copula,
    dispstr = "un",
    df = df_start,
    df.fixed = FALSE
  )
  
  t_fit <- copula::fitCopula(
    t_copula_model,
    data = u_data,
    method = "mpl",
    start = c(start_params, df_start),
    optim.method = "BFGS"
  )
  
  return(t_fit)
}

simulate_copula_scenarios <- function(t_fit,
                                      copula_data,
                                      n_sims = N_SIM,
                                      seed = SEED) {
  
  set.seed(seed)
  
  u_sim <- copula::rCopula(
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
    sim_matrix[, j] <- stats::quantile(
      copula_data[[j]],
      probs = u_sim[, j],
      na.rm = TRUE,
      type = 8
    )
  }
  
  as.data.frame(sim_matrix)
}