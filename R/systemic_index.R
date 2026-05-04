# ============================================================
# Systemic Risk Index Functions
# ============================================================

build_systemic_index <- function(copula_data, joint_sim_data) {
  
  # Historical means and standard deviations
  historical_means <- sapply(copula_data, mean, na.rm = TRUE)
  historical_sds <- sapply(copula_data, sd, na.rm = TRUE)
  
  # Standardise historical data before PCA
  copula_scaled <- scale(copula_data)
  
  # Fit PCA to historical standardised risk factors
  pca_fit <- prcomp(
    copula_scaled,
    center = FALSE,
    scale. = FALSE
  )
  
  # First principal component loadings
  pca_weights <- pca_fit$rotation[, 1]
  
  # Make positive oil shocks correspond to higher systemic stress
  if ("WTI_Return" %in% names(pca_weights)) {
    if (pca_weights["WTI_Return"] < 0) {
      pca_weights <- -pca_weights
    }
  }
  
  # Standardise simulated data using historical means and standard deviations
  sim_scaled <- sweep(joint_sim_data, 2, historical_means, "-")
  sim_scaled <- sweep(sim_scaled, 2, historical_sds, "/")
  
  # Create PCA-based systemic risk index
  joint_sim_data$Systemic_Index <- as.numeric(
    as.matrix(sim_scaled) %*% pca_weights
  )
  
  # Add individual standardised factor scores
  joint_sim_data <- joint_sim_data %>%
    mutate(
      WTI_Score = sim_scaled[, "WTI_Return"],
      Brent_Score = sim_scaled[, "Brent_Return"],
      USD_Score = sim_scaled[, "USD_Return"],
      Inflation_Score = sim_scaled[, "CPI_Inflation"],
      GPR_Score = sim_scaled[, "GPR_Change"]
    )
  
  list(
    joint_sim_data = joint_sim_data,
    pca_fit = pca_fit,
    pca_weights = pca_weights,
    sim_scaled = sim_scaled,
    historical_means = historical_means,
    historical_sds = historical_sds
  )
}
