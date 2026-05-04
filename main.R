# ============================================================
# Main Pipeline
# Systemic Risk Transfer via Insurance-Linked Securities
# ============================================================

# ------------------------------------------------------------
# 0. Load global settings and functions
# ------------------------------------------------------------

source("global.R")

source("R/data_retrieval.R")
source("R/data_processing.R")
source("R/copula_model.R")
source("R/systemic_index.R")
source("R/payout_kernel.R")
source("R/pricing_summary.R")
source("R/basis_risk.R")
source("R/evt_tail_model.R")
source("R/stress_scenarios.R")
source("R/plotting.R")

# ------------------------------------------------------------
# 1. Retrieve / import raw data
# ------------------------------------------------------------

raw_data <- retrieve_risk_data(
  start_date = START_DATE,
  end_date = END_DATE,
  gpr_file = GPR_FILE
)

wti_fred   <- raw_data$wti_fred
brent_fred <- raw_data$brent_fred
cpi_fred   <- raw_data$cpi_fred
usd_fred   <- raw_data$usd_fred
gpr        <- raw_data$gpr

# ------------------------------------------------------------
# 2. Process and merge monthly risk factors
# ------------------------------------------------------------

risk_data <- build_monthly_risk_dataset(
  wti_fred = wti_fred,
  brent_fred = brent_fred,
  cpi_fred = cpi_fred,
  usd_fred = usd_fred,
  gpr = gpr
)

model_data <- create_model_returns(risk_data)

write.csv(risk_data, file.path(PROCESSED_DIR, "risk_data.csv"), row.names = FALSE)
write.csv(model_data, file.path(PROCESSED_DIR, "model_data.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 3. Prepare copula data
# ------------------------------------------------------------

copula_inputs <- prepare_copula_data(model_data)

copula_data <- copula_inputs$copula_data
u_data <- copula_inputs$u_data
dim_copula <- copula_inputs$dim_copula

# ------------------------------------------------------------
# 4. Fit copula model
# ------------------------------------------------------------

gaussian_start <- fit_gaussian_start(u_data)

t_fit <- fit_student_t_copula(
  u_data = u_data,
  start_params = gaussian_start$start_params,
  df_start = COPULA_DF_START
)

print(summary(t_fit))

# ------------------------------------------------------------
# 5. Simulate joint risk scenarios
# ------------------------------------------------------------

joint_sim_data <- simulate_copula_scenarios(
  t_fit = t_fit,
  copula_data = copula_data,
  n_sims = N_SIM,
  seed = SEED
)

# Save sample only, not full 10m simulations
write.csv(
  head(joint_sim_data, 10000),
  file.path(SIM_DATA_DIR, "joint_sim_data_sample.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. Build PCA systemic risk index
# ------------------------------------------------------------

systemic_results <- build_systemic_index(
  copula_data = copula_data,
  joint_sim_data = joint_sim_data
)

joint_sim_data <- systemic_results$joint_sim_data
pca_fit <- systemic_results$pca_fit
pca_weights <- systemic_results$pca_weights
sim_scaled <- systemic_results$sim_scaled

print(pca_weights)

# ------------------------------------------------------------
# 7. Define ILS trigger points
# ------------------------------------------------------------

attachment_systemic <- quantile(
  joint_sim_data$Systemic_Index,
  probs = ATTACHMENT_Q,
  na.rm = TRUE
)

exhaustion_systemic <- quantile(
  joint_sim_data$Systemic_Index,
  probs = EXHAUSTION_Q,
  na.rm = TRUE
)

alpha_systemic <- LIMIT / as.numeric(exhaustion_systemic - attachment_systemic)

trigger_points <- data.frame(
  Metric = c(
    "Attachment Point",
    "Exhaustion Point",
    "Contract Limit",
    "Notional",
    "Risk Load",
    "Alpha Scaling"
  ),
  Value = c(
    as.numeric(attachment_systemic),
    as.numeric(exhaustion_systemic),
    LIMIT,
    NOTIONAL,
    RISK_LOAD,
    alpha_systemic
  )
)

print(trigger_points)

# ------------------------------------------------------------
# 8. Calculate ILS payouts
# ------------------------------------------------------------

systemic_payouts <- calc_ils_payout(
  index = joint_sim_data$Systemic_Index,
  type = "general",
  A = attachment_systemic,
  E = exhaustion_systemic,
  L = LIMIT
)

binary_payouts <- calc_ils_payout(
  index = joint_sim_data$Systemic_Index,
  type = "binary",
  A = attachment_systemic,
  L = LIMIT
)

linear_excess_payouts <- calc_ils_payout(
  index = joint_sim_data$Systemic_Index,
  type = "linear_excess",
  A = attachment_systemic,
  L = LIMIT,
  alpha = alpha_systemic
)

persistence_payouts <- calc_ils_payout(
  index = joint_sim_data$Systemic_Index,
  type = "persistence",
  A = attachment_systemic,
  L = LIMIT,
  alpha = alpha_systemic,
  d = 3,
  n = 1
)

joint_sim_data <- joint_sim_data %>%
  mutate(
    General_Payout = systemic_payouts,
    Binary_Payout = binary_payouts,
    Linear_Excess_Payout = linear_excess_payouts,
    Persistence_Payout = persistence_payouts
  )

# ------------------------------------------------------------
# 9. Pricing summaries
# ------------------------------------------------------------

payout_comparison <- bind_rows(
  summarise_payout(
    payouts = systemic_payouts,
    model_name = "General Kernel",
    notional = NOTIONAL,
    risk_load = RISK_LOAD,
    limit = LIMIT
  ),
  summarise_payout(
    payouts = binary_payouts,
    model_name = "Binary Trigger",
    notional = NOTIONAL,
    risk_load = RISK_LOAD,
    limit = LIMIT
  ),
  summarise_payout(
    payouts = linear_excess_payouts,
    model_name = "Linear Excess",
    notional = NOTIONAL,
    risk_load = RISK_LOAD,
    limit = LIMIT
  ),
  summarise_payout(
    payouts = persistence_payouts,
    model_name = "Persistence Adjusted",
    notional = NOTIONAL,
    risk_load = RISK_LOAD,
    limit = LIMIT
  )
)

print(payout_comparison)

write.csv(
  payout_comparison,
  file.path(TABLES_DIR, "payout_comparison.csv"),
  row.names = FALSE
)

write.csv(
  trigger_points,
  file.path(TABLES_DIR, "trigger_points.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 10. Sponsor loss and basis risk
# ------------------------------------------------------------

basis_results <- calculate_basis_risk(
  joint_sim_data = joint_sim_data,
  ils_payouts = systemic_payouts,
  exhaustion = exhaustion_systemic,
  loss_threshold_q = SPONSOR_TRIGGER_Q,
  max_sponsor_loss = SPONSOR_LOSS_CAP
)

joint_sim_data <- basis_results$joint_sim_data
basis_risk_summary <- basis_results$basis_risk_summary

print(basis_risk_summary)

write.csv(
  basis_risk_summary,
  file.path(TABLES_DIR, "basis_risk_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. EVT tail model
# ------------------------------------------------------------

evt_results <- fit_evt_tail(
  systemic_index = joint_sim_data$Systemic_Index,
  threshold_q = 0.95
)

evt_fit <- evt_results$fit
gpd_threshold <- evt_results$threshold

print(evt_fit)

evt_summary <- summarise_evt_tail(
  evt_fit = evt_fit,
  threshold = gpd_threshold
)

exceedance_data <- prepare_evt_exceedances(
  systemic_index = joint_sim_data$Systemic_Index,
  threshold = gpd_threshold
)

print(evt_summary)

write.csv(
  evt_summary,
  file.path(TABLES_DIR, "evt_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 12. Stress scenarios and sensitivity analysis
# ------------------------------------------------------------

stress_scenarios <- build_stress_scenarios(
  systemic_index = joint_sim_data$Systemic_Index,
  attachment = attachment_systemic,
  exhaustion = exhaustion_systemic,
  limit = LIMIT
)

print(stress_scenarios)

sensitivity_results <- run_trigger_sensitivity(
  systemic_index = joint_sim_data$Systemic_Index,
  attachment_grid = c(0.85, 0.90, 0.95),
  exhaustion_grid = c(0.975, 0.99, 0.995),
  limit = LIMIT,
  notional = NOTIONAL,
  risk_load = RISK_LOAD
)

print(sensitivity_results)

write.csv(
  stress_scenarios,
  file.path(TABLES_DIR, "stress_scenarios.csv"),
  row.names = FALSE
)

write.csv(
  sensitivity_results,
  file.path(TABLES_DIR, "sensitivity_results.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 13. Prepare plotting datasets
# ------------------------------------------------------------

systemic_plot_data <- joint_sim_data %>%
  mutate(
    Systemic_Payout = General_Payout,
    Triggered = Systemic_Payout > 0,
    Full_Payout = Systemic_Payout >= LIMIT
  )

risk_factor_long <- model_data %>%
  pivot_longer(
    cols = -Date,
    names_to = "Risk_Factor",
    values_to = "Value"
  )

cor_matrix <- cor(copula_data, use = "complete.obs")

cor_data <- as.data.frame(as.table(cor_matrix)) %>%
  rename(
    Factor_1 = Var1,
    Factor_2 = Var2,
    Correlation = Freq
  )

set.seed(SEED)

scatter_sample <- systemic_plot_data %>%
  slice_sample(n = min(50000, nrow(systemic_plot_data)))

score_long <- systemic_plot_data %>%
  select(WTI_Score, Brent_Score, USD_Score, Inflation_Score, GPR_Score) %>%
  slice_sample(n = min(50000, nrow(systemic_plot_data))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Score",
    values_to = "Value"
  )

trigger_data <- systemic_plot_data %>%
  count(Triggered) %>%
  mutate(Status = ifelse(Triggered, "Triggered", "Not Triggered"))

full_payout_data <- systemic_plot_data %>%
  count(Full_Payout) %>%
  mutate(Status = ifelse(Full_Payout, "Full Payout", "Not Full Payout"))

curve_data <- build_payout_curve_data(
  attachment = attachment_systemic,
  exhaustion = exhaustion_systemic,
  limit = LIMIT
)

# ------------------------------------------------------------
# 14. Generate and save plots
# ------------------------------------------------------------

p_wti <- plot_wti_price(risk_data)
save_plot(p_wti, "wti_price.png", 10, 5)

p_gpr <- plot_gpr_index(risk_data)
save_plot(p_gpr, "gpr_index.png", 10, 5)

p_returns <- plot_risk_factor_returns(risk_factor_long)
save_plot(p_returns, "risk_factor_returns.png", 12, 5)

p_corr <- plot_correlation_heatmap(cor_data)
save_plot(p_corr, "correlation_heatmap.png", 12, 5)

p_systemic <- plot_systemic_index(
  systemic_plot_data = systemic_plot_data,
  attachment = attachment_systemic,
  exhaustion = exhaustion_systemic
)
save_plot(p_systemic, "systemic_index_distribution.png", 12, 5)

p_ranked <- plot_ranked_systemic_index(
  systemic_plot_data = systemic_plot_data,
  attachment = attachment_systemic,
  exhaustion = exhaustion_systemic
)
save_plot(p_ranked, "ranked_systemic_index.png", 12, 5)

p_scores <- plot_component_scores(score_long)
save_plot(p_scores, "component_scores.png", 12, 5)

p_loadings <- plot_pca_loadings(pca_weights)
save_plot(p_loadings, "pca_loadings.png", 12, 5)

p_positive_payouts <- plot_positive_payouts(systemic_plot_data)
save_plot(p_positive_payouts, "positive_payout_distribution.png", 12, 4)

p_index_vs_payout <- plot_index_vs_payout(
  scatter_sample = scatter_sample,
  attachment = attachment_systemic,
  exhaustion = exhaustion_systemic
)
save_plot(p_index_vs_payout, "systemic_index_vs_payout.png", 12, 5)

p_payout_curve <- plot_payout_curve(
  curve_data = curve_data,
  attachment = attachment_systemic,
  exhaustion = exhaustion_systemic,
  limit = LIMIT
)
save_plot(p_payout_curve, "payout_curve_comparison.png", 12, 5)

p_elr <- plot_expected_loss_ratio(payout_comparison)
save_plot(p_elr, "expected_loss_ratio_by_structure.png", 12, 4)

p_trigger <- plot_trigger_counts(trigger_data)
save_plot(p_trigger, "triggered_vs_not_triggered.png", 7.5, 7.5)

p_full <- plot_full_payout_counts(full_payout_data)
save_plot(p_full, "full_payout_vs_not_full_payout.png", 7.5, 7.5)

p_basis_dist <- plot_basis_risk_distribution(joint_sim_data)
save_plot(p_basis_dist, "basis_risk_distribution.png", 12, 4)

p_sponsor_vs_payout <- plot_sponsor_loss_vs_payout(joint_sim_data)
save_plot(p_sponsor_vs_payout, "sponsor_loss_vs_ils_payout.png", 12, 6)

p_stress <- plot_stress_scenarios(stress_scenarios)
save_plot(p_stress, "stress_scenario_payouts.png", 12, 5)

p_sensitivity <- plot_sensitivity_heatmap(sensitivity_results)
save_plot(p_sensitivity, "sensitivity_spread_heatmap.png", 12, 4)

p_evt <- plot_evt_exceedances(exceedance_data)
save_plot(p_evt, "evt_exceedances.png", 12, 5)

# ------------------------------------------------------------
# 15. Print key plots to viewer
# ------------------------------------------------------------

p_wti
p_gpr
p_systemic
p_payout_curve
p_elr
p_basis_dist
p_evt

# ------------------------------------------------------------
# 16. End
# ------------------------------------------------------------

cat("\nPipeline complete.\n")
cat("Figures saved to:", FIGURES_DIR, "\n")
cat("Tables saved to:", TABLES_DIR, "\n")