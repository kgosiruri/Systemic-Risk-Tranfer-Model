# ============================================================
# Plotting Functions
# Systemic Risk Transfer via ILS
# ============================================================

# ------------------------------------------------------------
# Theme and save helper
# ------------------------------------------------------------

theme_kes <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold",
        colour = KES_DARK,
        size = base_size + 3
      ),
      plot.subtitle = element_text(
        colour = "grey35",
        size = base_size
      ),
      axis.title = element_text(
        face = "bold",
        colour = KES_DARK
      ),
      axis.text = element_text(colour = "grey25"),
      strip.text = element_text(face = "bold", colour = "white"),
      strip.background = element_rect(fill = KES_RED, colour = NA),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(
        colour = "grey45",
        size = base_size - 2
      )
    )
}

save_plot <- function(plot, filename, width = 10, height = 5) {
  ggsave(
    filename = file.path(FIGURES_DIR, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 1200,
    bg = "white"
  )
}

# ------------------------------------------------------------
# 1. Raw data plots
# ------------------------------------------------------------

plot_wti_price <- function(risk_data) {
  ggplot(risk_data, aes(x = Date, y = WTI)) +
    geom_area(fill = KES_RED_3, alpha = 0.18) +
    geom_line(linewidth = 0.75, colour = KES_RED) +
    labs(
      title = "Monthly WTI Crude Oil Price",
      subtitle = "Oil prices are used as a core transmission channel for energy-linked systemic stress.",
      x = NULL,
      y = "WTI Price"
    ) +
    theme_kes()
}

plot_gpr_index <- function(risk_data) {
  ggplot(risk_data, aes(x = Date, y = GPR)) +
    geom_area(fill = KES_RED_3, alpha = 0.18) +
    geom_line(linewidth = 0.75, colour = KES_RED) +
    labs(
      title = "Monthly Geopolitical Risk Index",
      subtitle = "Geopolitical stress is included as a macro-financial shock amplifier.",
      x = NULL,
      y = "GPR Index"
    ) +
    theme_kes()
}

plot_risk_factor_returns <- function(risk_factor_long) {
  ggplot(risk_factor_long, aes(x = Date, y = Value)) +
    geom_line(linewidth = 0.45, colour = KES_RED) +
    facet_wrap(~ Risk_Factor, scales = "free_y", ncol = 2) +
    labs(
      title = "Monthly Risk Factor Returns and Changes",
      subtitle = "Risk factors are transformed into monthly log returns or log changes before dependence modelling.",
      x = NULL,
      y = "Return / Change"
    ) +
    theme_kes()
}

# ------------------------------------------------------------
# 2. Dependence plots
# ------------------------------------------------------------

plot_correlation_heatmap <- function(cor_data) {
  ggplot(cor_data, aes(x = Factor_1, y = Factor_2, fill = Correlation)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(
      aes(label = round(Correlation, 2)),
      colour = KES_DARK,
      size = 3.2,
      fontface = "bold"
    ) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = KES_RED,
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    labs(
      title = "Correlation Matrix of Systemic Risk Factors",
      subtitle = "Linear dependence provides a first diagnostic before copula modelling.",
      x = NULL,
      y = NULL,
      fill = "Correlation"
    ) +
    theme_kes() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

plot_copula_density <- function(scatter_sample, xvar, yvar, title, xlab, ylab) {
  ggplot(scatter_sample, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    stat_density_2d_filled(
      contour_var = "ndensity",
      alpha = 0.9,
      show.legend = FALSE
    ) +
    stat_density_2d(
      colour = "white",
      linewidth = 0.25,
      alpha = 0.85
    ) +
    scale_fill_viridis_d(option = "magma", direction = -1) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_kes(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      legend.position = "none"
    )
}

# ------------------------------------------------------------
# 3. Systemic index plots
# ------------------------------------------------------------

plot_systemic_index <- function(systemic_plot_data, attachment, exhaustion) {
  ggplot(systemic_plot_data, aes(x = Systemic_Index)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 70,
      fill = KES_RED_3,
      colour = "white",
      alpha = 0.75
    ) +
    geom_density(colour = KES_DARK, linewidth = 0.9) +
    geom_vline(xintercept = attachment, linetype = "dashed", colour = KES_RED, linewidth = 0.9) +
    geom_vline(xintercept = exhaustion, linetype = "dashed", colour = KES_DARK, linewidth = 0.9) +
    labs(
      title = "Distribution of Simulated Systemic Risk Index",
      subtitle = "PCA-based index generated from Student-t copula simulations.",
      x = "Systemic Risk Index",
      y = "Density"
    ) +
    theme_kes()
}

plot_ranked_systemic_index <- function(systemic_plot_data, attachment, exhaustion) {
  index_path_data <- systemic_plot_data %>%
    arrange(Systemic_Index) %>%
    mutate(Rank = dplyr::row_number())
  
  ggplot(index_path_data, aes(x = Rank, y = Systemic_Index)) +
    geom_line(colour = KES_RED, linewidth = 0.8) +
    geom_hline(yintercept = attachment, linetype = "dashed", colour = KES_RED_2, linewidth = 0.9) +
    geom_hline(yintercept = exhaustion, linetype = "dashed", colour = KES_DARK, linewidth = 0.9) +
    labs(
      title = "Ranked Simulated Systemic Risk Index",
      subtitle = "Attachment and exhaustion points define the ILS risk layer.",
      x = "Simulation Rank",
      y = "Systemic Risk Index"
    ) +
    theme_kes()
}

plot_component_scores <- function(score_long) {
  ggplot(score_long, aes(x = Value, y = Score, fill = Score)) +
    geom_density_ridges(alpha = 0.85, colour = "white", scale = 1.35) +
    scale_fill_manual(
      values = rep(c(KES_RED, KES_RED_2, KES_RED_3, KES_DARK, KES_GOLD), 2)
    ) +
    labs(
      title = "Distribution of Standardised Component Scores",
      subtitle = "Standardised simulated factors used to construct the PCA systemic index.",
      x = "Standardised Score",
      y = NULL
    ) +
    theme_kes() +
    theme(legend.position = "none")
}

plot_pca_loadings <- function(pca_weights) {
  data.frame(
    Factor = names(pca_weights),
    Weight = as.numeric(pca_weights)
  ) %>%
    ggplot(aes(x = forcats::fct_reorder(Factor, Weight), y = Weight, fill = Weight > 0)) +
    geom_col(width = 0.65) +
    geom_hline(yintercept = 0, colour = KES_DARK, linewidth = 0.4) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = KES_RED, "FALSE" = KES_DARK)) +
    labs(
      title = "PCA Loadings for the Systemic Risk Index",
      subtitle = "Positive and negative weights show each factor's contribution to systemic stress.",
      x = NULL,
      y = "PCA Loading"
    ) +
    theme_kes() +
    theme(legend.position = "none")
}

# ------------------------------------------------------------
# 4. Payout plots
# ------------------------------------------------------------

plot_positive_payouts <- function(systemic_plot_data) {
  systemic_plot_data %>%
    dplyr::filter(Systemic_Payout > 0) %>%
    ggplot(aes(x = Systemic_Payout)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 60,
      fill = KES_RED_3,
      colour = "white",
      alpha = 0.75
    ) +
    geom_density(colour = KES_DARK, linewidth = 0.9) +
    scale_x_continuous(labels = scales::dollar) +
    labs(
      title = "Distribution of Positive Systemic ILS Payouts",
      subtitle = "Conditional distribution given trigger activation.",
      x = "Positive Payout",
      y = "Density"
    ) +
    theme_kes()
}

plot_index_vs_payout <- function(scatter_sample, attachment, exhaustion) {
  ggplot(scatter_sample, aes(x = Systemic_Index, y = Systemic_Payout)) +
    stat_density_2d_filled(
      aes(fill = after_stat(level)),
      alpha = 0.88,
      contour_var = "ndensity"
    ) +
    geom_vline(xintercept = attachment, linetype = "dashed", colour = KES_RED, linewidth = 0.9) +
    geom_vline(xintercept = exhaustion, linetype = "dashed", colour = KES_DARK, linewidth = 0.9) +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_viridis_d(option = "magma", direction = -1) +
    labs(
      title = "Systemic Risk Index vs ILS Payout",
      subtitle = "The payout layer activates at attachment and reaches the full limit at exhaustion.",
      x = "Systemic Risk Index",
      y = "ILS Payout",
      fill = "Density"
    ) +
    theme_kes()
}

plot_payout_curve <- function(curve_data, attachment, exhaustion, limit) {
  ggplot(curve_data, aes(x = Systemic_Index, y = Payout, colour = Structure)) +
    geom_line(linewidth = 1.25) +
    geom_vline(xintercept = attachment, linetype = "dashed", colour = KES_RED, linewidth = 0.85) +
    geom_vline(xintercept = exhaustion, linetype = "dashed", colour = KES_DARK, linewidth = 0.85) +
    scale_y_continuous(labels = scales::dollar) +
    scale_colour_manual(values = c(
      "General Kernel" = KES_DARK,
      "Binary Trigger" = KES_RED,
      "Linear Excess" = KES_RED_3,
      "Persistence Adjusted" = KES_GOLD
    )) +
    labs(
      title = "Payout Structure Comparison",
      subtitle = "Deterministic mapping from systemic stress to ILS payout.",
      x = "Systemic Risk Index",
      y = "Payout",
      colour = "Structure"
    ) +
    theme_kes()
}

plot_payout_distribution_by_structure <- function(joint_sim_data) {
  payout_long <- joint_sim_data %>%
    dplyr::select(
      General_Payout,
      Binary_Payout,
      Linear_Excess_Payout,
      Persistence_Payout
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Structure",
      values_to = "Payout"
    ) %>%
    dplyr::mutate(
      Structure = dplyr::recode(
        Structure,
        "General_Payout" = "General Kernel",
        "Binary_Payout" = "Binary Trigger",
        "Linear_Excess_Payout" = "Linear Excess",
        "Persistence_Payout" = "Persistence Adjusted"
      )
    )
  
  payout_long %>%
    dplyr::filter(Payout > 0) %>%
    ggplot(aes(x = Payout, fill = Structure)) +
    geom_density(alpha = 0.45, linewidth = 0.85) +
    scale_x_continuous(labels = scales::dollar) +
    scale_fill_manual(values = c(
      "General Kernel" = KES_DARK,
      "Binary Trigger" = KES_RED,
      "Linear Excess" = KES_RED_3,
      "Persistence Adjusted" = KES_GOLD
    )) +
    labs(
      title = "Conditional Distribution of ILS Payouts by Structure",
      subtitle = "Positive payouts only, conditional on trigger activation.",
      x = "Payout",
      y = "Density",
      fill = "Structure"
    ) +
    theme_kes()
}

# ------------------------------------------------------------
# 5. Pricing plots
# ------------------------------------------------------------

plot_expected_loss_ratio <- function(payout_comparison) {
  ggplot(
    payout_comparison,
    aes(
      x = forcats::fct_reorder(Model, Expected_Loss_Ratio),
      y = Expected_Loss_Ratio
    )
  ) +
    geom_col(fill = KES_RED, alpha = 0.9, width = 0.65) +
    geom_text(
      aes(label = scales::percent(Expected_Loss_Ratio, accuracy = 0.1)),
      hjust = -0.1,
      fontface = "bold",
      colour = KES_DARK,
      size = 3.4
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = "Expected Loss Ratio by Payout Structure",
      subtitle = "Higher expected loss implies a higher required investor spread.",
      x = NULL,
      y = "Expected Loss Ratio"
    ) +
    theme_kes()
}

plot_trigger_counts <- function(trigger_data) {
  ggplot(trigger_data, aes(x = Status, y = n, fill = Status)) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(
      aes(label = scales::comma(n)),
      vjust = -0.35,
      fontface = "bold"
    ) +
    scale_fill_manual(values = c(
      "Not Triggered" = KES_MIDGREY,
      "Triggered" = KES_RED
    )) +
    labs(
      title = "Triggered vs Non-Triggered Simulations",
      subtitle = "Trigger frequency is determined by the attachment threshold.",
      x = NULL,
      y = "Number of Simulations"
    ) +
    theme_kes() +
    theme(legend.position = "none")
}

plot_full_payout_counts <- function(full_payout_data) {
  ggplot(full_payout_data, aes(x = Status, y = n, fill = Status)) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(
      aes(label = scales::comma(n)),
      vjust = -0.35,
      fontface = "bold"
    ) +
    scale_fill_manual(values = c(
      "Not Full Payout" = KES_MIDGREY,
      "Full Payout" = KES_RED
    )) +
    labs(
      title = "Full Payout vs Non-Full Payout Simulations",
      subtitle = "Full payout frequency is controlled by the exhaustion threshold.",
      x = NULL,
      y = "Number of Simulations"
    ) +
    theme_kes() +
    theme(legend.position = "none")
}

# ------------------------------------------------------------
# 6. Basis risk plots
# ------------------------------------------------------------

plot_basis_risk_distribution <- function(joint_sim_data) {
  ggplot(joint_sim_data, aes(x = Basis_Risk)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 70,
      fill = KES_RED_3,
      colour = "white",
      alpha = 0.75
    ) +
    geom_density(colour = KES_DARK, linewidth = 0.9) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = KES_RED, linewidth = 0.9) +
    scale_x_continuous(labels = scales::dollar) +
    labs(
      title = "Distribution of Basis Risk",
      subtitle = "Basis risk is measured as Sponsor Loss minus ILS Payout.",
      x = "Basis Risk",
      y = "Density"
    ) +
    theme_kes()
}

plot_sponsor_loss_vs_payout <- function(joint_sim_data) {
  joint_sim_data %>%
    dplyr::slice_sample(n = min(50000, nrow(joint_sim_data))) %>%
    ggplot(aes(x = Sponsor_Loss, y = ILS_Payout)) +
    stat_density_2d_filled(contour_var = "ndensity", alpha = 0.9) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "white",
      linewidth = 0.9
    ) +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_viridis_d(option = "magma", direction = -1) +
    labs(
      title = "Sponsor Loss vs ILS Payout",
      subtitle = "The dashed line indicates perfect indemnification.",
      x = "Sponsor Loss",
      y = "ILS Payout",
      fill = "Density"
    ) +
    theme_kes()
}

p_basis_compare_dist <- basis_compare %>%
  sample_n(min(100000, nrow(.))) %>%
  ggplot(aes(x = Basis_Risk, fill = Structure)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Basis Risk Distribution by Payout Structure",
    subtitle = "Basis Risk = Sponsor Loss - ILS Payout",
    x = "Basis Risk",
    y = "Density",
    fill = "Payout Structure"
  ) +
  theme_kes()


p_basis_bar <- basis_compare_summary %>%
  select(Structure, Mean_Absolute_Basis_Risk, RMSE_Basis_Risk) %>%
  pivot_longer(
    cols = -Structure,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = reorder(Structure, Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Basis Risk Metrics by Payout Structure",
    subtitle = "Lower values indicate better hedge performance",
    x = NULL,
    y = "Basis Risk",
    fill = "Metric"
  ) +
  theme_kes()

p_underpayment <- basis_compare_summary %>%
  ggplot(aes(x = reorder(Structure, Probability_Underpayment),
             y = Probability_Underpayment,
             fill = Structure)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(Probability_Underpayment, accuracy = 0.1)),
    hjust = -0.1,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Probability of Sponsor Underpayment",
    subtitle = "Share of simulations where Sponsor Loss exceeds ILS Payout",
    x = NULL,
    y = "Probability of Underpayment"
  ) +
  theme_kes() +
  theme(legend.position = "none")

# ------------------------------------------------------------
# 7. Stress and sensitivity plots
# ------------------------------------------------------------

plot_stress_scenarios <- function(stress_scenarios) {
  ggplot(
    stress_scenarios,
    aes(
      x = forcats::fct_reorder(Scenario, Payout),
      y = Payout,
      fill = Scenario
    )
  ) +
    geom_col(width = 0.65, alpha = 0.9) +
    geom_text(
      aes(label = scales::dollar(Payout)),
      hjust = -0.05,
      fontface = "bold",
      size = 3.3
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::dollar,
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_fill_manual(values = c(KES_MIDGREY, KES_RED_3, KES_RED_2, KES_RED)) +
    labs(
      title = "Stress Scenario Payouts",
      subtitle = "Payouts increase as the systemic index moves from attachment to exhaustion.",
      x = NULL,
      y = "Payout"
    ) +
    theme_kes() +
    theme(legend.position = "none")
}

plot_sensitivity_heatmap <- function(sensitivity_results) {
  sensitivity_results %>%
    ggplot(
      aes(
        x = factor(Exhaustion_Percentile),
        y = factor(Attachment_Percentile),
        fill = Investor_Spread
      )
    ) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(
      aes(label = scales::percent(Investor_Spread, accuracy = 0.1)),
      colour = "white",
      fontface = "bold",
      size = 3.3
    ) +
    scale_fill_gradient(low = KES_RED_3, high = KES_DARK) +
    labs(
      title = "Sensitivity of Investor Spread to Trigger Calibration",
      subtitle = "Lower attachment and exhaustion thresholds increase expected loss and investor spread.",
      x = "Exhaustion Percentile",
      y = "Attachment Percentile",
      fill = "Investor Spread"
    ) +
    theme_kes()
}

# ------------------------------------------------------------
# 8. EVT plot
# ------------------------------------------------------------

plot_evt_exceedances <- function(exceedance_data) {
  ggplot(exceedance_data, aes(x = Exceedance)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 60,
      fill = KES_RED_3,
      colour = "white",
      alpha = 0.75
    ) +
    geom_density(colour = KES_DARK, linewidth = 0.9) +
    labs(
      title = "Extreme Value Tail Exceedances",
      subtitle = "Exceedances above the 95th percentile threshold of the systemic risk index.",
      x = "Exceedance Above Threshold",
      y = "Density"
    ) +
    theme_kes()
}