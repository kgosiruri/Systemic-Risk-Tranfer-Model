#### Plotting Functions ####
kes_red      <- "#8B0000"
kes_red_2    <- "#B22222"
kes_red_3    <- "#D94A4A"
kes_dark     <- "#1C1C1C"
kes_grey     <- "#F4F4F4"
kes_midgrey  <- "#9E9E9E"
kes_gold     <- "#C9A227"


theme_kes <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", colour = kes_dark, size = base_size + 3),
      plot.subtitle = element_text(colour = "grey35", size = base_size),
      axis.title = element_text(face = "bold", colour = kes_dark),
      axis.text = element_text(colour = "grey25"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(colour = "grey45", size = base_size - 2)
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

#### Helper functions for Plottings ####

plot_wti_price <- function(risk_data) {
  ggplot(risk_data, aes(x = Date, y = WTI)) +
    geom_area(fill = kes_red_3, alpha = 0.18) +
    geom_line(linewidth = 0.75, colour = kes_red) +
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
    geom_area(fill = kes_red_3, alpha = 0.18) +
    geom_line(linewidth = 0.75, colour = kes_red) +
    labs(
      title = "Monthly Geopolitical Risk Index",
      subtitle = "Geopolitical stress is included as a macro-financial shock amplifier.",
      x = NULL,
      y = "GPR Index"
    ) +
    theme_kes()
}

plot_systemic_index <- function(systemic_plot_data, attachment, exhaustion) {
  ggplot(systemic_plot_data, aes(x = Systemic_Index)) +
    geom_histogram(aes(y = after_stat(density)), bins = 70,
                   fill = kes_red_3, colour = "white", alpha = 0.75) +
    geom_density(colour = kes_dark, linewidth = 0.9) +
    geom_vline(xintercept = attachment, linetype = "dashed", colour = kes_red) +
    geom_vline(xintercept = exhaustion, linetype = "dashed", colour = kes_dark) +
    labs(
      title = "Distribution of Simulated Systemic Risk Index",
      subtitle = "PCA-based index generated from Student-t copula simulations.",
      x = "Systemic Risk Index",
      y = "Density"
    ) +
    theme_kes()
}

plot_payout_curve <- function(curve_data, attachment, exhaustion, limit) {
  ggplot(curve_data, aes(x = Systemic_Index, y = Payout, colour = Structure)) +
    geom_line(linewidth = 1.25) +
    geom_vline(xintercept = attachment, linetype = "dashed", colour = kes_red) +
    geom_vline(xintercept = exhaustion, linetype = "dashed", colour = kes_dark) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = "Payout Structure Comparison",
      subtitle = "Deterministic mapping from systemic stress to ILS payout.",
      x = "Systemic Risk Index",
      y = "Payout",
      colour = "Structure"
    ) +
    theme_kes()
}

plot_pricing_summary <- function(pricing_data) {
  ggplot(pricing_data, aes(x = Metric, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
    scale_fill_manual(values = c(kes_red, kes_dark, KES_BLUE)) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = "ILS Pricing Summary",
      subtitle = "Key pricing metrics for the ILS structure.",
      x = NULL,
      y = "Value"
    ) +
    theme_kes()
}

plot_basis_risk <- function(basis_data) {
  ggplot(basis_data, aes(x = Systemic_Index, y = Basis_Risk)) +
    geom_line(linewidth = 1.25, colour = kes_red) +
    labs(
      title = "Basis Risk Profile",
      subtitle = "Difference between sponsor loss and ILS payout across systemic stress levels.",
      x = "Systemic Risk Index",
      y = "Basis Risk"
    ) +
    theme_kes()
}

plot_evt_tail <- function(evt_data) {
  ggplot(evt_data, aes(x = Systemic_Index)) +
    geom_histogram(aes(y = after_stat(density)), bins = 70,
                   fill = kes_red_3, colour = "white", alpha = 0.75) +
    geom_density(colour = kes_dark, linewidth = 0.9) +
    labs(
      title = "Tail Distribution of Systemic Risk Index",
      subtitle = "Focus on extreme values to assess tail risk.",
      x = "Systemic Risk Index",
      y = "Density"
    ) +
    theme_kes()
}

plot_stress_scenarios <- function(stress_data) {
  ggplot(stress_data, aes(x = Systemic_Index, y = Payout, colour = Scenario)) +
    geom_line(linewidth = 1.25) +
    labs(
      title = "Stress Scenario Payout Curves",
      subtitle = "Payout profiles under different macro-financial stress scenarios.",
      x = "Systemic Risk Index",
      y = "Payout",
      colour = "Scenario"
    ) +
    theme_kes()
}

p1 <- plot_wti_price(risk_data)
save_plot(p1, "wti_price.png", 10, 5)

p2 <- plot_gpr_index(risk_data)
save_plot(p2, "gpr_index.png", 10, 5)

p3 <- plot_systemic_index(
  systemic_plot_data,
  attachment_systemic,
  exhaustion_systemic
)
save_plot(p3, "sim_risk_index.png", 12, 5)

p4 <- plot_payout_curve(
  curve_data,
  attachment_systemic,
  exhaustion_systemic,
  LIMIT
)
save_plot(p4, "pay_curve_comp.png", 12, 5)

p5 <- plot_pricing_summary(pricing_summary_data)
save_plot(p5, "pricing_summary.png", 8, 5)

p6 <- plot_basis_risk(basis_risk_data)
save_plot(p6, "basis_risk.png", 10, 5)

p7 <- plot_evt_tail(evt_data)
save_plot(p7, "evt_tail.png", 10, 5)

p8 <- plot_stress_scenarios(stress_data)
save_plot(p8, "stress_scenarios.png", 12, 5)

