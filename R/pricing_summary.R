#### Pricing Summary Functions ####

summarise_payout <- function(payouts, model_name, notional, risk_load, limit) {
  
  expected_payout <- mean(payouts, na.rm = TRUE)
  expected_loss_ratio <- expected_payout / notional
  
  investor_spread <- expected_loss_ratio + risk_load
  investor_expected_return <- investor_spread - expected_loss_ratio
  
  trigger_frequency <- mean(payouts > 0, na.rm = TRUE)
  full_payout_frequency <- mean(payouts >= limit, na.rm = TRUE)
  
  data.frame(
    Model = model_name,
    Expected_Payout = expected_payout,
    Expected_Loss_Ratio = expected_loss_ratio,
    Trigger_Frequency = trigger_frequency,
    Full_Payout_Frequency = full_payout_frequency,
    Investor_Spread = investor_spread,
    Investor_Expected_Return = investor_expected_return
  )
}

build_payout_comparison <- function(
  systemic_payouts,
  binary_payouts,
  linear_excess_payouts,
  persistence_payouts,
  notional,
  risk_load,
  limit
) {
  
  dplyr::bind_rows(
    
    summarise_payout(
      payouts = systemic_payouts,
      model_name = "General Kernel",
      notional = notional,
      risk_load = risk_load,
      limit = limit
    ),
    
    summarise_payout(
      payouts = binary_payouts,
      model_name = "Binary Trigger",
      notional = notional,
      risk_load = risk_load,
      limit = limit
    ),
    
    summarise_payout(
      payouts = linear_excess_payouts,
      model_name = "Linear Excess",
      notional = notional,
      risk_load = risk_load,
      limit = limit
    ),
    
    summarise_payout(
      payouts = persistence_payouts,
      model_name = "Persistence Adjusted",
      notional = notional,
      risk_load = risk_load,
      limit = limit
    )
  )
}

build_trigger_summary <- function(
  attachment,
  exhaustion,
  notional,
  limit,
  risk_load,
  alpha
) {
  
  data.frame(
    Metric = c(
      "Attachment Point",
      "Exhaustion Point",
      "Contract Limit",
      "Notional",
      "Risk Load",
      "Alpha Scaling"
    ),
    Value = c(
      as.numeric(attachment),
      as.numeric(exhaustion),
      limit,
      notional,
      risk_load,
      alpha
    )
  )
}

summarise_tail_metrics <- function(payouts, probs = c(0.95, 0.99)) {
  
  var_vals <- sapply(probs, function(p) quantile(payouts, p, na.rm = TRUE))
  
  tvar_vals <- sapply(probs, function(p) {
    threshold <- quantile(payouts, p, na.rm = TRUE)
    mean(payouts[payouts >= threshold], na.rm = TRUE)
  })
  
  data.frame(
    Metric = c(
      "Mean",
      paste0("VaR_", probs * 100),
      paste0("TVaR_", probs * 100),
      "Max"
    ),
    Value = c(
      mean(payouts, na.rm = TRUE),
      var_vals,
      tvar_vals,
      max(payouts, na.rm = TRUE)
    )
  )
}

calculate_payout_efficiency <- function(systemic_index, payouts) {
  
  cor(systemic_index, payouts, use = "complete.obs")
}

build_efficiency_table <- function(joint_sim_data) {
  
  data.frame(
    Structure = c(
      "General Kernel",
      "Binary Trigger",
      "Linear Excess",
      "Persistence Adjusted"
    ),
    Correlation = c(
      cor(joint_sim_data$Systemic_Index, joint_sim_data$General_Payout),
      cor(joint_sim_data$Systemic_Index, joint_sim_data$Binary_Payout),
      cor(joint_sim_data$Systemic_Index, joint_sim_data$Linear_Excess_Payout),
      cor(joint_sim_data$Systemic_Index, joint_sim_data$Persistence_Payout)
    )
  )
}

build_persistence_surface_data <- function(joint_sim_data, attachment, exhaustion, limit) {
  
  joint_sim_data %>%
    dplyr::mutate(
      Attachment_Scaled = pmax(0, pmin(1, (Systemic_Index - attachment) / (exhaustion - attachment))),
      Exhaustion_Scaled = pmax(0, pmin(1, (Systemic_Index - exhaustion) / (limit - exhaustion))),
      Persistence_Adjustment = 1 + Attachment_Scaled * (1 - Exhaustion_Scaled)
    ) %>%
    dplyr::select(Systemic_Index, Persistence_Adjustment)
}
