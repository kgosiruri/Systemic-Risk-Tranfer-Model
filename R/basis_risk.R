# ============================================================
# Basis Risk Functions
# ============================================================

calculate_basis_risk <- function(joint_sim_data,
                                 ils_payouts,
                                 exhaustion,
                                 loss_threshold_q = SPONSOR_TRIGGER_Q,
                                 max_sponsor_loss = SPONSOR_LOSS_CAP) {
  
  loss_threshold <- quantile(
    joint_sim_data$Systemic_Index,
    probs = loss_threshold_q,
    na.rm = TRUE
  )
  
  joint_sim_data <- joint_sim_data %>%
    mutate(
      Sponsor_Loss = max_sponsor_loss *
        pmin(
          1,
          pmax(
            (Systemic_Index - loss_threshold) /
              (exhaustion - loss_threshold),
            0
          )
        ),
      
      ILS_Payout = ils_payouts,
      
      Basis_Risk = Sponsor_Loss - ILS_Payout
    )
  
  basis_risk_summary <- data.frame(
    Metric = c(
      "Loss Threshold",
      "Mean Sponsor Loss",
      "Mean ILS Payout",
      "Mean Basis Risk",
      "Probability of Underpayment",
      "Average Underpayment Given Underpayment",
      "Correlation: Sponsor Loss vs ILS Payout"
    ),
    Value = c(
      as.numeric(loss_threshold),
      mean(joint_sim_data$Sponsor_Loss, na.rm = TRUE),
      mean(joint_sim_data$ILS_Payout, na.rm = TRUE),
      mean(joint_sim_data$Basis_Risk, na.rm = TRUE),
      mean(joint_sim_data$Basis_Risk > 0, na.rm = TRUE),
      mean(joint_sim_data$Basis_Risk[joint_sim_data$Basis_Risk > 0], na.rm = TRUE),
      cor(
        joint_sim_data$Sponsor_Loss,
        joint_sim_data$ILS_Payout,
        use = "complete.obs"
      )
    )
  )
  
  list(
    joint_sim_data = joint_sim_data,
    basis_risk_summary = basis_risk_summary,
    loss_threshold = loss_threshold
  )
}
