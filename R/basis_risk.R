# Basis Risk Functions

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

# Basis Risk Comparison Across Payout Structures

basis_compare <- joint_sim_data %>%
  mutate(
    General_Basis_Risk = Sponsor_Loss - General_Payout,
    Binary_Basis_Risk = Sponsor_Loss - Binary_Payout,
    Linear_Excess_Basis_Risk = Sponsor_Loss - Linear_Excess_Payout,
    Persistence_Basis_Risk = Sponsor_Loss - Persistence_Payout
  ) %>%
  select(
    Systemic_Index,
    Sponsor_Loss,
    General_Basis_Risk,
    Binary_Basis_Risk,
    Linear_Excess_Basis_Risk,
    Persistence_Basis_Risk
  ) %>%
  pivot_longer(
    cols = ends_with("Basis_Risk"),
    names_to = "Structure",
    values_to = "Basis_Risk"
  ) %>%
  mutate(
    Structure = recode(
      Structure,
      "General_Basis_Risk" = "General Kernel",
      "Binary_Basis_Risk" = "Binary Trigger",
      "Linear_Excess_Basis_Risk" = "Linear Excess",
      "Persistence_Basis_Risk" = "Persistence Adjusted"
    )
  )

basis_compare_summary <- basis_compare %>%
  group_by(Structure) %>%
  summarise(
    Mean_Basis_Risk = mean(Basis_Risk, na.rm = TRUE),
    Mean_Absolute_Basis_Risk = mean(abs(Basis_Risk), na.rm = TRUE),
    RMSE_Basis_Risk = sqrt(mean(Basis_Risk^2, na.rm = TRUE)),
    Probability_Underpayment = mean(Basis_Risk > 0, na.rm = TRUE),
    Average_Underpayment = mean(Basis_Risk[Basis_Risk > 0], na.rm = TRUE),
    VaR_95_Basis_Risk = quantile(Basis_Risk, 0.95, na.rm = TRUE),
    VaR_99_Basis_Risk = quantile(Basis_Risk, 0.99, na.rm = TRUE),
    .groups = "drop"
  )
