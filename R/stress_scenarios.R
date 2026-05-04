# ============================================================
# Stress Scenario Functions
# ============================================================

build_stress_scenarios <- function(systemic_index,
                                   attachment,
                                   exhaustion,
                                   limit) {
  
  stress_scenarios <- data.frame(
    Scenario = c(
      "Moderate Stress",
      "Severe Stress",
      "Extreme Stress",
      "Collapse Scenario"
    ),
    Systemic_Index = c(
      as.numeric(attachment),
      as.numeric(quantile(systemic_index, 0.95, na.rm = TRUE)),
      as.numeric(exhaustion),
      as.numeric(quantile(systemic_index, 0.999, na.rm = TRUE))
    )
  )
  
  stress_scenarios$Payout <- calc_ils_payout(
    index = stress_scenarios$Systemic_Index,
    type = "general",
    A = attachment,
    E = exhaustion,
    L = limit
  )
  
  stress_scenarios
}

run_trigger_sensitivity <- function(systemic_index,
                                    attachment_grid = c(0.85, 0.90, 0.95),
                                    exhaustion_grid = c(0.975, 0.99, 0.995),
                                    limit,
                                    notional,
                                    risk_load) {
  
  expand.grid(
    Attachment_Percentile = attachment_grid,
    Exhaustion_Percentile = exhaustion_grid
  ) %>%
    dplyr::filter(Exhaustion_Percentile > Attachment_Percentile) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      A = as.numeric(quantile(systemic_index, Attachment_Percentile, na.rm = TRUE)),
      E = as.numeric(quantile(systemic_index, Exhaustion_Percentile, na.rm = TRUE)),
      
      Expected_Payout = mean(
        calc_ils_payout(
          index = systemic_index,
          type = "general",
          A = A,
          E = E,
          L = limit
        ),
        na.rm = TRUE
      ),
      
      Expected_Loss_Ratio = Expected_Payout / notional,
      Investor_Spread = Expected_Loss_Ratio + risk_load,
      
      Trigger_Frequency = mean(
        calc_ils_payout(
          index = systemic_index,
          type = "general",
          A = A,
          E = E,
          L = limit
        ) > 0,
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()
}