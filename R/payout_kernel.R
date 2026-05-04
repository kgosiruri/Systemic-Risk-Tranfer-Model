# ============================================================
# Payout Kernel Functions
# ============================================================

calc_ils_payout <- function(index,
                            type = c("general", "binary", "linear_excess", "persistence"),
                            A,
                            E = NULL,
                            L = LIMIT,
                            alpha = 1,
                            d = 1,
                            n = 1) {
  
  type <- match.arg(type)
  
  payout <- switch(
    type,
    
    # Linear payout between attachment A and exhaustion E
    "general" = {
      if (is.null(E)) {
        stop("Exhaustion point E is required for the general payout kernel.")
      }
      if (E <= A) {
        stop("Exhaustion point E must be greater than attachment point A.")
      }
      
      L * pmin(
        1,
        pmax((index - A) / (E - A), 0)
      )
    },
    
    # Full payout once the index exceeds attachment
    "binary" = {
      ifelse(index > A, L, 0)
    },
    
    # Payout increases with excess stress above attachment
    "linear_excess" = {
      pmin(
        L,
        alpha * pmax(index - A, 0)
      )
    },
    
    # Linear excess payout adjusted by duration/persistence factor
    "persistence" = {
      pmin(
        L,
        alpha * pmax(index - A, 0) * (d^n)
      )
    }
  )
  
  return(payout)
}

build_payout_curve_data <- function(attachment,
                                    exhaustion,
                                    limit,
                                    range_buffer = 1.5) {
  
  alpha <- limit / as.numeric(exhaustion - attachment)
  
  curve_data <- tibble::tibble(
    Systemic_Index = seq(
      as.numeric(attachment) - range_buffer,
      as.numeric(exhaustion) + range_buffer,
      length.out = 1000
    )
  ) %>%
    dplyr::mutate(
      `General Kernel` = calc_ils_payout(
        index = Systemic_Index,
        type = "general",
        A = attachment,
        E = exhaustion,
        L = limit
      ),
      `Binary Trigger` = calc_ils_payout(
        index = Systemic_Index,
        type = "binary",
        A = attachment,
        L = limit
      ),
      `Linear Excess` = calc_ils_payout(
        index = Systemic_Index,
        type = "linear_excess",
        A = attachment,
        L = limit,
        alpha = alpha
      ),
      `Persistence Adjusted` = calc_ils_payout(
        index = Systemic_Index,
        type = "persistence",
        A = attachment,
        L = limit,
        alpha = alpha,
        d = 2,
        n = 1
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -Systemic_Index,
      names_to = "Structure",
      values_to = "Payout"
    )
  
  return(curve_data)
}
