calc_ils_payout <- function(index, 
                            type = c("general", "binary", "linear_excess", "persistence"), 
                            A, 
                            E = NULL, 
                            L = 10000000, 
                            alpha = 1,
                            d = 1,  # Duration of the shock
                            n = 1)  # Persistence factor
{
  type <- match.arg(type)
  
  payout <- switch(type,
    # 1. General Payout Kernel (Section 1.4)
    "general" = {
      if(is.null(E)) stop("Exhaustion point 'E' required for General Kernel.")
      L * pmin(1, pmax((index - A) / (E - A), 0))
    },
    
    # 2. Binary Trigger (Section 1.6)
    "binary" = {
      ifelse(index > A, L, 0)
    },
    
    # 3. Linear Excess Trigger (Section 1.6)
    "linear_excess" = {
      alpha * pmax(index - A, 0)
    },
    
    # 4. Persistence-Adjusted Trigger (Section 1.6)[cite: 1]
    # Payout scales with how long the shock lasts
    "persistence" = {
      alpha * pmax(index - A, 0) * (d^n)
    }
  )
  
  return(payout)
}
