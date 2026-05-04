#### EVT Tail Model Functions ####

fit_evt_tail <- function(systemic_index, threshold_q = 0.95) {
  
  threshold <- quantile(
    systemic_index,
    probs = threshold_q,
    na.rm = TRUE
  )
  
  evt_fit <- evir::gpd(
    systemic_index,
    threshold = threshold
  )
  
  list(
    threshold = threshold,
    fit = evt_fit
  )
}

prepare_evt_exceedances <- function(systemic_index, threshold) {
  
  data.frame(
    Systemic_Index = systemic_index
  ) %>%
    filter(Systemic_Index > threshold) %>%
    mutate(
      Exceedance = Systemic_Index - threshold
    )
}

summarise_evt_tail <- function(evt_fit, threshold) {
  
  params <- evt_fit$par.ests
  
  data.frame(
    Metric = c(
      "EVT Threshold",
      "GPD Shape",
      "GPD Scale"
    ),
    Value = c(
      as.numeric(threshold),
      as.numeric(params["xi"]),
      as.numeric(params["beta"])
    )
  )
}