# ============================================================
# Data Processing Functions
# ============================================================

# ------------------------------------------------------------
# Helper: safely take last non-missing value
# ------------------------------------------------------------

last_non_na <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  dplyr::last(x)
}

# ------------------------------------------------------------
# 1. Build monthly aligned risk dataset
# ------------------------------------------------------------

build_monthly_risk_dataset <- function(wti_fred,
                                       brent_fred,
                                       cpi_fred,
                                       usd_fred,
                                       gpr) {
  
  wti_monthly <- wti_fred %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      WTI = last_non_na(WTI),
      .groups = "drop"
    )
  
  brent_monthly <- brent_fred %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      Brent = last_non_na(Brent),
      .groups = "drop"
    )
  
  cpi_monthly <- cpi_fred %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      CPI = last_non_na(CPI),
      .groups = "drop"
    )
  
  usd_monthly <- usd_fred %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      USD_Index = last_non_na(USD_Index),
      .groups = "drop"
    )
  
  gpr_monthly <- gpr %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, "month")) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      GPR = last_non_na(GPR),
      .groups = "drop"
    )
  
  risk_data <- wti_monthly %>%
    dplyr::full_join(brent_monthly, by = "Date") %>%
    dplyr::full_join(cpi_monthly, by = "Date") %>%
    dplyr::full_join(usd_monthly, by = "Date") %>%
    dplyr::full_join(gpr_monthly, by = "Date") %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(
      WTI = zoo::na.locf(WTI, na.rm = FALSE),
      Brent = zoo::na.locf(Brent, na.rm = FALSE),
      CPI = zoo::na.locf(CPI, na.rm = FALSE),
      USD_Index = zoo::na.locf(USD_Index, na.rm = FALSE),
      GPR = zoo::na.locf(GPR, na.rm = FALSE)
    ) %>%
    dplyr::filter(stats::complete.cases(.))
  
  return(risk_data)
}

# ------------------------------------------------------------
# 2. Create model returns / changes
# ------------------------------------------------------------

create_model_returns <- function(risk_data) {
  
  model_data <- risk_data %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(
      WTI_Return = log(WTI / dplyr::lag(WTI)),
      Brent_Return = log(Brent / dplyr::lag(Brent)),
      USD_Return = log(USD_Index / dplyr::lag(USD_Index)),
      CPI_Inflation = log(CPI / dplyr::lag(CPI)),
      GPR_Change = log(GPR / dplyr::lag(GPR))
    ) %>%
    dplyr::select(
      Date,
      WTI_Return,
      Brent_Return,
      USD_Return,
      CPI_Inflation,
      GPR_Change
    ) %>%
    dplyr::filter(dplyr::if_all(-Date, ~ is.finite(.))) %>%
    tidyr::drop_na()
  
  return(model_data)
}

# ------------------------------------------------------------
# 3. Save processed data
# ------------------------------------------------------------

save_processed_data <- function(risk_data, model_data) {
  
  if (!dir.exists(PROCESSED_DIR)) {
    dir.create(PROCESSED_DIR, recursive = TRUE)
  }
  
  write.csv(
    risk_data,
    file.path(PROCESSED_DIR, "risk_data.csv"),
    row.names = FALSE
  )
  
  write.csv(
    model_data,
    file.path(PROCESSED_DIR, "model_data.csv"),
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 4. Basic data checks
# ------------------------------------------------------------

check_model_data <- function(model_data) {
  
  checks <- data.frame(
    Variable = names(model_data),
    Missing_Count = sapply(model_data, function(x) sum(is.na(x))),
    Infinite_Count = sapply(model_data, function(x) sum(is.infinite(x))),
    Class = sapply(model_data, function(x) class(x)[1])
  )
  
  return(checks)
}
