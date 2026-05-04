# ============================================================
# Data Retrieval Functions
# ============================================================

retrieve_risk_data <- function(start_date, end_date) {
  
  options(download.file.method = "libcurl")
  
  # ------------------------------------------------------------
  # 1. Download FRED data
  # ------------------------------------------------------------
  
  DCOILWTICO <- quantmod::getSymbols(
    "DCOILWTICO",
    src = "FRED",
    from = start_date,
    to = end_date,
    auto.assign = FALSE
  )
  
  CPIAUCSL <- quantmod::getSymbols(
    "CPIAUCSL",
    src = "FRED",
    from = start_date,
    to = end_date,
    auto.assign = FALSE
  )
  
  DTWEXBGS <- quantmod::getSymbols(
    "DTWEXBGS",
    src = "FRED",
    from = start_date,
    to = end_date,
    auto.assign = FALSE
  )
  
  DCOILBRENTEU <- quantmod::getSymbols(
    "DCOILBRENTEU",
    src = "FRED",
    from = start_date,
    to = end_date,
    auto.assign = FALSE
  )
  
  # ------------------------------------------------------------
  # 2. Convert FRED series to data frames
  # ------------------------------------------------------------
  
  wti_fred <- data.frame(
    Date = zoo::index(DCOILWTICO),
    WTI = as.numeric(DCOILWTICO$DCOILWTICO)
  )
  
  brent_fred <- data.frame(
    Date = zoo::index(DCOILBRENTEU),
    Brent = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
  )
  
  cpi_fred <- data.frame(
    Date = zoo::index(CPIAUCSL),
    CPI = as.numeric(CPIAUCSL$CPIAUCSL)
  )
  
  usd_fred <- data.frame(
    Date = zoo::index(DTWEXBGS),
    USD_Index = as.numeric(DTWEXBGS$DTWEXBGS)
  )
  
  # ------------------------------------------------------------
  # 3. Download and import GPR data
  # ------------------------------------------------------------
  
  url <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"
  dest <- tempfile(fileext = ".xls")
  
  download.file(
    url,
    destfile = dest,
    mode = "wb"
  )
  
  gpr_raw <- readxl::read_excel(dest)
  
  gpr <- gpr_raw %>%
    dplyr::transmute(
      Date = as.Date(month),
      GPR = as.numeric(GPRH_BASIC)
    ) %>%
    tidyr::drop_na()
  
  # ------------------------------------------------------------
  # 4. Return datasets
  # ------------------------------------------------------------
  
  list(
    wti_fred = wti_fred,
    brent_fred = brent_fred,
    cpi_fred = cpi_fred,
    usd_fred = usd_fred,
    gpr_raw = gpr_raw,
    gpr = gpr
  )
}