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

# New FRED series for freight rates added this 13 May 2026 should improve the GPR model's performance by providing a more direct measure of global trade conditions, which are a key driver of geopolitical risk.
TSIFRGHT <- quantmod::getSymbols(
  "TSIFRGHT",
  src = "FRED",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)


# ------------------------------------------------------------
# 1.3. Fetch Shipping Companies (Yahoo Finance - Daily)
# ------------------------------------------------------------
# Frontline (FRO), Scorpio (STNG), DHT Holdings (DHT), International Seaways (INSW)

# Note: auto.assign = FALSE requires pulling tickers individually

fro_stock <- quantmod::getSymbols(
  "FRO",
  src = "yahoo", 
  from = start_date, 
  to = end_date, 
  auto.assign = FALSE)

stng_stock <- quantmod::getSymbols(
  "STNG", 
  src = "yahoo", 
  from = start_date, 
  to = end_date, 
  auto.assign = FALSE)
dht_stock <- quantmod::getSymbols(
  "DHT", 
  src = "yahoo", 
  from = start_date, 
  to = end_date, 
  auto.assign = FALSE)

insw_stock <- quantmod::getSymbols(
  "INSW", 
  src = "yahoo", 
  from = start_date, 
  to = end_date, 
  auto.assign = FALSE)

# ------------------------------------------------------------
# 1.4 Fetch Oil Producer Exchange Rates (FRED - Monthly/Daily)
# ------------------------------------------------------------
# Canada Dollar to USD (DEXCAUS - Daily)
cad_fx <- quantmod::getSymbols("DEXCAUS", 
src = "FRED", 
from = start_date, 
to = end_date, 
auto.assign = FALSE)
  
# Brazil Real to USD (DEXBZUS - Daily)
brl_fx <- quantmod::getSymbols("DEXBZUS", 
src = "FRED", 
from = start_date, 
to = end_date, 
auto.assign = FALSE)

# ------------------------------------------------------------
# 3. Convert New Series to Data Frames (Matching your format)
# ------------------------------------------------------------
# Shipping Stocks (Using Adjusted Close prices to account for dividends/splits)
fro_df <- data.frame(
    Date = zoo::index(fro_stock),
    Shipping_FRO = as.numeric(quantmod::Ad(fro_stock))
  )

stng_df <- data.frame(
    Date = zoo::index(stng_stock),
    Shipping_STNG = as.numeric(quantmod::Ad(stng_stock))
  )

dht_df <- data.frame(
  Date = zoo::index(dht_stock),
  Shipping_DHT = as.numeric(quantmod::Ad(dht_stock))
)

insw_df <- data.frame(
  Date = zoo::index(insw_stock),
  Shipping_INSW = as.numeric(quantmod::Ad(insw_stock))
)

# Producer Currency Exchange Rates
cad_df <- data.frame(
  Date = zoo::index(cad_fx),
  FX_Canada = as.numeric(cad_fx$DEXCAUS)
)

brl_df <- data.frame(
  Date = zoo::index(brl_fx),
  FX_Brazil = as.numeric(brl_fx$DEXBZUS)
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

freight_fred <- data.frame(
  Date = zoo::index(TSIFRGHT),
  Freight_Rate = as.numeric(TSIFRGHT$TSIFRGHT)
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
    gpr = gpr,
    freight_fred = freight_fred,
    fro_df = fro_df,
    stng_df = stng_df,
    dht_df = dht_df,
    insw_df = insw_df,
    cad_df = cad_df,
    brl_df = brl_df
  )
}
