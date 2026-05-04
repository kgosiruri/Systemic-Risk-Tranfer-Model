options(download.file.method = "libcurl")

wti <- getSymbols(
  "DCOILWTICO",
  src = "FRED",
  auto.assign = FALSE
)
# WTI crude oil price
getSymbols("DCOILWTICO", src = "FRED", from = start_date, to = end_date)

# U.S. CPI index
getSymbols("CPIAUCSL", src = "FRED", from = start_date, to = end_date)

# Nominal Broad U.S. Dollar Index
getSymbols("DTWEXBGS", src = "FRED", from = start_date, to = end_date)

# Optional: Brent crude oil
getSymbols("DCOILBRENTEU", src = "FRED", from = start_date, to = end_date)

wti_fred <- data.frame(
  Date = index(DCOILWTICO),
  WTI = as.numeric(DCOILWTICO$DCOILWTICO)
)

brent_fred <- data.frame(
  Date = index(DCOILBRENTEU),
  Brent = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
)

cpi_fred <- data.frame(
  Date = index(CPIAUCSL),
  CPI = as.numeric(CPIAUCSL$CPIAUCSL)
)

usd_fred <- data.frame(
  Date = index(DTWEXBGS),
  USD_Index = as.numeric(DTWEXBGS$DTWEXBGS)
)


url <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"
dest <- tempfile(fileext = ".xls")

download.file(url, destfile = dest, mode = "wb")

gpr_raw <- read_excel(dest)

names(gpr_raw)

gpr <- gpr_raw %>%
  transmute(
    Date = as.Date(month),
    GPR = as.numeric(GPRH_BASIC)
  ) %>%
  na.omit()