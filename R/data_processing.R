# 5. Merge All Risk Factors at Monthly Frequency

wti_monthly <- wti_fred %>%
  mutate(Date = floor_date(Date, "month")) %>%
  group_by(Date) %>%
  summarise(WTI = last(na.omit(WTI)), .groups = "drop")

brent_monthly <- brent_fred %>%
  mutate(Date = floor_date(Date, "month")) %>%
  group_by(Date) %>%
  summarise(Brent = last(na.omit(Brent)), .groups = "drop")

usd_monthly <- usd_fred %>%
  mutate(Date = floor_date(Date, "month")) %>%
  group_by(Date) %>%
  summarise(USD_Index = last(na.omit(USD_Index)), .groups = "drop")

cpi_monthly <- cpi_fred %>%
  mutate(Date = floor_date(Date, "month")) %>%
  group_by(Date) %>%
  summarise(CPI = last(na.omit(CPI)), .groups = "drop")

gpr_monthly <- gpr %>%
  mutate(Date = floor_date(Date, "month")) %>%
  group_by(Date) %>%
  summarise(GPR = last(na.omit(GPR)), .groups = "drop")

risk_data <- wti_monthly %>%
  full_join(brent_monthly, by = "Date") %>%
  full_join(usd_monthly, by = "Date") %>%
  full_join(cpi_monthly, by = "Date") %>%
  full_join(gpr_monthly, by = "Date") %>%
  arrange(Date) %>%
  mutate(
    WTI = na.locf(WTI, na.rm = FALSE),
    Brent = na.locf(Brent, na.rm = FALSE),
    USD_Index = na.locf(USD_Index, na.rm = FALSE),
    CPI = na.locf(CPI, na.rm = FALSE),
    GPR = na.locf(GPR, na.rm = FALSE)
  ) %>%
  na.omit()

# 6. Monthly Returns / Changes

model_data <- risk_data %>%
  mutate(
    WTI_Return = log(WTI / lag(WTI)),
    Brent_Return = log(Brent / lag(Brent)),
    USD_Return = log(USD_Index / lag(USD_Index)),
    CPI_Inflation = log(CPI / lag(CPI)),
    GPR_Change = log(GPR / lag(GPR))
  ) %>%
  select(
    Date,
    WTI_Return,
    Brent_Return,
    USD_Return,
    CPI_Inflation,
    GPR_Change
  ) %>%
  filter(if_all(-Date, ~ is.finite(.))) %>%
  na.omit()
