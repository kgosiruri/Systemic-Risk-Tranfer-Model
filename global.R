#### Load required packages ####
required_packages <- c(
  "quantmod", "dplyr", "lubridate", "readxl", "zoo",
  "copula", "Matrix", "ggplot2", "tidyr", "scales",
  "forcats", "patchwork", "ggridges", "viridis", "evir"
)
invisible(lapply(required_packages, library, character.only = TRUE))

#### Load the packages ####
lapply(required_packages, library, character.only = TRUE)

#### Global Options ####
options(
  scipen = 999,
  digits = 6,
  stringsAsFactors = FALSE
)

#### Project paths ####
BASE_DIR <- getwd()

DATA_DIR        <- file.path(BASE_DIR, "data")
RAW_DATA_DIR    <- file.path(DATA_DIR, "raw")
PROCESSED_DIR   <- file.path(DATA_DIR, "processed")
SIM_DATA_DIR    <- file.path(DATA_DIR, "simulated")

OUTPUT_DIR      <- file.path(BASE_DIR, "outputs")
FIGURES_DIR     <- file.path(OUTPUT_DIR, "figures")
TABLES_DIR      <- file.path(OUTPUT_DIR, "tables")
LOGS_DIR        <- file.path(OUTPUT_DIR, "logs")

FIG_REPORT_DIR  <- file.path(BASE_DIR, "report", "figures")

#### Creating necessary directories if they don't exist ####
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

#### Model Configuration ####

# Date range
start_date <- "1987-01-01"
end_date   <- Sys.Date()

# Simulation
N_SIM <- 1000
SEED  <- 123

# Copula
COPULA_DF_START <- 6
EPSILON <- 1e-4

# ILS structure
NOTIONAL   <- 10000000
LIMIT      <- 10000000
RISK_LOAD  <- 0.04

ATTACHMENT_Q  <- 0.90
EXHAUSTION_Q  <- 0.995

# Sponsor loss
SPONSOR_LOSS_CAP <- 20000000
SPONSOR_TRIGGER_Q <- 0.85