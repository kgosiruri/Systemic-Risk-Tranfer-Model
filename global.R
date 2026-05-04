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
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SIM_DATA_DIR, recursive = TRUE, showWarnings = FALSE)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOGS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

#### Model Configuration ####

# Date range
START_DATE <- "1987-01-01"
END_DATE   <- Sys.Date()

# Simulation
N_SIM <- 100000000
SEED  <- 123

joint_sim_data <- c()

# Copula
COPULA_DF_START <- 6
EPSILON <- 1e-6

# ILS structure
NOTIONAL   <- 10000000
LIMIT      <- 10000000
RISK_LOAD  <- 0.04

ATTACHMENT_Q  <- 0.90
EXHAUSTION_Q  <- 0.995

# Sponsor loss
SPONSOR_LOSS_CAP <- 20000000
SPONSOR_TRIGGER_Q <- 0.70

KES_RED      <- "#8B0000"
KES_RED_2    <- "#B22222"
KES_RED_3    <- "#D94A4A"
KES_DARK     <- "#1C1C1C"
KES_GREY     <- "#F4F4F4"
KES_MIDGREY  <- "#9E9E9E"
KES_GOLD     <- "#C9A227"