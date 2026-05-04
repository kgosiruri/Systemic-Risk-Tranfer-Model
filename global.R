# Load required packages
pkgs <- readLines("/Users/kruri/Desktop/systemic risk transfer/Systemic Risk Tranfer Model/requirements.txt")
install.packages(pkgs)

# Load the packages
lapply(pkgs, library, character.only = TRUE)

#Date range for data retrieval
start_date <- "2010-01-01"
end_date   <- Sys.Date()

source("Systemic Risk Tranfer Model/payout_kernel.R")

# Force HTTP/1.1 to bypass the 'INTERNAL_ERROR' on FRED connections
options(download.file.method = "libcurl")
options(url.method = "libcurl")