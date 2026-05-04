
# Systemic Risk Transfer via Insurance-Linked Securities

This repository contains an exploratory R framework for modelling systemic risk transfer through insurance-linked securities (ILS). The model combines oil price movements, exchange rates, inflation, and geopolitical risk into a multi-factor dependence framework using a Student-t copula, PCA-based systemic risk index construction, parametric payout functions, basis risk analysis, and EVT tail diagnostics.

## Project Overview

The objective is to design and test a parametric ILS structure for macro-systemic and energy-linked stress events.

The framework:

- imports macro-financial risk factors from FRED;
- incorporates the Caldara-Iacoviello Geopolitical Risk Index;
- transforms raw series into monthly log returns and changes;
- fits a Student-t copula to model joint dependence;
- simulates systemic risk scenarios;
- constructs a PCA-based systemic risk index;
- defines attachment and exhaustion thresholds;
- evaluates multiple ILS payout structures;
- estimates expected loss ratios and investor spreads;
- analyses basis risk between sponsor loss and ILS payout;
- applies EVT diagnostics to the upper tail of the systemic index;
- produces publication-ready figures for reporting.

## Repository Structure

```text
systemic-risk-transfer-ils/
│
├── main.R
├── global.R
├── requirements.txt
│
├── R/
│   ├── data_retrieval.R
│   ├── data_processing.R
│   ├── copula_model.R
│   ├── systemic_index.R
│   ├── payout_kernel.R
│   ├── pricing_summary.R
│   ├── basis_risk.R
│   ├── evt_tail_model.R
│   ├── stress_scenarios.R
│   └── plotting.R
│
├── data/
│   ├── raw/
│   ├── processed/
│   └── simulated/
│
├── outputs/
│   ├── figures/
│   ├── tables/
│   └── logs/
│
├── report/
│   ├── manuscript.tex
│   ├── references.bib
│   └── figures/
│
└── docs/
    ├── methodology.md
    ├── model_assumptions.md
    └── limitations.md
````

## Data Sources

The model uses the following macro-financial inputs:

| Variable  | Description                                | Source                        |
| --------- | ------------------------------------------ | ----------------------------- |
| WTI       | West Texas Intermediate crude oil price    | FRED                          |
| Brent     | Brent crude oil price                      | FRED                          |
| CPI       | U.S. Consumer Price Index                  | FRED                          |
| USD Index | Nominal broad U.S. dollar index            | FRED                          |
| GPR       | Caldara-Iacoviello Geopolitical Risk Index | Matteo Iacoviello GPR dataset |

The GPR dataset should be downloaded manually and saved as:

```text
data/raw/GPR_Data.xls
```

## Methodology

The modelling pipeline follows these steps:

1. Import raw market and macroeconomic data.
2. Convert all series to monthly frequency.
3. Transform variables into log returns or log changes.
4. Convert marginal data into pseudo-observations.
5. Fit a Gaussian copula for starting values.
6. Fit a Student-t copula using maximum pseudo-likelihood.
7. Simulate joint systemic risk scenarios.
8. Construct a PCA-based systemic risk index.
9. Set ILS attachment and exhaustion thresholds.
10. Apply different payout kernels.
11. Estimate expected payout, expected loss ratio, trigger frequency, and investor spread.
12. Analyse sponsor loss, ILS payout, and basis risk.
13. Fit EVT diagnostics to the upper tail of the systemic risk index.
14. Produce figures and summary tables.

## Payout Structures

The repository currently supports four payout structures:

| Structure            | Description                                                   |
| -------------------- | ------------------------------------------------------------- |
| General Kernel       | Linear payout between attachment and exhaustion               |
| Binary Trigger       | Full payout once attachment is breached                       |
| Linear Excess        | Payout increases with excess systemic stress above attachment |
| Persistence Adjusted | Linear excess payout adjusted for stress persistence          |

## Key Outputs

The model produces:

* systemic risk index distribution;
* PCA loading chart;
* copula dependence diagnostics;
* trigger and exhaustion calibration plots;
* payout distribution plots;
* expected loss ratio comparison;
* basis risk distribution;
* sponsor loss vs ILS payout chart;
* stress scenario payout table;
* sensitivity heatmap;
* EVT exceedance plot.

Outputs are saved in:

```text
outputs/figures/
outputs/tables/
```

## Installation

Install the required R packages:

```r
install.packages(c(
  "quantmod",
  "dplyr",
  "lubridate",
  "readxl",
  "zoo",
  "copula",
  "Matrix",
  "ggplot2",
  "tidyr",
  "scales",
  "forcats",
  "patchwork",
  "ggridges",
  "viridis",
  "evir"
))
```

## Running the Model

Run the full pipeline from the project root:

```r
source("main.R")
```

The main script sources the individual model components from the `R/` directory.

## Important Notes

The current model is exploratory and intended for research development. It is not a production pricing model.

Large simulation outputs should not be committed to GitHub. For example, if `n_copula_sims = 10000000`, only summary tables, figures, and sampled simulation outputs should be saved.

Raw datasets may be excluded from the repository depending on licensing and redistribution restrictions. The data retrieval process should be documented clearly so the analysis can be reproduced.

## Limitations

Current limitations include:

* simplified sponsor loss model;
* synthetic systemic index rather than observed traded trigger;
* dependence calibrated on historical macro-financial data;
* basis risk measured against a stylised sponsor loss function;
* no formal investor utility or spread calibration model;
* no liquidity, collateral, legal, or transaction cost modelling;
* EVT component currently used as a diagnostic rather than full pricing engine.

## Author

Kgosi Ruri Molebatsi

MSc Data Science
University of Kent

## Working Title

**Systemic Risk Transfer via Insurance-Linked Securities: A Parametric Framework for Energy Price Shocks and Multi-Factor Macro Risk**


