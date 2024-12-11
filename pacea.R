library(remotes)
remotes::install_github("r_lib/usethis", force=TRUE)
remotes::install_github("pbs-assess/pacea")
library(pacea)

# Play around with PACEA
help(package = "pacea")

pdo # Pacific Decadal Oscillation (monthly anomalies)
npi_annual # North Pacific Index values and annual anomalies

alpi # Aleutian Low Pressure Index annual anomalies

