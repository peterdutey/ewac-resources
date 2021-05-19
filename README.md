# <img src="man/figures/logo.svg" align="right" style="max-width: 30%; padding: 10px;"/>EWAC: Resources to Estimate Weekly Alcohol Consumption from the Extended AUDIT-C


This repository contain resources to implement the Estimator of Weekly Alcohol Consumption (EWAC) developed and validated in a pre-print manuscript available from **medRxiv** (DOI: [](https://doi.org/)).


## R package

The easiest way is to install the package directly from GitHub using the `remotes` package:

```
remotes::install_github("peterdutey/ewac-resources")
library("EWAC")
compute_ewac(data.frame(list(
  audit1_label = "2 to 3 times a week",
  audit2_label = "1 to 2",
  audit3_label = "Monthly"
)))
```

## Spreadsheets

Spreadsheets with formulae are available

* in [ODS format (10KB)](https://github.com/peterdutey/ewac-resources/raw/master/spreadsheets/ewac-spreadsheet.ods)
* in [XLSX format (18KB)](https://github.com/peterdutey/ewac-resources/raw/master/spreadsheets/ewac-spreadsheet.xlsx)
