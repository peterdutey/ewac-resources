# <img src="man/figures/logo.svg" align="right" width="150px"/>EWAC: Resources to Estimate Weekly Alcohol Consumption from the Extended AUDIT-C

<!-- badges: start -->
[![R-CMD-check](https://github.com/peterdutey/ewac-resources/workflows/R-CMD-check/badge.svg)](https://github.com/peterdutey/ewac-resources/actions)
<!-- badges: end -->


This repository contain resources to implement the Estimator of Weekly Alcohol Consumption (EWAC) developed and validated in a peer-reviewed journal article available from *Addiction* (DOI: [10.1111/add.15662](https://doi.org/10.1111/add.15662)).


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
