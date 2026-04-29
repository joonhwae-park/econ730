# ECON 730 Replication

This repository replicates Table 1 main result from:

Hersh, J. and R. K. Mithas, "How APIs Create Growth by Inverting the Firm".

The replicated part is the main two-way fixed effects result for public API adoption and log market value. The original Stata code uses `reghdfe` with firm fixed effects, quarter fixed effects, and firm clustered standard errors. The R code here uses `fixest::feols()` for the same specification.

I also added two extra checks:

- two-way clustered standard errors by firm and quarter;
- Callaway and Sant'Anna DID estimates using the `did` package, with a dynamic ATT plot.

The script also makes a pretrend/event-study plot and saves all numeric results to JSON.

## Files

- `programs/replicate_table1_main_result.R`: main replication script.
- `analysis_dataset/table1_minimal.csv.gz`: small data file with only variables needed for this replication.
- `tables/TABLE_01_main_result_replication.json`: saved regression and DID results.
- `figures/TABLE_01_pretrend_event_study_replication.png`: event-study pretrend plot.
- `figures/TABLE_01_did_santanna_dynamic_replication.png`: Callaway-Sant'Anna dynamic DID plot.

The full original Stata data files are not included because they are too large for normal GitHub upload and are not needed for this reduced replication.

## How to Run

Install the needed R packages:

```r
install.packages(c("haven", "dplyr", "did", "fixest", "ggplot2", "jsonlite", "tibble"))
```

Run the replication from the repository root:

```bash
Rscript programs/replicate_table1_main_result.R
```

The script prints the main results and writes:

```text
tables/TABLE_01_main_result_replication.json
figures/TABLE_01_pretrend_event_study_replication.png
figures/TABLE_01_did_santanna_dynamic_replication.png
```
