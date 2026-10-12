# DOAJ–Scopus typology

R code for *A Global DOAJ–Scopus Typology: Indonesia's Disproportionate Gap*.

- `01_build_dataset.R`: links the DOAJ and SJR exports by ISSN
- `02_analysis.R`: all statistics and tables
- `03_make_figures.R`: Figures 2–7 (PNG)
- `PROTOCOL.md`: definitions and analysis steps

## Run

Install R packages `tidyverse`, `patchwork` and `ggforce`. Download the DOAJ journal CSV (https://doaj.org) and the SJR 2025 journal export (https://www.scimagojr.com), save them as `raw/doaj2026.csv` and `raw/sjr2025.csv`, and set `DIR` at the top of each script to this folder. Paste the scripts into the R console in order (01, 02, 03). Output folders are created automatically.

The paper used files downloaded on 17 April 2026. DOAJ changes daily, and newer downloads will give slightly different counts. The linked dataset is not posted here; it is available from the author on reasonable request.

## License

MIT (code only). When using the source data, cite DOAJ and SCImago (SJR — SCImago Journal & Country Rank, https://www.scimagojr.com).
