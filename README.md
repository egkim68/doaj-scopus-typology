# DOAJ–Scopus typology

Code and data for *A Global DOAJ–Scopus Typology: Indonesia's Disproportionate Gap*.

- `data/global_all_journals.csv`: ISSN-linked DOAJ (2026) and SJR (2025) journal list (43,817 journals)
- `code/01_build_dataset.R`: rebuilds the CSV from raw DOAJ and SJR exports (optional)
- `code/02_analysis.R`: all statistics and tables
- `code/03_make_figures.R`: Figures 2–7 (PNG)
- `PROTOCOL.md`: definitions and analysis steps

## Run

Install R packages `tidyverse`, `patchwork` and `ggforce`. Set `DIR` at the top of each script to this folder and paste the script into the R console. Results go to `output/`, which is created automatically.

To rebuild the dataset, place `doaj2026.csv` and `sjr2025.csv` in a `raw/` folder and run `01_build_dataset.R`. Both files were downloaded on 17 April 2026; newer downloads will differ slightly.

## License

Code: MIT. Data derived from DOAJ (CC0) and SCImago Journal & Country Rank; cite both when reusing.
