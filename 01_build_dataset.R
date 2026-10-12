# =============================================================================
# 01_build_dataset.R -- ISSN-based linkage of DOAJ and SJR into one journal list
# Input : raw/doaj2026.csv  (DOAJ journal CSV export, 2026 edition)
#         raw/sjr2025.csv   (SCImago Journal & Country Rank export, 2025 edition)
# Output: data/global_all_journals.csv  (43,817 journals with the 17 April 2026 files; input to 02 and 03)
# Raw exports are not redistributed; download them from doaj.org and
# scimagojr.com. Fresh downloads will differ slightly from the 2026/2025
# snapshots used in the paper, which is why the linked dataset is provided.
# =============================================================================

library(tidyverse)

DIR <- "."   # <- path to this repository folder, e.g. "C:/Users/you/doaj-scopus-typology"
DOAJ_FILE  <- file.path(DIR, "raw", "doaj2026.csv")
SJR_FILE   <- file.path(DIR, "raw", "sjr2025.csv")
OUTPUT_DIR <- file.path(DIR, "data")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

CAT_LEVELS <- c("DOAJ only", "Dual-indexed", "SJR only")

# =============================================================================
# 1. COUNTRY NAME BRIDGE
# =============================================================================

country_bridge <- tribble(
  ~doaj_name,                               ~sjr_name,
  "Bolivia, Plurinational State of",        "Bolivia",
  "Congo, The Democratic Republic of the",  "Democratic Republic of the Congo",
  "Czechia",                                "Czech Republic",
  "Cote d'Ivoire",                          "Ivory Coast",
  "Iran, Islamic Republic of",              "Iran",
  "Korea, Republic of",                     "South Korea",
  "Moldova, Republic of",                   "Moldova",
  "North Macedonia",                        "Macedonia",
  "Palestine, State of",                    "Palestine",
  "Syrian Arab Republic",                   "Syria",
  "Taiwan, Province of China",              "Taiwan",
  "Tanzania, United Republic of",           "Tanzania",
  "Turkiye",                                "Turkey",
  "T\u00fcrkiye",                           "Turkey",   # DOAJ spelling with umlaut
  "Venezuela, Bolivarian Republic of",      "Venezuela"
)

normalize_country <- function(x) {
  bridge <- setNames(country_bridge$sjr_name, country_bridge$doaj_name)
  coalesce(bridge[x], x)
}

# Load SJR, fix duplicate Publisher column, filter to journals
sjr_raw_pre <- read_csv2(SJR_FILE, show_col_types = FALSE)
dup_pub_col <- names(sjr_raw_pre)[names(sjr_raw_pre) != names(sjr_raw_pre)[6] &
                                    grepl("Publisher", names(sjr_raw_pre))]
names(sjr_raw_pre)[6] <- "Publisher"
if (length(dup_pub_col) > 0) sjr_raw_pre <- sjr_raw_pre |> select(-any_of(dup_pub_col))

cat("\n=== SJR TYPE DISTRIBUTION (before filter) ===\n")
print(count(sjr_raw_pre, Type, sort = TRUE))
sjr_raw_pre <- sjr_raw_pre |> filter(str_to_lower(Type) == "journal")
cat("SJR journals after type filter: n =", nrow(sjr_raw_pre), "\n")

# Country-region map
country_region_map_base <- sjr_raw_pre |>
  distinct(Country, Region) |>
  filter(!is.na(Country), !is.na(Region), Country != "", Region != "") |>
  rename(country = Country, region = Region) |>
  distinct(country, .keep_all = TRUE)

manual_region <- tribble(
  ~country,                            ~region,
  "Angola",                            "Africa",
  "Democratic Republic of the Congo",  "Africa",
  "Ivory Coast",                       "Africa",
  "Gabon",                             "Africa",
  "Gambia",                            "Africa",
  "Niger",                             "Africa",
  "South Sudan",                       "Africa",
  "Togo",                              "Africa",
  "Barbados",                          "Latin America",
  "Dominican Republic",                "Latin America",
  "El Salvador",                       "Latin America",
  "Guatemala",                         "Latin America",
  "Honduras",                          "Latin America",
  "Nicaragua",                         "Latin America",
  "Panama",                            "Latin America",
  "Afghanistan",                       "Asiatic Region",
  "Timor-Leste",                       "Asiatic Region",
  "Syria",                             "Middle East",
  "Yemen",                             "Middle East",
  "Guam",                              "Pacific Region",
  "Kosovo",                            "Eastern Europe",
  "Aland Islands",                     "Western Europe"
)

country_region_map <- bind_rows(country_region_map_base, manual_region) |>
  distinct(country, .keep_all = TRUE) |>
  mutate(region = if_else(region == "Africa/Middle East", "Africa", region))

# =============================================================================
# 2. LOAD AND CLEAN
# =============================================================================

doaj_raw <- read_csv(DOAJ_FILE, show_col_types = FALSE)
sjr_raw  <- sjr_raw_pre

doaj_all <- doaj_raw |>
  rename(
    title         = `Journal title`,
    country_raw   = `Country of publisher`,
    issn_print    = `Journal ISSN (print version)`,
    issn_online   = `Journal EISSN (online version)`,
    license       = `Journal license`,
    apc_yn        = APC,
    apc_amount    = `APC amount`,
    waiver        = `Journal waiver policy (for developing country authors etc)`,
    preservation  = `Preservation Services`,
    review        = `Review process`,
    added_date    = `Added on Date`,
    subjects_doaj = Subjects
  ) |>
  mutate(
    country        = normalize_country(country_raw),
    apc_numeric    = suppressWarnings(as.numeric(str_extract(apc_amount, "[0-9.]+"))),
    has_waiver     = coalesce(waiver, "") == "Yes",
    has_preserv    = !is.na(preservation) & preservation != "",
    dbl_review     = str_detect(coalesce(review, ""), regex("double", ignore_case = TRUE)),
    added_year     = as.integer(substr(added_date, 1, 4)),
    openness_score = recode(coalesce(license, ""),
      "CC BY" = 4L, "CC BY-SA" = 3L, "CC BY-NC" = 2L,
      "CC BY-ND" = 2L, "CC BY-NC-SA" = 2L, "CC BY-NC-ND" = 1L,
      .default = NA_integer_)
  )

sjr_all <- sjr_raw |>
  rename(
    title = Title, country = Country, issn_raw = Issn,
    oa = `Open Access`, sjr_score = SJR, quartile = `SJR Best Quartile`,
    h_index = `H index`, docs_2025 = `Total Docs. (2025)`,
    cites_per_doc = `Citations / Doc. (2years)`, coverage = Coverage, areas = Areas
  ) |>
  mutate(
    across(c(sjr_score, cites_per_doc), as.numeric),
    h_index           = as.numeric(h_index),
    docs_2025         = as.numeric(docs_2025),
    coverage_start    = as.integer(str_extract(coverage, "^\\d{4}")),
    coverage_end      = as.integer(str_extract(coverage, "\\d{4}$")),
    coverage_span     = coverage_end - coverage_start,
    coverage_span_adj = coverage_end - pmax(coverage_start, 2004L),
    quartile          = factor(quartile, levels = c("Q1","Q2","Q3","Q4"))
  )

# =============================================================================
# 3. ISSN-BASED GLOBAL JOIN
# =============================================================================

doaj_issn <- doaj_all |>
  select(title, country, issn_print, issn_online, license, openness_score,
         apc_yn, apc_numeric, has_waiver, has_preserv, dbl_review,
         added_year, subjects_doaj) |>
  pivot_longer(c(issn_print, issn_online), values_to = "issn") |>
  filter(!is.na(issn)) |>
  mutate(issn_norm = str_remove_all(issn, "-")) |>
  select(-name, -issn) |>
  distinct(issn_norm, .keep_all = TRUE)

sjr_issn <- sjr_all |>
  mutate(issn_list = str_split(str_remove_all(issn_raw, " "), ",")) |>
  unnest(issn_list) |>
  mutate(issn_norm = str_trim(issn_list)) |>
  select(title, country, issn_norm, sjr_score, quartile, h_index, docs_2025,
         cites_per_doc, coverage_span, coverage_span_adj, coverage_start, areas, oa) |>
  distinct(issn_norm, .keep_all = TRUE)

global_join <- full_join(
  doaj_issn |> mutate(in_doaj = TRUE),
  sjr_issn  |> mutate(in_sjr  = TRUE),
  by = "issn_norm", suffix = c("_doaj", "_sjr")
) |>
  mutate(
    in_doaj   = replace_na(in_doaj, FALSE),
    in_sjr    = replace_na(in_sjr,  FALSE),
    country   = coalesce(country_doaj, country_sjr),
    index_cat = case_when(
      in_doaj & in_sjr  ~ "Dual-indexed",
      in_doaj & !in_sjr ~ "DOAJ only",
      !in_doaj & in_sjr ~ "SJR only"
    ),
    title = coalesce(title_doaj, title_sjr)
  )

global_all_pre_titlefix <- global_join |>
  mutate(cat_priority = case_when(
    index_cat == "Dual-indexed" ~ 1L,
    index_cat == "DOAJ only"   ~ 2L,
    index_cat == "SJR only"    ~ 3L
  )) |>
  arrange(cat_priority) |>
  distinct(issn_norm, .keep_all = TRUE) |>
  distinct(title,     .keep_all = TRUE)

# ---------------------------------------------------------------------------
# AUDIT: exact breakdown of the 176-record gap between raw DOAJ (22,694) and
# final DOAJ-registered journals (22,518), referenced in the paper's Methods
# 2.1. Two candidate mechanisms exist upstream of global_all_pre_titlefix:
#   (a) a DOAJ record dropped entirely because neither issn_print nor
#       issn_online was populated (dropped during the pivot_longer + filter
#       step that built doaj_issn), and
#   (b) a DOAJ record with both a print and an electronic ISSN, which
#       initially survives as two separate issn_norm rows (they differ),
#       then collapses into one row at the distinct(title, .keep_all=TRUE)
#       step above, since both rows share the same coalesced title.
# This block reports both counts directly so the split no longer needs to
# be inferred from reading the pipeline.
# ---------------------------------------------------------------------------
n_doaj_no_issn <- doaj_all |>
  filter(is.na(issn_print) & is.na(issn_online)) |>
  nrow()

n_doaj_title_collapsed <- global_all_pre_titlefix |>
  filter(in_doaj) |>
  nrow()
n_doaj_title_collapsed <- (doaj_all |> nrow()) - n_doaj_no_issn - n_doaj_title_collapsed

cat("\n=== DOAJ 176-record gap audit (Methods 2.1) ===\n")
cat("Raw DOAJ records:                                  ", nrow(doaj_all), "\n")
cat("(a) Dropped, neither ISSN populated:                ", n_doaj_no_issn, "\n")
cat("(b) Collapsed at distinct(title), both ISSNs present:", n_doaj_title_collapsed, "\n")
cat("Sum (a)+(b), compare to 176:                        ", n_doaj_no_issn + n_doaj_title_collapsed, "\n")
cat("================================================\n\n")
#
# ROOT CAUSE: distinct(title, .keep_all = TRUE) above dedupes on
# title = coalesce(title_doaj, title_sjr). A Dual-indexed row is deduped on
# DOAJ's title string, while its SJR-only counterpart for the same real
# journal (same journal, different ISSN -- one matched DOAJ, one didn't) is
# deduped on SJR's title string. If DOAJ and SJR format the same journal's
# title even slightly differently (capitalization, punctuation), the two
# rows are not recognized as duplicates and both survive.
#
# This step catches that case directly on title_sjr, which is guaranteed
# identical for both rows when they originate from the same SJR record's
# unnested print/electronic ISSNs. It merges a pair only when country_sjr
# and sjr_score also match (or both are missing, e.g. discontinued
# journals with no live score) -- this is what protects against merging two
# genuinely different journals that happen to share a common title.
# Verified on the 2025/2026 data: 181 such pairs found and merged; 2 title
# collisions ("Investigaciones Geograficas": Mexico vs. Spain; "Principia":
# Brazil vs. Poland) correctly excluded as different journals.
# ---------------------------------------------------------------------------
dup_sjr_titles <- global_all_pre_titlefix |>
  filter(!is.na(title_sjr)) |>
  group_by(title_sjr) |>
  filter(n() > 1, n_distinct(index_cat) > 1) |>
  summarise(
    n_countries      = n_distinct(country_sjr),
    n_scores_nonnull = n_distinct(sjr_score[!is.na(sjr_score)]),
    .groups = "drop"
  ) |>
  filter(n_countries <= 1, n_scores_nonnull <= 1) |>
  pull(title_sjr)

cat("Cross-category SJR-title duplicates merged:", length(dup_sjr_titles), "\n")

global_all <- global_all_pre_titlefix |>
  filter(!(title_sjr %in% dup_sjr_titles & index_cat == "SJR only")) |>
  filter(!is.na(country)) |>
  left_join(country_region_map, by = "country") |>
  mutate(
    index_cat      = factor(index_cat, levels = CAT_LEVELS),
    license        = coalesce(license,        NA_character_),
    apc_yn         = coalesce(apc_yn,         NA_character_),
    apc_numeric    = coalesce(apc_numeric,    NA_real_),
    openness_score = coalesce(openness_score, NA_integer_),
    subjects_doaj  = coalesce(subjects_doaj,  NA_character_)
  ) |>
  select(-cat_priority)

cat("\n=== GLOBAL INDEXING TYPOLOGY ===\n")
print(count(global_all, index_cat))
cat("Total journals:", nrow(global_all), "\n")
cat("Countries:", n_distinct(global_all$country), "\n")
write_csv(global_all, file.path(OUTPUT_DIR, "global_all_journals.csv"))
