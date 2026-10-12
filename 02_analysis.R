# =============================================================================
# 02_analysis.R -- every number, table and test reported in the paper
# Input : data/global_all_journals.csv   (from 01_build_dataset.R)
# Output: output/tables/*.csv and output/analysis_log.txt
# Run   : set DIR below, then source this file or paste it into the R console.
# =============================================================================

library(tidyverse)

DIR <- "."   # <- path to this repository folder, e.g. "C:/Users/you/doaj-scopus-typology"
IN_FILE <- file.path(DIR, "data", "global_all_journals.csv")
OUT_DIR <- file.path(DIR, "output", "tables")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
LOG <- file.path(DIR, "output", "analysis_log.txt")
sink(LOG, split = TRUE)

hdr  <- function(x) cat("\n=====", x, "=====\n")
pc   <- function(x, d = 1) round(100 * x, d)
rbis <- function(y, g, lv) {                       # Mann-Whitney U + rank-biserial r
  t  <- wilcox.test(y[g == lv[1]], y[g == lv[2]], exact = FALSE)
  n1 <- sum(g == lv[1] & !is.na(y)); n2 <- sum(g == lv[2] & !is.na(y))
  c(p = t$p.value, r = unname(1 - 2 * t$statistic / (n1 * n2)))
}

# ---- data --------------------------------------------------------------------
d <- read_csv(IN_FILE, show_col_types = FALSE)
# DOAJ spells the country "Türkiye", SJR "Turkey"; keep one name and SJR's region
is_tr <- str_detect(d$country, "^T.{1,2}rk(ey|iye)$")
d$country[is_tr] <- "Turkey"
d$region[is_tr]  <- "Middle East"
d <- d |> mutate(index_cat = recode(index_cat, "DOAJ only" = "DOAJ-only", "SJR only" = "SJR-only"))
reg <- d |> filter(in_doaj) |> mutate(dual = as.integer(index_cat == "Dual-indexed"))

# =============================================================================
hdr("2.1 / 3.1  Dataset and global overview")
tot <- count(d, index_cat) |> mutate(pct = pc(n / sum(n)))
print(tot)
cat("Total journals:", nrow(d), "| countries:", n_distinct(d$country), "\n")
cat("DOAJ-registered:", nrow(reg), "| Scopus (SJR) journals:", sum(d$in_sjr), "\n")
cat("Dual-indexed share of DOAJ-registered:", pc(mean(reg$dual)), "%\n")

hdr("2.1  Edition offset: sensitivity excluding DOAJ registrations in 2026")
cat("Journals added to DOAJ in 2026:", sum(reg$added_year == 2026, na.rm = TRUE), "\n")
r25 <- reg |> filter(added_year <= 2025)
cat("Overlap rate without 2026:", pc(mean(r25$dual)), "% | Indonesia share of DOAJ-only:",
    pc(mean(r25$country[r25$dual == 0] == "Indonesia")), "%\n")

hdr("2.1  ISSN-linkage validation (title agreement for dual-indexed journals)")
du  <- d |> filter(index_cat == "Dual-indexed")
nrm <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("\\(.*?\\)", "", x))
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9 ]", " ", x)))
}
a <- nrm(du$title_doaj); b <- nrm(du$title_sjr)
sim <- 1 - mapply(function(u, v) adist(u, v)[1, 1], a, b) / pmax(nchar(a), nchar(b), 1)
contained <- mapply(function(u, v) nchar(u) > 3 & nchar(v) > 3 &
                      (grepl(u, v, fixed = TRUE) | grepl(v, u, fixed = TRUE)), a, b)
du$title_match <- sim >= 0.8 | contained
cat("Matching titles:", sum(du$title_match), "of", nrow(du), "(", pc(mean(du$title_match)), "%)\n")
set.seed(2026)
audit <- du |> filter(!title_match) |> slice_sample(n = 40) |>
  select(issn_norm, title_doaj, title_sjr, country_doaj, country_sjr)
write_csv(audit, file.path(OUT_DIR, "linkage_audit_sample.csv"))
cat("Audit sample of 40 non-matching pairs written for manual inspection\n")

# =============================================================================
hdr("3.1  Registration trajectories (Figure 3)")
traj <- reg |> filter(between(added_year, 2003, 2025)) |> count(added_year, index_cat) |>
  pivot_wider(names_from = index_cat, values_from = n, values_fill = 0)
print(traj |> filter(added_year %in% c(2014, 2015, 2021, 2025)))

# =============================================================================
hdr("3.2  Regional composition (Figure 4)")
regional <- d |> filter(!is.na(region)) |> count(region, index_cat) |>
  group_by(region) |> mutate(total = sum(n), pct = pc(n / total)) |> ungroup()
print(regional |> select(-n) |> pivot_wider(names_from = index_cat, values_from = pct), width = Inf)
print(regional |> filter(index_cat == "Dual-indexed") |> select(region, n, total))
cat("Journals with a region:", sum(!is.na(d$region)), "| without:", sum(is.na(d$region)), "\n")
write_csv(regional, file.path(OUT_DIR, "regional_composition.csv"))

hdr("2.2 / 3.2  Country DIG and regional DIG")
cty <- d |> group_by(country, region) |>
  summarise(n_total = n(), doaj_only = sum(index_cat == "DOAJ-only"),
            dual = sum(index_cat == "Dual-indexed"), sjr_only = sum(index_cat == "SJR-only"),
            .groups = "drop") |>
  mutate(doaj_reg = doaj_only + dual,
         dig = (doaj_only - dual) / doaj_reg,
         dual_rate_all = dual / n_total)
dig15 <- cty |> filter(doaj_reg >= 15, !is.na(region))
cat("Countries meeting the 15-journal threshold:", nrow(dig15), "\n")
print(dig15 |> group_by(region) |> summarise(mean_dig = round(mean(dig), 2), countries = n()) |>
        arrange(desc(mean_dig)))
kw <- kruskal.test(dig ~ factor(region), data = dig15)
cat(sprintf("Kruskal-Wallis on country DIG: H(%d) = %.1f, p = %.2g\n", kw$parameter, kw$statistic, kw$p.value))
sp <- cor.test(dig15$dig, dig15$dual_rate_all, method = "spearman", exact = FALSE)
cat(sprintf("Spearman DIG vs. three-category dual rate: rho = %.2f, p = %.2g\n", sp$estimate, sp$p.value))
print(dig15 |> filter(country %in% c("Indonesia", "Yemen", "Viet Nam", "Estonia", "Slovenia", "Croatia")) |>
        select(country, doaj_reg, dig))
write_csv(cty, file.path(OUT_DIR, "country_summary.csv"))

# =============================================================================
hdr("3.3  Table 1: bibliometric metrics, dual-indexed vs. SJR-only")
q <- d |> filter(index_cat %in% c("Dual-indexed", "SJR-only"), !is.na(sjr_score))
tbl1 <- q |> group_by(index_cat) |>
  summarise(n = n(), median_sjr = median(sjr_score), median_h = median(h_index, na.rm = TRUE),
            pct_q1q2 = pc(mean(quartile %in% c("Q1", "Q2"))),
            median_cites = median(cites_per_doc, na.rm = TRUE),
            median_docs = median(docs_2025, na.rm = TRUE))
print(tbl1, width = Inf); write_csv(tbl1, file.path(OUT_DIR, "table1_metrics.csv"))
lv <- c("Dual-indexed", "SJR-only")
for (v in c("sjr_score", "h_index", "docs_2025", "cites_per_doc")) {
  res <- rbis(q[[v]], q$index_cat, lv)
  cat(sprintf("%-14s Mann-Whitney p = %.3g, rank-biserial r = %.2f\n", v, res["p"], res["r"]))
}

# =============================================================================
hdr("3.4  Subject areas (Figure 5)")
subj <- d |> filter(index_cat %in% lv, !is.na(areas)) |>
  mutate(area = str_split(areas, ";")) |> unnest(area) |> mutate(area = str_trim(area)) |>
  filter(area != "") |> group_by(area) |>
  summarise(n = n(), pct_dual = pc(mean(index_cat == "Dual-indexed")),
            csg = median(coverage_span_adj[index_cat == "SJR-only"], na.rm = TRUE) -
                  median(coverage_span_adj[index_cat == "Dual-indexed"], na.rm = TRUE),
            .groups = "drop") |>
  filter(n >= 50) |> arrange(desc(pct_dual))
cat("Subject areas with >= 50 journals:", nrow(subj), "\n")
print(bind_rows(head(subj, 3), tail(subj, 3)))
cat("CSG range:", min(subj$csg), "to", max(subj$csg), "\n")
write_csv(subj, file.path(OUT_DIR, "subject_areas.csv"))

# =============================================================================
hdr("3.5  Coverage span and temporal sequencing")
sp_d <- d |> filter(index_cat %in% lv, !is.na(coverage_span_adj), coverage_span_adj >= 0)
print(sp_d |> group_by(index_cat) |> summarise(median_span = median(coverage_span_adj)))
res <- rbis(sp_d$coverage_span_adj, sp_d$index_cat, lv)
cat(sprintf("Mann-Whitney coverage span: p = %.3g, r = %.2f\n", res["p"], res["r"]))
mx <- max(sp_d$coverage_span_adj)
cat("Maximum span:", mx, "| SJR-only at maximum:",
    pc(mean(sp_d$coverage_span_adj[sp_d$index_cat == "SJR-only"] == mx)), "%\n")

seqd <- d |> filter(index_cat == "Dual-indexed", !is.na(added_year), !is.na(coverage_start))
cat("Dual-indexed excluded for Scopus start before 2004:", sum(seqd$coverage_start < 2004), "\n")
seqd <- seqd |> filter(coverage_start >= 2004) |>
  mutate(gap = added_year - coverage_start,
         sequence = case_when(gap > 0 ~ "Scopus first", gap < 0 ~ "DOAJ first", TRUE ~ "Same year"))
cat("Scopus coverage start before DOAJ registration:", pc(mean(seqd$sequence == "Scopus first")), "%\n")
print(seqd |> filter(!is.na(region)) |> count(region, sequence) |>
        group_by(region) |> mutate(pct = pc(n / sum(n))) |> select(-n) |>
        pivot_wider(names_from = sequence, values_from = pct))

# =============================================================================
hdr("3.6  Concentration benchmark")
conc <- function(x) {
  t <- sort(table(x), decreasing = TRUE); s <- as.numeric(t) / sum(t)
  tibble(n = sum(t), top1 = names(t)[1], top1_pct = pc(s[1]),
         top10_pct = pc(sum(s[1:10])), hhi = round(10000 * sum(s^2)))
}
print(bind_rows(`DOAJ-registered` = conc(reg$country), `DOAJ-only` = conc(reg$country[reg$dual == 0]),
                .id = "set"))
p_glob <- mean(reg$dual)
oe <- reg |> group_by(country) |> summarise(doaj_reg = n(), dual = sum(dual), .groups = "drop") |>
  mutate(doaj_only = doaj_reg - dual, expected = doaj_reg * (1 - p_glob),
         ratio = round(doaj_only / expected, 2), excess = doaj_only - expected,
         dig = round((doaj_only - dual) / doaj_reg, 3))
pos <- sum(oe$excess[oe$excess > 0])
oe <- oe |> mutate(share_of_excess = if_else(excess > 0, pc(excess / pos), NA_real_)) |> arrange(desc(excess))
cat("Global excess:", round(pos), "| top-5 share:", pc(sum(head(oe$excess, 5)) / pos), "%\n")
print(head(oe, 6))
write_csv(oe, file.path(OUT_DIR, "country_observed_expected.csv"))

hdr("3.6  Table 2: top 10 countries by DOAJ-only count")
top10 <- oe |> arrange(desc(doaj_only)) |> slice(1:10) |>
  mutate(pct_global = pc(doaj_only / sum(reg$dual == 0)))
csg_c <- map_dfr(top10$country, function(cn) {
  x  <- sp_d |> filter(country == cn)
  y1 <- x$coverage_span_adj[x$index_cat == "SJR-only"]; y2 <- x$coverage_span_adj[x$index_cat == "Dual-indexed"]
  tibble(country = cn, csg = median(y1) - median(y2), p = wilcox.test(y1, y2, exact = FALSE)$p.value)
}) |> mutate(p_holm = p.adjust(p, "holm"))
tbl2 <- top10 |> select(country, dig, doaj_only, pct_global, ratio) |> left_join(csg_c, by = "country")
print(tbl2, width = Inf); write_csv(tbl2, file.path(OUT_DIR, "table2_top10.csv"))
cat("Countries with DOAJ-only journals:", n_distinct(reg$country[reg$dual == 0]), "\n")

hdr("3.6  Indonesia profile")
print(cty |> filter(country %in% c("Indonesia", "United Kingdom", "United States")) |>
        select(country, n_total, doaj_only, dual, sjr_only) |>
        mutate(across(c(doaj_only, dual, sjr_only), ~ pc(.x / n_total), .names = "{.col}_pct")))

# =============================================================================
hdr("3.7  Cohort rates and logistic regression (DOAJ-registered journals)")
reg <- reg |>
  mutate(idn = as.integer(country == "Indonesia"),
         cohort = cut(added_year, c(2000, 2010, 2014, 2017, 2020, 2023, 2026),
                      labels = c("2002-10", "2011-14", "2015-17", "2018-20", "2021-23", "2024-26")),
         s1 = str_trim(str_remove(str_remove(coalesce(subjects_doaj, "Unknown"), "\\|.*$"), ":.*$")),
         apc = as.integer(apc_yn == "Yes"), dbl = as.integer(dbl_review %in% TRUE))
keep <- names(which(table(reg$s1) >= 100))
reg <- reg |> mutate(subj = factor(if_else(s1 %in% keep, s1, "Other")))
print(reg |> group_by(cohort, idn) |> summarise(n = n(), rate = pc(mean(dual)), .groups = "drop") |>
        pivot_wider(names_from = idn, values_from = c(n, rate)))
rest <- reg |> filter(idn == 0) |> group_by(cohort) |> summarise(r = mean(dual))
cat("Indonesia dual-indexed: observed", sum(reg$dual[reg$idn == 1]), "| expected at rest-of-world cohort rates",
    round(sum(rest$r[match(reg$cohort[reg$idn == 1], rest$cohort)])), "\n")
cat("Median DOAJ registration year: Indonesia", median(reg$added_year[reg$idn == 1]),
    "| elsewhere", median(reg$added_year[reg$idn == 0]), "\n")
or <- function(f, v = "idn", data = reg) {
  m <- glm(f, data = data, family = binomial); ci <- exp(confint.default(m)[v, ])
  sprintf("OR = %.3f (95%% CI %.3f-%.3f), p = %.2g", exp(coef(m)[v]), ci[1], ci[2], summary(m)$coefficients[v, 4])
}
cat("Unadjusted:     ", or(dual ~ idn), "\n")
cat("Cohort only:    ", or(dual ~ idn + cohort), "\n")
cat("Fully adjusted: ", or(dual ~ idn + cohort + subj + apc + dbl), "\n")
for (cn in c("Brazil", "Iran", "Turkey", "Egypt")) {
  reg$x <- as.integer(reg$country == cn)
  cat(sprintf("%-8s adjusted: %s\n", cn, or(dual ~ x + cohort + subj + apc + dbl, "x")))
}
cat("Adjusted OR for Indonesia without 2026 registrations:",
    or(dual ~ idn + cohort + subj + apc + dbl, data = filter(reg, added_year <= 2025)), "\n")
ct <- table(reg$index_cat, reg$apc_yn); print(ct)
cs <- chisq.test(ct, correct = FALSE)
cat(sprintf("APC charging by category: chi2(1) = %.1f, p = %.2g, Cramer's V = %.2f\n",
            cs$statistic, cs$p.value, sqrt(cs$statistic / sum(ct))))

hdr("3.7  Indonesia's share of annual DOAJ-only additions (Figure 7)")
idn_y <- reg |> filter(dual == 0, between(added_year, 2015, 2025)) |> group_by(added_year) |>
  summarise(global = n(), indonesia = sum(country == "Indonesia"), share = pc(indonesia / global))
print(idn_y); cat("Average share 2015-2025:", round(mean(idn_y$share), 1), "%\n")
write_csv(idn_y, file.path(OUT_DIR, "indonesia_annual_share.csv"))

sink()
cat("\nDone. Log:", LOG, "\n")
