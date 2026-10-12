# =============================================================================
# 03_make_figures.R -- Figures 2-7 of the paper
# Input : data/global_all_journals.csv   (from 01_build_dataset.R)
# Output: output/figures/*.png (created automatically)
#
# Every figure is saved at the manuscript's text width (7 in, US Letter with
# 0.75 in margins), so the point sizes below are the sizes that appear in
# print and are consistent across figures. Insert each PNG at 7 in wide
# (Figure 2 at 5.5 in).
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(ggforce)

DIR <- "."   # <- path to this repository folder, e.g. "C:/Users/you/doaj-scopus-typology"
IN_FILE <- file.path(DIR, "data", "global_all_journals.csv")
OUT_DIR <- file.path(DIR, "output", "figures")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ---- shared sizes (pt) -------------------------------------------------------
FS_AXIS   <- 11    # axis tick labels
FS_TITLE  <- 12    # axis titles
FS_PANEL  <- 13.5  # panel (subplot) titles
FS_LEGEND <- 13    # legend text
FS_INPLOT <- 10.5  # text drawn inside plots
PT        <- 1 / .pt   # converts pt to ggplot's mm text size
FIG_W     <- 7

theme_fig <- function() {
  theme_minimal(base_size = FS_AXIS) +
    theme(
      text             = element_text(color = "black"),
      axis.text        = element_text(size = FS_AXIS, color = "black"),
      axis.title       = element_text(size = FS_TITLE, color = "black"),
      plot.title       = element_text(size = FS_PANEL, face = "bold", color = "black",
                                      margin = margin(b = 8)),
      legend.text      = element_text(size = FS_LEGEND, color = "black"),
      legend.title     = element_blank(),
      legend.position  = "bottom",
      legend.key.size  = unit(16, "pt"),
      legend.box.spacing = unit(18, "pt"),       # blank line before the legend
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

save_fig <- function(p, file, h, w = FIG_W) {
  ggsave(file.path(OUT_DIR, file), p, width = w, height = h, units = "in",
         dpi = 300, bg = "white")
  cat("saved", file, "\n")
}

# ---- data --------------------------------------------------------------------
d <- read_csv(IN_FILE, show_col_types = FALSE) |>
  mutate(
    is_tr     = str_detect(country, "^T.{1,2}rk(ey|iye)$"),       # DOAJ "Türkiye" = SJR "Turkey"
    country   = if_else(is_tr, "Turkey", country),
    region    = if_else(is_tr, "Middle East", region),
    index_cat = recode(index_cat, "DOAJ only" = "DOAJ-only", "SJR only" = "SJR-only"),
    index_cat = factor(index_cat, levels = c("DOAJ-only", "Dual-indexed", "SJR-only"))
  )

CAT_COLORS <- c("DOAJ-only" = "#2ca25f", "Dual-indexed" = "#2c7bb6", "SJR-only" = "#d7191c")

n_doaj_only <- sum(d$index_cat == "DOAJ-only")
n_dual      <- sum(d$index_cat == "Dual-indexed")
n_sjr_only  <- sum(d$index_cat == "SJR-only")
n_total     <- nrow(d)

# =============================================================================
# Figure 2 -- proportional Venn
# =============================================================================
lens_area <- function(dd, r1, r2) {
  if (dd >= r1 + r2) return(0)
  if (dd <= abs(r1 - r2)) return(pi * min(r1, r2)^2)
  a1 <- r1^2 * acos((dd^2 + r1^2 - r2^2) / (2 * dd * r1))
  a2 <- r2^2 * acos((dd^2 + r2^2 - r1^2) / (2 * dd * r2))
  a3 <- 0.5 * sqrt((-dd + r1 + r2) * (dd + r1 - r2) * (dd - r1 + r2) * (dd + r1 + r2))
  a1 + a2 - a3
}
r1 <- sqrt((n_doaj_only + n_dual) / pi)
r2 <- sqrt((n_sjr_only + n_dual) / pi)
dv <- uniroot(function(x) lens_area(x, r1, r2) - n_dual,
              lower = abs(r1 - r2) + 1e-6, upper = r1 + r2 - 1e-6)$root
cx1 <- -dv / 2; cx2 <- dv / 2
lens_mid <- ((cx2 - r2) + (cx1 + r1)) / 2      # centre of the overlap region

circ <- tibble(x0 = c(cx1, cx2), y0 = 0, r = c(r1, r2), grp = c("DOAJ", "Scopus"))
pct  <- function(n) paste0("(", round(n / n_total * 100, 1), "%)")
lab  <- tibble(
  x      = c(cx1 - r1 * 0.45, lens_mid, cx2 + r2 * 0.42),
  name   = c("DOAJ-only", "Dual-indexed", "SJR-only"),
  count  = comma(c(n_doaj_only, n_dual, n_sjr_only)),
  share  = pct(c(n_doaj_only, n_dual, n_sjr_only))
)
db_lab <- tibble(x = c(cx1, cx2), y = c(r1, r2) + 0.07 * r2,
                 text = c(paste0("DOAJ\nn = ", comma(n_doaj_only + n_dual)),
                          paste0("Scopus (SJR)\nn = ", comma(n_sjr_only + n_dual))))

fig2 <- ggplot() +
  geom_circle(data = circ, aes(x0 = x0, y0 = y0, r = r, fill = grp), alpha = 0.30, color = NA) +
  geom_circle(data = circ, aes(x0 = x0, y0 = y0, r = r, color = grp), size = 1.1, fill = NA) +
  scale_fill_manual(values = c(DOAJ = "#2ca25f", Scopus = "#d7191c")) +
  scale_color_manual(values = c(DOAJ = "#2ca25f", Scopus = "#d7191c")) +
  geom_text(data = lab, aes(x = x, y = 0.12 * r2, label = name,
                            size = if_else(name == "Dual-indexed", 11.5, 12.5) * PT),
            fontface = "bold") + scale_size_identity() +
  geom_text(data = lab, aes(x = x, y = -0.01 * r2, label = count),
            fontface = "bold", size = 14 * PT) +
  geom_text(data = lab, aes(x = x, y = -0.14 * r2, label = share), size = 12 * PT) +
  geom_text(data = db_lab, aes(x = x, y = y, label = text),
            fontface = "italic", size = 12.5 * PT, lineheight = 0.95, vjust = 0) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(40, 10, 10, 10),
        plot.background = element_rect(fill = "white", color = NA))
save_fig(fig2, "Figure2.png", h = 4.4, w = 5.5)

# =============================================================================
# Figure 3 -- DOAJ registration trajectories
# =============================================================================
reg <- d |>
  filter(index_cat %in% c("DOAJ-only", "Dual-indexed"), between(added_year, 2003, 2025)) |>
  count(added_year, index_cat) |>
  complete(added_year = 2003:2025, index_cat = factor(c("DOAJ-only", "Dual-indexed"),
           levels = levels(d$index_cat)), fill = list(n = 0)) |>
  filter(index_cat != "SJR-only") |> mutate(index_cat = droplevels(index_cat)) |>
  group_by(index_cat) |> arrange(added_year) |> mutate(cumulative = cumsum(n)) |> ungroup()

traj <- function(y, title, ylab) {
  ggplot(reg, aes(added_year, .data[[y]], color = index_cat)) +
    geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
    scale_color_manual(values = CAT_COLORS, drop = TRUE) +
    scale_x_continuous(breaks = seq(2003, 2025, 2)) +
    scale_y_continuous(labels = comma) +
    labs(title = title, x = NULL, y = paste0(ylab, "\n")) +
    theme_fig() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}
fig3 <- (traj("n", "Annual Registrations", "Journals added per year") |
         traj("cumulative", "Cumulative Registrations", "Total journals registered")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_fig(fig3, "Figure3.png", h = 4.6)

# =============================================================================
# Figure 4 -- regional composition
# =============================================================================
regional <- d |> filter(!is.na(region)) |>
  count(region, index_cat) |>
  group_by(region) |> mutate(pct = n / sum(n) * 100) |> ungroup()
reg_order <- regional |> filter(index_cat == "Dual-indexed") |> arrange(pct) |> pull(region)
regional <- regional |>
  mutate(region = factor(region, levels = reg_order),
         label  = case_when(pct >= 18 ~ paste0(round(pct), "% (", comma(n), ")"),
                            pct >= 6  ~ paste0(round(pct), "%\n(", comma(n), ")"),
                            TRUE      ~ NA_character_))

fig4 <- ggplot(regional, aes(x = region, y = pct, fill = index_cat)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 9 * PT, color = "white", fontface = "bold", lineheight = 0.85, na.rm = TRUE) +
  coord_flip() +
  scale_fill_manual(values = CAT_COLORS) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = c(0, 0)) +
  labs(x = NULL, y = "\nProportion of journals") +
  theme_fig() + theme(panel.grid.major.y = element_blank(),
                      plot.margin = margin(5, 15, 5, 5))
save_fig(fig4, "Figure4.png", h = 4.6)

# =============================================================================
# Figure 5 -- subject composition (Scopus-indexed) + coverage-span gap
# =============================================================================
areas <- d |>
  filter(index_cat %in% c("Dual-indexed", "SJR-only"), !is.na(areas)) |>
  mutate(area = str_split(areas, ";")) |> unnest(area) |>
  mutate(area = str_trim(area)) |> filter(area != "")

subj <- areas |>
  group_by(area) |>
  summarise(n = n(), pct_dual = mean(index_cat == "Dual-indexed") * 100,
            csg = median(coverage_span_adj[index_cat == "SJR-only"], na.rm = TRUE) -
                  median(coverage_span_adj[index_cat == "Dual-indexed"], na.rm = TRUE),
            .groups = "drop") |>
  filter(n >= 50) |> arrange(pct_dual)
write_csv(subj, file.path(OUT_DIR, "Figure5_data.csv"))
subj_levels <- subj$area

f5a_data <- subj |>
  mutate(`SJR-only` = 100 - pct_dual, `Dual-indexed` = pct_dual) |>
  pivot_longer(c(`Dual-indexed`, `SJR-only`), names_to = "cat", values_to = "pct") |>
  mutate(area = factor(area, levels = subj_levels),
         cat  = factor(cat, levels = c("Dual-indexed", "SJR-only")))

f5a <- ggplot(f5a_data, aes(x = area, y = pct, fill = cat)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = CAT_COLORS[c("Dual-indexed", "SJR-only")]) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = expansion(mult = c(0, 0.03))) +
  labs(title = "Indexing\nComposition", x = NULL, y = "\nProportion of journals") +
  theme_fig() + theme(axis.text.y = element_text(size = 10), panel.grid.major.y = element_blank(),
                      plot.margin = margin(5, 14, 5, 5))

f5b <- ggplot(subj |> mutate(area = factor(area, levels = subj_levels)),
              aes(x = area, y = csg)) +
  geom_segment(aes(xend = area, y = 0, yend = csg), color = "grey70", linewidth = 0.4) +
  geom_point(size = 2.2) +
  coord_flip() +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 20, 5), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Coverage-\nSpan Gap", x = NULL, y = "\nYears") +
  theme_fig() + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

fig5 <- (f5a | f5b) + plot_layout(widths = c(2.4, 1), guides = "collect") &
  theme(legend.position = "bottom")
save_fig(fig5, "Figure5.png", h = 7.6)

# =============================================================================
# Figure 6 -- temporal sequencing (dual-indexed; Scopus start before 2004 excluded)
# =============================================================================
SEQ_COLORS <- c("Scopus first" = "#d7191c", "Same year" = "#fdae61", "DOAJ first" = "#2c7bb6")
temporal <- d |>
  filter(index_cat == "Dual-indexed", !is.na(added_year), !is.na(coverage_start),
         coverage_start >= 2004) |>
  mutate(gap = added_year - coverage_start,
         sequence = factor(case_when(gap > 0 ~ "Scopus first", gap < 0 ~ "DOAJ first",
                                     TRUE ~ "Same year"), levels = names(SEQ_COLORS)))
cat("Scopus first share:", round(mean(temporal$sequence == "Scopus first") * 100, 1), "%\n")

f6a <- ggplot(temporal, aes(gap, fill = sequence)) +
  geom_histogram(binwidth = 2, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  scale_fill_manual(values = SEQ_COLORS, drop = FALSE) +
  scale_y_continuous(labels = comma) +
  labs(title = "Coverage-Start Gap", y = "Number of journals\n",
       x = "\nGap in years\n(DOAJ minus Scopus start)") +
  theme_fig()

f6b <- temporal |> filter(!is.na(region)) |>
  count(region, sequence) |> group_by(region) |> mutate(pct = n / sum(n) * 100) |> ungroup() |>
  mutate(region = factor(region, levels = reg_order)) |>
  ggplot(aes(region, pct, fill = sequence)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = SEQ_COLORS, drop = FALSE) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = c(0, 0)) +
  labs(title = "Sequence by Region", x = NULL, y = "\nProportion of journals") +
  guides(fill = "none") +
  theme_fig() + theme(panel.grid.major.y = element_blank(), plot.margin = margin(5, 16, 5, 5))

fig6 <- (f6a | f6b) + plot_layout(widths = c(1.1, 1), guides = "collect") &
  theme(legend.position = "bottom")
save_fig(fig6, "Figure6.png", h = 4.6)

# =============================================================================
# Figure 7 -- Indonesia's share of global annual DOAJ-only additions
# =============================================================================
idn <- d |>
  filter(index_cat == "DOAJ-only", between(added_year, 2015, 2025)) |>
  group_by(added_year) |>
  summarise(global = n(), idn = sum(country == "Indonesia"), .groups = "drop") |>
  mutate(share = round(idn / global * 100, 1))
avg_share <- round(mean(idn$share), 1)
# Years whose % label goes below the point (others go above); chosen so that
# no label touches the line, the dashed average, or a neighbouring label.
LABEL_BELOW <- c(2015, 2018, 2020, 2021, 2024)
idn <- idn |>
  mutate(dir = if_else(added_year %in% LABEL_BELOW, -1, 1),
         off = case_when(added_year == 2025 ~ 1.2, added_year == 2016 ~ 2.6, added_year == 2020 ~ 2.2, added_year == 2023 ~ 1.7, TRUE ~ 1.6),
         ly  = share + dir * off, vj = if_else(dir > 0, 0, 1),
         lx  = added_year + case_when(added_year == 2018 ~ -0.3, added_year == 2020 ~ -0.15, added_year == 2023 ~ 0.12, TRUE ~ 0),
         hj  = if_else(added_year == 2023, 0, 0.5))

f7a <- idn |>
  pivot_longer(c(global, idn), names_to = "s", values_to = "n") |>
  mutate(s = factor(recode(s, global = "Global DOAJ-only additions", idn = "Indonesia"),
                    levels = c("Global DOAJ-only additions", "Indonesia"))) |>
  ggplot(aes(factor(added_year), n, fill = s)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c("Global DOAJ-only additions" = "#9e9e9e", "Indonesia" = "#2ca25f")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Annual DOAJ-only Additions", x = NULL, y = "Journals per year\n") +
  theme_fig() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

f7b <- ggplot(idn, aes(added_year, share)) +
  geom_hline(yintercept = avg_share, linetype = "dashed", color = "grey45") +
  annotate("text", x = 2025.5, y = 38.5, hjust = 1,
           label = paste0("Average ", avg_share, "% (dashed)"), size = FS_INPLOT * PT, color = "grey25") +
  geom_line(color = "#2ca25f", linewidth = 1) +
  geom_point(color = "#2ca25f", size = 2.2) +
  geom_text(aes(x = lx, y = ly, label = paste0(share, "%"), vjust = vj, hjust = hj), size = FS_INPLOT * PT) +
  scale_x_continuous(breaks = 2015:2025, limits = c(2014.5, 2025.5)) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  labs(title = "Indonesia's Share", x = NULL, y = "Share of global additions (%)\n") +
  theme_fig() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig7 <- (f7a | f7b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_fig(fig7, "Figure7.png", h = 4.6)

cat("\nDone. Figures written to", OUT_DIR, "\n")
