# Analysis protocol

1. **Sources.** DOAJ journal CSV (2026 edition, 22,694 records) and SJR export (2025 edition), filtered to `Type == "journal"` (30,486 records). Both accessed 17 April 2026.
2. **Linkage.** Print and electronic ISSNs from both sources are normalized (hyphens and spaces removed) and joined with a full outer join. Records are deduplicated to one row per ISSN, then per title, keeping the dual-indexed classification when duplicates disagree. Country names are harmonized between sources (e.g., DOAJ "Türkiye" = SJR "Turkey").
3. **Categories.** DOAJ-only (DOAJ, not SJR), dual-indexed (both), SJR-only (SJR, not DOAJ).
4. **Dual-Indexing Gap (DIG).** For a country, DIG = (DOAJ-only − dual) / (DOAJ-only + dual), equal to 1 − 2p, where p is the dual-indexed share of its DOAJ-registered journals. Reported for countries with ≥ 15 DOAJ-registered journals; regional DIG is the unweighted mean of country values.
5. **Concentration.** Expected DOAJ-only count = DOAJ-registered count × (1 − global overlap rate). Excess = observed − expected; each country's share of the summed positive excess is reported, with top-10 shares and HHI for DOAJ-only and all DOAJ-registered journals as a benchmark.
6. **Coverage span.** Scopus coverage end minus start, with start truncated at 2004. Coverage-Span Gap (CSG) = median span (SJR-only) − median span (dual-indexed), by subject area and country; within-country Mann–Whitney tests are Holm-corrected.
7. **Sequencing.** For dual-indexed journals with Scopus coverage starting in 2004 or later, DOAJ registration year minus Scopus coverage start year.
8. **Robustness.** Registration-cohort rates; logistic regression of dual indexing on an Indonesia indicator with cohort, first DOAJ subject class, APC charging and double-blind review; exclusion of 2026 DOAJ registrations; title-agreement validation of ISSN matches.
