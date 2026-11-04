# =============================================================================
# R/06_combine.R
#
# Joins deaths (numerator) to population (denominator), collapses the
# deaths IMD deciles to quintiles to match the population, then merges.
# Produces the final general-purpose infrastructure dataset.
#
# Age-standardisation follows Oldham et al. (2025) lines 993-1002:
#   - weighted mean of age-specific rates using 2013 ESP weights
#   - 95% CIs by Dobson's method (PHEindicatormethods::phe_dsr)
#
# Output: data/mortality_imd_england_2002_2022.rds
#         data/asr_imd_england_2002_2022.rds
#         data/asr_imd_england_2002_2022.csv.gz
# =============================================================================

message("\n--- 06_combine.R ---")

deaths <- readRDS(here("data", "deaths_clean.rds"))
PopFinal <- readRDS(here("data", "population_clean.rds"))

# 2013 European Standard Population (Oldham lines 42-45)
ESP <- tibble(
  Age5 = c("Under 1", "1-4", "5-9", "10-14", "15-19", "20-24",
            "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
            "85-89", "90+"),
  ESP  = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000,
           7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000)
)

# =============================================================================
# Collapse deaths from decile to quintile to match population
# Oldham lines 88-93 (applied to deaths here since population is already
# at quintile level)
# Raw IMD_decile in deaths: 1 = least deprived (after 11 - raw decile recoding
# in 02_process_deaths.R), 10 = most deprived
# Wait - recheck direction: in 02_process_deaths.R we set IMD_decile = 11 - raw
# So IMD_decile 1 there = least deprived (raw decile 10)
# But in PopFinal, raw decile 1 (most deprived) -> Q5
# We need to be consistent.
#
# RECONCILIATION:
# In the deaths file: raw `IMD decile` 1 = most deprived (ONS convention)
# Oldham line 70: IMD = 11 - `IMD decile`
#   => raw decile 1 -> IMD 10 (most deprived gets highest number)
#   => raw decile 10 -> IMD 1 (least deprived gets lowest number)
# Then Oldham lines 89-93 collapse these:
#   IMD 9,10 (most deprived) -> Q5 most deprived
#   IMD 1,2  (least deprived)-> Q1 least deprived
#
# In our 02_process_deaths.R we applied the same: IMD_decile = 11 - raw decile
# So IMD_decile 10 = most deprived, IMD_decile 1 = least deprived
#
# In PopFinal (05_build_population.R) raw decile 1 (most deprived) -> Q5
# = deciles 1,2 -> Q5; 9,10 -> Q1
# This matches Oldham exactly.
# =============================================================================

message("Collapsing deaths from decile to quintile...")

deaths_q <- deaths %>%
  # Exclude 2001: population denominator unreliable for that year
  filter(Year >= 2002L) %>%
  mutate(IMD_q = case_when(
    IMD_decile %in% c(9L, 10L) ~ "Q5 (most deprived)",
    IMD_decile %in% c(7L, 8L)  ~ "Q4",
    IMD_decile %in% c(5L, 6L)  ~ "Q3",
    IMD_decile %in% c(3L, 4L)  ~ "Q2",
    IMD_decile %in% c(1L, 2L)  ~ "Q1 (least deprived)",
    TRUE                        ~ NA_character_
  )) %>%
  filter(!is.na(IMD_q)) %>%
  group_by(Year, Sex, IMD_q, Age5, icd10) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop")

# =============================================================================
# Build population with "Total" sex added (sum of male + female)
# Follows Oldham lines 924-928
# =============================================================================

PopWithTotal <- bind_rows(
  PopFinal,
  PopFinal %>%
    group_by(Age5, Year, IMD_q) %>%
    summarise(Pop = sum(Pop), .groups = "drop") %>%
    mutate(Sex = "Total")
)

# Add "Overall" IMD group (all quintiles combined)
PopWithOverall <- bind_rows(
  PopWithTotal,
  PopWithTotal %>%
    group_by(Age5, Sex, Year) %>%
    summarise(Pop = sum(Pop), .groups = "drop") %>%
    mutate(IMD_q = "Overall")
)

# =============================================================================
# Join deaths to population
# =============================================================================

message("Joining deaths to population...")

mortality <- deaths_q %>%
  # Add "Overall" IMD category to deaths too
  bind_rows(
    deaths_q %>%
      group_by(Year, Sex, Age5, icd10) %>%
      summarise(Deaths = sum(Deaths), .groups = "drop") %>%
      mutate(IMD_q = "Overall")
  ) %>%
  left_join(PopWithOverall, by = c("Year", "Sex", "IMD_q", "Age5")) %>%
  mutate(mx = Deaths * 100000 / Pop)

# =============================================================================
# Age-standardised rates — Dobson's method (PHE recommended)
#
# PHE Technical Guide: Confidence Intervals (2018) specifies Dobson's method
# (Appendix 4) for directly standardised rates. This supersedes the simpler
# Byar approximation used in Oldham et al. (2025).
#
# Dobson's method correctly propagates age-specific uncertainty through the
# ESP weights via Var(DSR), whereas the simple Byar approximation applies
# uncertainty to the total count without accounting for the weighted structure.
# For large counts the difference is negligible; for sparse cells (e.g. rare
# causes in younger age bands) Dobson's method gives more accurate coverage.
#
# Implementation uses PHEindicatormethods::phe_dsr(), which is the PHE/OHID
# standard implementation used in Fingertips indicators.
#
# Reference:
#   Dobson AJ et al. Confidence intervals for weighted sums of Poisson
#   parameters. Statistics in Medicine. 1991;10:457-462.
#   PHE Technical Guide: Confidence Intervals. 2018. Appendix 4.
# =============================================================================

message("Computing age-standardised rates (Dobson's method)...")

if (!requireNamespace("PHEindicatormethods", quietly = TRUE)) {
  message("Installing PHEindicatormethods...")
  install.packages("PHEindicatormethods")
}
library(PHEindicatormethods)

# phe_dsr() requires: x (observed deaths per age band), n (population per
# age band), stdpop (ESP weights), and grouping variables.
# It returns the DSR per 100,000 with Dobson 95% CIs.

ASRates <- mortality %>%
  left_join(ESP, by = "Age5") %>%
  group_by(Year, Sex, IMD_q, icd10) %>%
  phe_dsr(
    x          = Deaths,
    n          = Pop,
    stdpop     = ESP,
    stdpoptype = "field",
    multiplier = 100000,
    confidence = 0.95,
    type       = "full"
  ) %>%
  rename(
    mx      = value,
    LowerCI = lowercl,
    UpperCI = uppercl
  ) %>%
  select(Year, Sex, IMD_q, icd10, mx, total_count, total_pop,
         LowerCI, UpperCI) %>%
  rename(Deaths = total_count, Pop = total_pop)

# =============================================================================
# Save outputs
# =============================================================================

# Full cell-level dataset (deaths + pop by age5 x icd10 x quintile x year x sex)
saveRDS(mortality, here("data", "mortality_imd_england_2002_2022.rds"))
message("Saved: data/mortality_imd_england_2002_2022.rds (",
        nrow(mortality), " rows)")

# Age-standardised rates (ready for analysis and plotting)
saveRDS(ASRates, here("data", "asr_imd_england_2002_2022.rds"))
data.table::fwrite(ASRates, here("data", "asr_imd_england_2002_2022.csv.gz"))
message("Saved: data/asr_imd_england_2002_2022.rds (",
        nrow(ASRates), " rows)")

# =============================================================================
# Data dictionary
# =============================================================================

dict <- "
DATA DICTIONARY
===============

mortality_imd_england_2002_2022.rds  [cell-level]
──────────────────────────────────────────────────
Year     <int>  Registration year (2002-2022)
Sex      <chr>  'Male' | 'Female' | 'Total'
IMD_q    <chr>  'Q1 (least deprived)' ... 'Q5 (most deprived)' | 'Overall'
Age5     <fct>  5-year ESP band: 'Under 1', '1-4', ..., '85-89', '90+'
icd10    <chr>  4-character ICD-10 underlying cause of death
Deaths   <dbl>  Registered deaths
Pop      <dbl>  Mid-year population denominator
mx       <dbl>  Crude age-specific rate per 100,000

asr_imd_england_2002_2022.rds  [age-standardised]
──────────────────────────────────────────────────
Year     <int>  Registration year
Sex      <chr>  'Male' | 'Female' | 'Total'
IMD_q    <chr>  IMD quintile or 'Overall'
icd10    <chr>  4-character ICD-10 code
mx       <dbl>  Age-standardised rate per 100,000 (2013 ESP, Dobson's method)
Deaths   <dbl>  Total deaths (all ages)
Pop      <dbl>  Total population
LowerCI  <dbl>  Lower 95% CI (Dobson's method via PHEindicatormethods::phe_dsr)
UpperCI  <dbl>  Upper 95% CI (Dobson's method via PHEindicatormethods::phe_dsr)

METHODOLOGY
───────────
Deaths:      ONS ad-hoc 1355, England only, 2002-2022
             (2001 excluded: unreliable quinary population denominator)
Population:  LSOA-level ONS MYEs, joined to year-appropriate IMD version
IMD mapping: 2002-2005=IMD2004; 2006-2008=IMD2007; 2009-2012=IMD2010;
             2013-2017=IMD2015; 2018-2022=IMD2019
Quintiles:   Q5=most deprived (deciles 1-2), Q1=least deprived (deciles 9-10)
Std. pop:    2013 European Standard Population
ASR method:  Direct standardisation, phe_dsr() [PHEindicatormethods package]
CIs:         Dobson's method (PHE recommended for directly standardised rates)
             Reference: Dobson AJ et al. Stat Med. 1991;10:457-462
             PHE Technical Guide: Confidence Intervals. 2018. Appendix 4.

REFERENCES
──────────
Oldham M et al. Trends in alcohol-specific deaths in England, 2001-22.
  Lancet Public Health 2025. doi:10.1016/S2468-2667(25)00047-7
Dobson AJ et al. Confidence intervals for weighted sums of Poisson parameters.
  Statistics in Medicine. 1991;10:457-462.
PHE Technical Guide: Confidence Intervals. Public Health Data Science. 2018.
"

writeLines(dict, here("data", "data_dictionary.txt"))
message("Saved: data/data_dictionary.txt")
message("\n✓ 06_combine.R complete.")
