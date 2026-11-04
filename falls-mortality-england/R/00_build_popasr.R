# =============================================================================
# R/00_build_popasr.R
#
# Builds PopForASR: a complete population lookup table covering all
# combinations of Year x Sex x IMD_q x Age5.
#
# PURPOSE:
#   When computing ASRs for a specific cause of death (e.g. alcohol, falls,
#   COPD), filtering the mortality dataset to that cause and deriving
#   population via first(Pop) or sum(Pop) drops any age-sex-quintile-year
#   cells where zero deaths occurred. This biases the ASR upward because
#   the zero-death cells (which have population but no events) are excluded
#   from the weighted mean denominator.
#
#   PopForASR solves this by providing the complete set of denominators
#   independently of the deaths data. Analysis scripts should right_join
#   their aggregated deaths to PopForASR, then replace_na(Deaths, 0),
#   before computing age-specific rates and age-standardising.
#
# USAGE:
#   source(here("R", "00_build_popasr.R"))
#   # PopForASR is now available in the environment
#
# This replicates Oldham et al. (2025) AnalysisCode.R lines 101-104:
#   spread(Age5, Deaths) %>%
#   mutate(across(.cols=c(5:24), ~ if_else(is.na(.x), 0, .x))) %>%
#   gather(Age5, Deaths, c(5:24))
#
# VALIDATED: April 2026 — population totals within ~2% of ONS England
# published mid-year estimates throughout 2002-2022.
# =============================================================================

PopFinal <- readRDS(here("data", "population_clean.rds"))

PopForASR <- bind_rows(
  # Male and Female by quintile (from PopFinal directly)
  PopFinal,
  # Total sex by quintile
  PopFinal %>%
    group_by(Age5, Year, IMD_q) %>%
    summarise(Pop = sum(Pop), .groups = "drop") %>%
    mutate(Sex = "Total"),
  # Male by Overall
  PopFinal %>%
    filter(Sex == "Male") %>%
    group_by(Age5, Year, Sex) %>%
    summarise(Pop = sum(Pop), .groups = "drop") %>%
    mutate(IMD_q = "Overall"),
  # Female by Overall
  PopFinal %>%
    filter(Sex == "Female") %>%
    group_by(Age5, Year, Sex) %>%
    summarise(Pop = sum(Pop), .groups = "drop") %>%
    mutate(IMD_q = "Overall"),
  # Total sex by Overall
  PopFinal %>%
    group_by(Age5, Year) %>%
    summarise(Pop = sum(Pop), .groups = "drop") %>%
    mutate(Sex = "Total", IMD_q = "Overall")
)

message("PopForASR built: ", nrow(PopForASR), " rows | ",
        n_distinct(PopForASR$Year), " years | ",
        n_distinct(PopForASR$Sex), " sex categories | ",
        n_distinct(PopForASR$IMD_q), " IMD groups")
