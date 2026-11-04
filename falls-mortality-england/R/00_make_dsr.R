# =============================================================================
# R/00_make_dsr.R
#
# Shared helper for computing directly standardised rates using Dobson's method
# via PHEindicatormethods::calculate_dsr().
#
# PHE recommendation (Technical Guide: Confidence Intervals, 2018, Appendix 4):
#   Use Dobson's method for directly standardised rates. This correctly
#   propagates age-specific uncertainty through the ESP weights via Var(DSR),
#   unlike the simple Byar approximation which applies uncertainty to the
#   total count without accounting for the weighted structure.
#
# NOTE ON NAMESPACE:
#   segmented loads MASS which masks dplyr::select and dplyr::rename.
#   All dplyr calls are explicitly namespaced to avoid this conflict.
#
# USAGE:
#   source(here("R", "00_build_popasr.R"))
#   source(here("R", "00_make_dsr.R"))
#   my_asr <- make_dsr(my_cell, group_vars = c("Year", "Sex", "IMD_q"))
#
# RETURNS:
#   Tibble with columns: [group_vars], mx, Deaths, Pop, LowerCI, UpperCI
#
# REFERENCES:
#   Dobson AJ et al. Stat Med. 1991;10:457-462
#   PHE Technical Guide: Confidence Intervals. 2018. Appendix 4.
# =============================================================================

if (!requireNamespace("PHEindicatormethods", quietly = TRUE)) {
  message("Installing PHEindicatormethods...")
  install.packages("PHEindicatormethods")
}
library(PHEindicatormethods)

# 2013 European Standard Population (all 20 age bands)
ESP <- tibble::tibble(
  Age5 = c("Under 1", "1-4", "5-9", "10-14", "15-19", "20-24",
            "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
            "85-89", "90+"),
  ESP_wt = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000,
             7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000)
)

make_dsr <- function(data,
                     group_vars  = c("Year", "Sex", "IMD_q"),
                     multiplier  = 100000,
                     confidence  = 0.95) {

  # Join ESP weights — ESP_wt column required by calculate_dsr()
  data <- dplyr::left_join(data, ESP, by = "Age5")

  # Compute DSR via Dobson's method
  result <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    calculate_dsr(
      x          = Deaths,
      n          = Pop,
      stdpop     = ESP_wt,
      multiplier = multiplier,
      confidence = confidence,
      type       = "full"
    )

  # Defensive rename: handles both phe_dsr and calculate_dsr column naming
  # (varies by PHEindicatormethods version).
  # All calls explicitly namespaced to avoid MASS::rename masking.
  nms <- names(result)

  # value / dsr -> mx
  if ("value" %in% nms)           result <- dplyr::rename(result, mx = value)
  else if ("dsr" %in% nms)        result <- dplyr::rename(result, mx = dsr)

  # lowercl / lower_cl / lower -> LowerCI
  if ("lowercl" %in% nms)         result <- dplyr::rename(result, LowerCI = lowercl)
  else if ("lower_cl" %in% nms)   result <- dplyr::rename(result, LowerCI = lower_cl)
  else if ("lower" %in% nms)      result <- dplyr::rename(result, LowerCI = lower)

  # uppercl / upper_cl / upper -> UpperCI
  if ("uppercl" %in% nms)         result <- dplyr::rename(result, UpperCI = uppercl)
  else if ("upper_cl" %in% nms)   result <- dplyr::rename(result, UpperCI = upper_cl)
  else if ("upper" %in% nms)      result <- dplyr::rename(result, UpperCI = upper)

  # total_count / count -> Deaths
  if ("total_count" %in% nms)     result <- dplyr::rename(result, Deaths = total_count)
  else if ("count" %in% nms)      result <- dplyr::rename(result, Deaths = count)

  # total_pop / population -> Pop
  if ("total_pop" %in% nms)       result <- dplyr::rename(result, Pop = total_pop)
  else if ("population" %in% nms) result <- dplyr::rename(result, Pop = population)

  # Return only the columns we need — fully namespaced to avoid MASS masking
  result <- dplyr::select(result, dplyr::all_of(group_vars),
                           mx, Deaths, Pop, LowerCI, UpperCI)

  return(result)
}

message("make_dsr() loaded — Dobson's method via PHEindicatormethods::calculate_dsr()")
