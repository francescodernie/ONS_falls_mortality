# =============================================================================
# R/02b_single_year_deaths.R
#
# Saves deaths by SINGLE YEAR of age for falls_analysis.R Section D
# (median age at death, unassigned IMD, fall type).
#
# 02_process_deaths.R collapses age to 5-year bands, and 06_combine.R drops
# deaths with no IMD decile, so these summaries are taken from the raw ONS
# file here. Reads the same Excel file, sheets and cell ranges as
# 02_process_deaths.R.
#
#   all_cause: deaths from any cause, Year x Sex x IMD_q x Age
#   falls:     W00-W19 deaths,        Year x Sex x IMD_q x Age x icd10
#
# IMD_q uses the same decile-to-quintile mapping as 06_combine.R. Deaths with
# no IMD decile are kept as "Unassigned" so they can be counted.
#
# Standalone: can be run on its own once data-raw/deaths/ contains the ONS
# Excel file.
#   source(here::here("R", "02b_single_year_deaths.R"))
#
# Output: data/deaths_sya_summary.rds
# =============================================================================

library(tidyverse)
library(readxl)
library(here)

message("\n--- 02b_single_year_deaths.R ---")

deaths_xlsx <- here("data-raw", "deaths", "Deaths by IMD 2001-2022 FINAL.xlsx")
if (!file.exists(deaths_xlsx)) {
  stop("Deaths Excel not found at ", deaths_xlsx,
       ". Run R/01_download_deaths.R first.")
}

read_sheet <- function(sheet, last_row, sex) {
  message("Reading ", sex, " sheet...")
  read_excel(deaths_xlsx, sheet = sheet,
             range = cell_limits(c(6, 1), c(last_row, 25))) %>%
    mutate(Sex = sex)
}

sya_base <- bind_rows(
  read_sheet("1", 567283, "Total"),
  read_sheet("2", 415178, "Male"),
  read_sheet("3", 379521, "Female")
) %>%
  # Columns 4-25 are the year columns (2001-2022)
  pivot_longer(cols = 4:25, names_to = "Year", values_to = "Deaths") %>%
  mutate(
    Year       = as.integer(Year),
    Deaths     = suppressWarnings(as.numeric(Deaths)),
    # Same recoding as 02_process_deaths.R: 10 = most deprived
    IMD_decile = 11L - suppressWarnings(as.integer(`IMD decile`)),
    Age        = suppressWarnings(as.integer(parse_number(as.character(Age)))),
    icd10      = str_trim(str_to_upper(as.character(`ICD-10 code`))),
    IMD_q = case_when(
      IMD_decile %in% c(9L, 10L) ~ "Q5 (most deprived)",
      IMD_decile %in% c(7L, 8L)  ~ "Q4",
      IMD_decile %in% c(5L, 6L)  ~ "Q3",
      IMD_decile %in% c(3L, 4L)  ~ "Q2",
      IMD_decile %in% c(1L, 2L)  ~ "Q1 (least deprived)",
      TRUE                       ~ "Unassigned"
    )
  ) %>%
  filter(Year >= 2002L, !is.na(Deaths), Deaths > 0)

n_no_age <- sum(sya_base$Deaths[is.na(sya_base$Age)])
if (n_no_age > 0) message("  Deaths with unreadable age (dropped): ", n_no_age)
sya_base <- filter(sya_base, !is.na(Age))

deaths_sya <- list(
  all_cause = sya_base %>%
    group_by(Year, Sex, IMD_q, Age) %>%
    summarise(Deaths = sum(Deaths), .groups = "drop"),
  falls = sya_base %>%
    filter(substr(icd10, 1, 3) %in% paste0("W", sprintf("%02d", 0:19))) %>%
    group_by(Year, Sex, IMD_q, Age, icd10) %>%
    summarise(Deaths = sum(Deaths), .groups = "drop")
)

# ONS may top-code the oldest ages (e.g. 100+ recorded as 100). Report the
# highest age so this can be stated in the methods.
max_age <- max(deaths_sya$all_cause$Age)
message("  Highest single year of age in file: ", max_age, " (",
        scales::comma(sum(deaths_sya$all_cause$Deaths[
          deaths_sya$all_cause$Age == max_age])), " deaths at that age)")
message("  All-cause deaths, persons, 2002-2022: ",
        scales::comma(sum(deaths_sya$all_cause$Deaths[
          deaths_sya$all_cause$Sex == "Total"])))

saveRDS(deaths_sya, here("data", "deaths_sya_summary.rds"))
message("Saved: ", here("data", "deaths_sya_summary.rds"))
rm(sya_base, read_sheet)
