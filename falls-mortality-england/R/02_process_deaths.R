# =============================================================================
# R/02_process_deaths.R
#
# Reads ONS ad-hoc 1355 Excel file and produces a clean long-format deaths
# table. Structure follows Oldham et al. (2025) AnalysisCode.R lines 55-104
# exactly, but generalised to retain all ICD-10 codes rather than filtering
# to alcohol-specific causes.
#
# The Excel has 3 sheets: persons (sheet 1), males (sheet 2), females (sheet 3)
# Each sheet is wide-format: rows = ICD-10 x IMD decile x age combinations,
# columns 4-25 = years 2001-2022.
#
# IMD decile encoding in the raw file: 1 = least deprived, 10 = most deprived
# (confirmed from Oldham code line 70: IMD = 11 - `IMD decile`)
# We retain deciles here and collapse to quintiles in 05_build_population.R
# to ensure consistent direction with the population quintile assignment.
#
# Output: data/deaths_clean.rds
# =============================================================================

message("\n--- 02_process_deaths.R ---")

deaths_xlsx <- here("data-raw", "deaths", "Deaths by IMD 2001-2022 FINAL.xlsx")

if (!file_exists(deaths_xlsx)) {
  stop("Deaths Excel not found. Run 01_download_deaths.R first.")
}

# --- Read three sheets (persons, male, female) --------------------------------
# Cell ranges from Oldham AnalysisCode.R lines 55-65
message("Reading persons sheet...")
rawpersons <- read_excel(
  deaths_xlsx,
  sheet = "1",
  range = cell_limits(c(6, 1), c(567283, 25))
) %>%
  mutate(Sex = "Total")

message("Reading males sheet...")
rawmale <- read_excel(
  deaths_xlsx,
  sheet = "2",
  range = cell_limits(c(6, 1), c(415178, 25))
) %>%
  mutate(Sex = "Male")

message("Reading females sheet...")
rawfemale <- read_excel(
  deaths_xlsx,
  sheet = "3",
  range = cell_limits(c(6, 1), c(379521, 25))
) %>%
  mutate(Sex = "Female")

# --- Combine and pivot to long format ----------------------------------------
# Follows Oldham lines 67-76
message("Combining and reshaping...")

rawdata <- bind_rows(rawpersons, rawmale, rawfemale) %>%
  # Columns 4-25 are the year columns (2001-2022)
  gather(Year, Deaths, c(4:25)) %>%
  mutate(
    Year = as.integer(Year),
    # The raw deaths file uses standard ONS convention:
    #   raw `IMD decile` 1 = MOST deprived, 10 = LEAST deprived
    # Oldham et al. apply 11 - `IMD decile` (line 70 of their code) to flip
    # this so that higher numbers = more deprived in their working variable:
    #   After recoding: IMD_decile 10 = most deprived, 1 = least deprived
    # This is the direction used for quintile collapse in 06_combine.R:
    #   IMD_decile 9-10 -> Q5 (most deprived); 1-2 -> Q1 (least deprived)
    # NOTE: the IMD source files (04_download_imd.R) also use decile 1 = most
    # deprived, so the quintile mappings in 05_build_population.R and
    # 06_combine.R use opposite directions for population vs deaths — see
    # comments in those scripts.
    IMD_decile = 11L - as.integer(`IMD decile`)
  )

# --- Collapse single year of age to 5-year ESP bands -------------------------
# Follows Oldham lines 94-104
message("Collapsing to 5-year age bands...")

deaths_clean <- rawdata %>%
  mutate(
    Age5 = case_when(
      Age < 1   ~ "Under 1",
      Age < 5   ~ "1-4",
      Age < 10  ~ "5-9",
      Age < 15  ~ "10-14",
      Age < 20  ~ "15-19",
      Age < 25  ~ "20-24",
      Age < 30  ~ "25-29",
      Age < 35  ~ "30-34",
      Age < 40  ~ "35-39",
      Age < 45  ~ "40-44",
      Age < 50  ~ "45-49",
      Age < 55  ~ "50-54",
      Age < 60  ~ "55-59",
      Age < 65  ~ "60-64",
      Age < 70  ~ "65-69",
      Age < 75  ~ "70-74",
      Age < 80  ~ "75-79",
      Age < 85  ~ "80-84",
      Age < 90  ~ "85-89",
      Age >= 90 ~ "90+"
    )
  ) %>%
  # Clean ICD-10 codes
  rename(icd10 = `ICD-10 code`) %>%
  mutate(
    icd10 = str_trim(str_to_upper(as.character(icd10)))
  ) %>%
  # Remove rows with missing deaths or ICD-10
  filter(!is.na(Deaths), !is.na(icd10), !is.na(Age5)) %>%
  # Aggregate to year x sex x IMD decile x age5 x ICD-10
  group_by(Year, Sex, IMD_decile, Age5, icd10) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  # Apply factor ordering to age bands
  mutate(
    Age5 = factor(Age5, levels = c(
      "Under 1", "1-4", "5-9", "10-14", "15-19", "20-24",
      "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
      "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
      "85-89", "90+"
    ))
  )

# --- Diagnostics --------------------------------------------------------------
message("\nDeaths file summary:")
message("  Rows:          ", nrow(deaths_clean))
message("  Year range:    ", min(deaths_clean$Year), "-", max(deaths_clean$Year))
message("  Total deaths:  ", scales::comma(sum(deaths_clean$Deaths)))
message("  ICD-10 codes:  ", n_distinct(deaths_clean$icd10))
message("  IMD deciles:   ", paste(sort(unique(deaths_clean$IMD_decile[!is.na(deaths_clean$IMD_decile)])), collapse = ", "))
n_na <- sum(is.na(deaths_clean$IMD_decile))
message("  NA IMD decile: ", n_na, " (", round(100 * n_na / nrow(deaths_clean), 1), "%)")

# --- Save ---------------------------------------------------------------------
saveRDS(deaths_clean, here("data", "deaths_clean.rds"))
message("\nSaved: data/deaths_clean.rds")
