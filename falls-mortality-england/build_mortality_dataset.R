# =============================================================================
# build_mortality_dataset.R
#
# Master script: ONS mortality + IMD infrastructure dataset
# England, 2002-2022
#
# Methodology follows Oldham et al. (2025) Lancet Public Health exactly:
#   Deaths:      ONS ad-hoc 1355 - deaths by sex, single year of age,
#                ICD-10 UCOD, IMD decile, England, 2001-2022
#                (2001 dropped in 06_combine.R; analysis period 2002-2022)
#   Population:  LSOA-level ONS mid-year population estimates, mapped to
#                nearest IMD version for each year and aggregated to quintile
#   Rates:       Direct age-standardisation using 2013 European Standard
#                Population via weighted mean of age-specific rates
#
# Output schema (data/mortality_imd_england_2002_2022.rds):
#   year      <int>  2002-2022
#   sex       <chr>  "Male" | "Female" | "Total"
#   imd_q     <chr>  "Q1 (least deprived)" ... "Q5 (most deprived)" | "Overall"
#   age5      <chr>  5-year ESP band: "Under 1", "1-4", ..., "90+"
#   icd10     <chr>  4-character ICD-10 underlying cause
#   deaths    <int>  registered deaths
#   pop       <dbl>  mid-year population
#
# Reference:
#   Oldham M et al. Trends in alcohol-specific deaths in England, 2001-22.
#   Lancet Public Health 2025. doi:10.1016/S2468-2667(25)00047-7
#   Code: github.com/VictimOfMaths/Publications/AlcoholSpecificDeathsEWPaper
#
# Usage:
#   source("build_mortality_dataset.R")
# =============================================================================

rm(list = ls())

required_packages <- c("tidyverse", "curl", "readxl", "here", "fs",
                        "gtools", "data.table",
                        "PHEindicatormethods")

to_install <- required_packages[!vapply(required_packages,
                                         requireNamespace, logical(1),
                                         quietly = TRUE)]
if (length(to_install) > 0) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Create directories
walk(c("data-raw/deaths", "data-raw/population", "data-raw/imd",
       "data-raw/lookups", "data"), dir_create, recurse = TRUE)

# Run pipeline
source(here("R", "01_download_deaths.R"))
source(here("R", "02_process_deaths.R"))
source(here("R", "03_download_population.R"))
source(here("R", "04_download_imd.R"))
source(here("R", "05_build_population.R"))
source(here("R", "06_combine.R"))

message("\n✓ Pipeline complete.")
message("Output: data/mortality_imd_england_2002_2022.rds")
