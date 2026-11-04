# =============================================================================
# R/01_download_deaths.R
#
# Downloads ONS ad-hoc 1355: deaths by sex, single year of age, ICD-10 UCOD
# and IMD decile, England, 2001-2022.
#
# Source: Oldham et al. (2025) - same URL used in their AnalysisCode.R
# =============================================================================

message("\n--- 01_download_deaths.R ---")

deaths_zip  <- here("data-raw", "deaths", "deathsbyimd20012022final.zip")
deaths_dir  <- here("data-raw", "deaths")

if (!file_exists(deaths_zip)) {
  message("Downloading deaths file (~48 MB)...")
  url <- paste0(
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
    "birthsdeathsandmarriages/deaths/adhocs/",
    "1355deathsbysexsingleyearofageunderlyingcauseicd10code",
    "anddeprivationdecileengland2001to2022/",
    "deathsbyimd20012022final.zip"
  )
  curl_download(url = url, destfile = deaths_zip, quiet = FALSE, mode = "wb")
  unzip(zipfile = deaths_zip, exdir = deaths_dir)
  message("  Done.")
} else {
  message("Deaths zip already present, skipping download.")
}

# Confirm the Excel file is present
deaths_xlsx <- file.path(deaths_dir, "Deaths by IMD 2001-2022 FINAL.xlsx")
if (!file_exists(deaths_xlsx)) {
  message("Re-unzipping...")
  unzip(zipfile = deaths_zip, exdir = deaths_dir)
}

message("Deaths file ready: ", path_file(deaths_xlsx))
