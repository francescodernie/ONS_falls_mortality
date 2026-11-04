# =============================================================================
# R/03_download_population.R
#
# Downloads all LSOA-level mid-year population estimate files from ONS.
# Uses the exact URLs from Oldham et al. (2025) AnalysisCode.R lines 109-598.
#
# Files are large and numerous. Each is cached to data-raw/population/ so
# re-runs skip already-downloaded files.
#
# Three distinct ONS file series are used:
#   2021-2022: LSOA 2021 boundaries, single xlsx with both years
#   2013-2020: LSOA 2011 boundaries, one zip per year
#   2002-2012: LSOA 2011 boundaries, two combined zips (males / females)
#   2001:      LSOA 2011 boundaries, separate quinary age band file
# =============================================================================

message("\n--- 03_download_population.R ---")

pop_dir <- here("data-raw", "population")

# Helper: download only if not already present
maybe_download <- function(url, destfile, label) {
  if (!file_exists(destfile)) {
    message("  Downloading: ", label)
    tryCatch(
      curl_download(url = url, destfile = destfile, quiet = FALSE, mode = "wb"),
      error = function(e) warning("Failed to download ", label, ": ", e$message)
    )
  } else {
    message("  Already present: ", label)
  }
}

# =============================================================================
# 2021-2022: single file, LSOA 2021 boundaries
# Oldham lines 110-111
# =============================================================================
maybe_download(
  url = paste0(
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
    "populationandmigration/populationestimates/datasets/",
    "lowersuperoutputareamidyearpopulationestimates/",
    "mid2021andmid2022/sapelsoasyoatablefinal.xlsx"
  ),
  destfile = here(pop_dir, "pop_2021_2022.xlsx"),
  label = "2021-2022 LSOA populations"
)

# =============================================================================
# 2013-2020: individual year zips, LSOA 2011 boundaries
# Oldham lines 137-201 (2020, 2019, 2018) and 230-321 (2013-2017)
# =============================================================================
annual_files <- list(
  "2020" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
    ),
    dest = here(pop_dir, "pop_2020.xlsx"),
    zip  = FALSE
  ),
  "2019" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip"
    ),
    dest = here(pop_dir, "pop_2019.zip"),
    zip  = TRUE
  ),
  "2018" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2018.zip"),
    zip  = TRUE
  ),
  "2017" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2017/sape20dt1mid2017lsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2017.zip"),
    zip  = TRUE
  ),
  "2016" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2016/sape20dt1mid2016lsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2016.zip"),
    zip  = TRUE
  ),
  "2015" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2015/sape20dt1mid2015lsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2015.zip"),
    zip  = TRUE
  ),
  "2014" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2014/sape20dt1mid2014lsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2014.zip"),
    zip  = TRUE
  ),
  "2013" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2013/sape20dt1mid2013lsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2013.zip"),
    zip  = TRUE
  ),
  "2012" = list(
    url = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "populationandmigration/populationestimates/datasets/",
      "lowersuperoutputareamidyearpopulationestimates/",
      "mid2012/sape20dt1mid2012lsoasyoaestimatesformatted.zip"
    ),
    dest = here(pop_dir, "pop_2012.zip"),
    zip  = TRUE
  )
)

for (yr in names(annual_files)) {
  f <- annual_files[[yr]]
  maybe_download(f$url, f$dest, paste(yr, "LSOA populations"))
  if (f$zip && file_exists(f$dest)) {
    out_dir <- here(pop_dir, paste0("pop_", yr))
    if (!dir_exists(out_dir)) {
      dir_create(out_dir)
      unzip(f$dest, exdir = out_dir)
    }
  }
}

# =============================================================================
# 2002-2011: two combined zips (males 2002-2011, females 2002-2011)
# Oldham lines 349-460
# =============================================================================
maybe_download(
  url = paste0(
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
    "populationandmigration/populationestimates/datasets/",
    "lowersuperoutputareamidyearpopulationestimates/",
    "mid2002tomid2011males/rftlsoaunformattedtablemales.zip"
  ),
  destfile = here(pop_dir, "pop_2002_2011_males.zip"),
  label    = "2002-2011 LSOA populations (males)"
)
if (file_exists(here(pop_dir, "pop_2002_2011_males.zip")) &&
    !dir_exists(here(pop_dir, "pop_2002_2011_males"))) {
  dir_create(here(pop_dir, "pop_2002_2011_males"))
  unzip(here(pop_dir, "pop_2002_2011_males.zip"),
        exdir = here(pop_dir, "pop_2002_2011_males"))
}

maybe_download(
  url = paste0(
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
    "populationandmigration/populationestimates/datasets/",
    "lowersuperoutputareamidyearpopulationestimates/",
    "mid2002tomid2011females/rftlsoaunformattedtablefemales.zip"
  ),
  destfile = here(pop_dir, "pop_2002_2011_females.zip"),
  label    = "2002-2011 LSOA populations (females)"
)
if (file_exists(here(pop_dir, "pop_2002_2011_females.zip")) &&
    !dir_exists(here(pop_dir, "pop_2002_2011_females"))) {
  dir_create(here(pop_dir, "pop_2002_2011_females"))
  unzip(here(pop_dir, "pop_2002_2011_females.zip"),
        exdir = here(pop_dir, "pop_2002_2011_females"))
}

# =============================================================================
# 2001: quinary age bands, separate ONS ad-hoc file
# Oldham lines 584-597
# =============================================================================
maybe_download(
  url = paste0(
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
    "populationandmigration/populationestimates/adhocs/",
    "009729mid2001tomid2017populationestimatesforlowerlayer",
    "superoutputareaslsoainenglandandwalesbyfiveyearagebandsandsex",
    "supportinginformation/lsoa0117quinaryv1.xlsx"
  ),
  destfile = here(pop_dir, "pop_2001_quinary.xlsx"),
  label    = "2001 LSOA populations (quinary)"
)

message("\nAll population files downloaded.")
