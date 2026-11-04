# =============================================================================
# R/04_download_imd.R
#
# Downloads four IMD versions (2004, 2007, 2010, 2015, 2019) and two LSOA
# boundary lookup tables (2001->2011, 2011->2021).
#
# Uses exact URLs from Oldham et al. (2025) AnalysisCode.R lines 600-699.
#
# IMD year-to-population mapping (Oldham et al. protocol):
#   2001-2005 -> IMD 2004
#   2006-2008 -> IMD 2007
#   2009-2012 -> IMD 2010
#   2013-2017 -> IMD 2015
#   2018-2022 -> IMD 2019
# =============================================================================

message("\n--- 04_download_imd.R ---")

imd_dir     <- here("data-raw", "imd")
lookup_dir  <- here("data-raw", "lookups")

maybe_download <- function(url, destfile, label) {
  if (!file_exists(destfile)) {
    message("  Downloading: ", label)
    tryCatch(
      curl_download(url = url, destfile = destfile, quiet = FALSE, mode = "wb"),
      error = function(e) warning("Failed: ", label, " - ", e$message)
    )
  } else {
    message("  Already present: ", label)
  }
}

# =============================================================================
# IMD 2019 (Oldham line 603)
# =============================================================================
maybe_download(
  url      = "https://assets.publishing.service.gov.uk/media/5d8b3abded915d0373d3540f/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx",
  destfile = here(imd_dir, "IMD2019.xlsx"),
  label    = "IMD 2019"
)

# =============================================================================
# IMD 2015 (Oldham line 610)
# =============================================================================
maybe_download(
  url      = "https://assets.publishing.service.gov.uk/media/5a805f96ed915d74e33fa0df/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx",
  destfile = here(imd_dir, "IMD2015.xlsx"),
  label    = "IMD 2015"
)

# =============================================================================
# IMD 2010 (Oldham line 617)
# =============================================================================
maybe_download(
  url      = "https://assets.publishing.service.gov.uk/media/5a79b8c6ed915d042206a8e2/1871524.xls",
  destfile = here(imd_dir, "IMD2010.xls"),
  label    = "IMD 2010"
)

# =============================================================================
# IMD 2007 - National Archives URLs are unreliable for automated download.
# Download manually and place in data-raw/imd/IMD2007/
#
# URL: https://webarchive.nationalarchives.gov.uk/ukgwa/20100411141238/
#      http://www.communities.gov.uk/documents/communities/zip/indices2007.zip
#
# Unzip into data-raw/imd/IMD2007/ and confirm with:
#   fs::dir_ls("data-raw/imd/IMD2007", recurse = TRUE)
# =============================================================================
dir_create(here(imd_dir, "IMD2007"))
imd07_files <- dir_ls(here(imd_dir, "IMD2007"), glob = "*.xls*", recurse = TRUE)
if (length(imd07_files) == 0) {
  stop(
    "IMD 2007 file not found in data-raw/imd/IMD2007/\n",
    "Please download manually from the National Archives:\n",
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20100411141238/\n",
    "http://www.communities.gov.uk/documents/communities/zip/indices2007.zip\n",
    "Unzip into: ", here(imd_dir, "IMD2007")
  )
}
message("  IMD 2007 found: ", path_file(imd07_files[1]))

# =============================================================================
# IMD 2004 - same issue; download manually.
#
# URL: https://webarchive.nationalarchives.gov.uk/ukgwa/20100407164233/
#      http://www.communities.gov.uk/documents/communities/zip/soalevelid.zip
#
# Unzip into data-raw/imd/IMD2004/
# =============================================================================
dir_create(here(imd_dir, "IMD2004"))
imd04_files <- dir_ls(here(imd_dir, "IMD2004"), glob = "*.xls*", recurse = TRUE)
if (length(imd04_files) == 0) {
  stop(
    "IMD 2004 file not found in data-raw/imd/IMD2004/\n",
    "Please download manually from the National Archives:\n",
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20100407164233/\n",
    "http://www.communities.gov.uk/documents/communities/zip/soalevelid.zip\n",
    "Unzip into: ", here(imd_dir, "IMD2004")
  )
}
message("  IMD 2004 found: ", path_file(imd04_files[1]))

# =============================================================================
# LSOA 2011 -> 2021 lookup (Oldham lines 686-691)
# =============================================================================
maybe_download(
  url      = paste0(
    "https://hub.arcgis.com/api/v3/datasets/",
    "b14d449ba10a48508bd05cd4a9775e2b_0/downloads/data",
    "?format=csv&spatialRefId=3857&where=1%3D1"
  ),
  destfile = here(lookup_dir, "LSOA2011_to_LSOA2021.csv"),
  label    = "LSOA 2011 to 2021 lookup"
)

# =============================================================================
# LSOA 2001 -> 2011 lookup (Oldham lines 694-699)
# =============================================================================
maybe_download(
  url      = paste0(
    "https://opendata.arcgis.com/api/v3/datasets/",
    "3dd1bc5dd053426aa84a068c7afbb3b2_0/downloads/data",
    "?format=csv&spatialRefId=4326&where=1%3D1"
  ),
  destfile = here(lookup_dir, "LSOA2001_to_LSOA2011.csv"),
  label    = "LSOA 2001 to 2011 lookup"
)

# =============================================================================
# Load and save IMD files as RDS for faster subsequent access
# =============================================================================
message("\nProcessing IMD files...")

# IMD 2019 (Oldham line 606)
IMD19 <- read_excel(here(imd_dir, "IMD2019.xlsx"),
                    sheet = "IMD2019", range = "A1:F32845")
saveRDS(IMD19, here("data", "IMD2019.rds"))
message("  IMD 2019: ", nrow(IMD19), " LSOAs")

# IMD 2015 (Oldham line 613)
IMD15 <- read_excel(here(imd_dir, "IMD2015.xlsx"),
                    sheet = "IMD 2015", range = "A1:F32845")
saveRDS(IMD15, here("data", "IMD2015.rds"))
message("  IMD 2015: ", nrow(IMD15), " LSOAs")

# IMD 2010 (Oldham lines 620-634)
# Decile not pre-computed in this file - derived using quantcut on rank
IMD10_raw <- read_excel(here(imd_dir, "IMD2010.xls"),
                        sheet = "IMD 2010", range = "A1:G32483")
IMD10 <- IMD10_raw %>%
  mutate(decile = quantcut(`RANK OF IMD SCORE (where 1 is most deprived)`, 10)) %>%
  mutate(`Index of Multiple Deprivation (IMD) Decile` = case_when(
    decile == levels(.$decile)[1]  ~ 1L,
    decile == levels(.$decile)[2]  ~ 2L,
    decile == levels(.$decile)[3]  ~ 3L,
    decile == levels(.$decile)[4]  ~ 4L,
    decile == levels(.$decile)[5]  ~ 5L,
    decile == levels(.$decile)[6]  ~ 6L,
    decile == levels(.$decile)[7]  ~ 7L,
    decile == levels(.$decile)[8]  ~ 8L,
    decile == levels(.$decile)[9]  ~ 9L,
    decile == levels(.$decile)[10] ~ 10L
  ))
saveRDS(IMD10, here("data", "IMD2010.rds"))
message("  IMD 2010: ", nrow(IMD10), " LSOAs")

# IMD 2007 (Oldham lines 643-658) - uses IMD10 decile levels for consistency
# Detect filename dynamically as the zip contents may vary
imd07_dir <- here("data-raw", "imd", "IMD2007")
imd07_file <- dir_ls(imd07_dir, glob = "*.xls*", recurse = TRUE)
if (length(imd07_file) == 0) stop("No xls file found in ", imd07_dir)
message("  IMD 2007 file: ", path_file(imd07_file[1]))

IMD07_raw <- read_excel(
  imd07_file[1],
  sheet = "IMD 2007", range = "A1:G32483"
)
IMD07 <- IMD07_raw %>%
  mutate(decile = quantcut(`RANK OF IMD (where 1 is most deprived)`, 10)) %>%
  mutate(`Index of Multiple Deprivation (IMD) Decile` = case_when(
    decile == levels(.$decile)[1]  ~ 1L,
    decile == levels(.$decile)[2]  ~ 2L,
    decile == levels(.$decile)[3]  ~ 3L,
    decile == levels(.$decile)[4]  ~ 4L,
    decile == levels(.$decile)[5]  ~ 5L,
    decile == levels(.$decile)[6]  ~ 6L,
    decile == levels(.$decile)[7]  ~ 7L,
    decile == levels(.$decile)[8]  ~ 8L,
    decile == levels(.$decile)[9]  ~ 9L,
    decile == levels(.$decile)[10] ~ 10L
  ))
saveRDS(IMD07, here("data", "IMD2007.rds"))
message("  IMD 2007: ", nrow(IMD07), " LSOAs")

# IMD 2004 (Oldham lines 667-682)
# Detect filename dynamically
imd04_dir <- here("data-raw", "imd", "IMD2004")
imd04_file <- dir_ls(imd04_dir, glob = "*.xls*", recurse = TRUE)
if (length(imd04_file) == 0) stop("No xls file found in ", imd04_dir)
message("  IMD 2004 file: ", path_file(imd04_file[1]))

IMD04_raw <- read_excel(
  imd04_file[1],
  sheet = "IMD 2004", range = "A1:G32483"
)
IMD04 <- IMD04_raw %>%
  mutate(decile = quantcut(`RANK OF IMD (where 1 is most deprived)`, 10)) %>%
  mutate(`Index of Multiple Deprivation (IMD) Decile` = case_when(
    decile == levels(.$decile)[1]  ~ 1L,
    decile == levels(.$decile)[2]  ~ 2L,
    decile == levels(.$decile)[3]  ~ 3L,
    decile == levels(.$decile)[4]  ~ 4L,
    decile == levels(.$decile)[5]  ~ 5L,
    decile == levels(.$decile)[6]  ~ 6L,
    decile == levels(.$decile)[7]  ~ 7L,
    decile == levels(.$decile)[8]  ~ 8L,
    decile == levels(.$decile)[9]  ~ 9L,
    decile == levels(.$decile)[10] ~ 10L
  ))
saveRDS(IMD04, here("data", "IMD2004.rds"))
message("  IMD 2004: ", nrow(IMD04), " LSOAs")

# LSOA lookups (Oldham lines 690-699)
LSOA1121 <- read.csv(here(lookup_dir, "LSOA2011_to_LSOA2021.csv")) %>%
  select(LSOA11CD, LSOA21CD)
saveRDS(LSOA1121, here("data", "LSOA1121_lookup.rds"))
message("  LSOA 2011->2021 lookup: ", nrow(LSOA1121), " rows")

LSOA0111 <- read.csv(here(lookup_dir, "LSOA2001_to_LSOA2011.csv")) %>%
  select(LSOA01CD, LSOA11CD)
saveRDS(LSOA0111, here("data", "LSOA0111_lookup.rds"))
message("  LSOA 2001->2011 lookup: ", nrow(LSOA0111), " rows")

message("\nIMD and lookup files ready.")
