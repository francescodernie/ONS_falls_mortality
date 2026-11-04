# =============================================================================
# R/05_build_population.R
#
# Reads LSOA-level population files, joins to year-appropriate IMD versions
# and LSOA boundary lookups, then aggregates to IMD quintile level.
#
# Directly implements Oldham et al. (2025) AnalysisCode.R lines 701-902.
#
# Year-to-IMD mapping:
#   2018-2022 -> IMD 2019  (LSOA 2011 codes; 2021-22 need 2021->2011 lookup)
#   2013-2017 -> IMD 2015  (LSOA 2011 codes)
#   2009-2012 -> IMD 2010  (LSOA 2001 codes -> need 2001->2011 lookup)
#   2006-2008 -> IMD 2007  (LSOA 2001 codes -> need 2001->2011 lookup)
#   2001-2005 -> IMD 2004  (LSOA 2001 codes -> need 2001->2011 lookup)
#
# Quintile direction: Q5 = most deprived, Q1 = least deprived
# (Oldham lines 891-896: deciles 1-2 -> Q5 most deprived)
# Note: raw IMD decile 1 = most deprived in all IMD files.
# =============================================================================

message("\n--- 05_build_population.R ---")

pop_dir <- here("data-raw", "population")

# Load IMD and lookups
IMD19    <- readRDS(here("data", "IMD2019.rds"))
IMD15    <- readRDS(here("data", "IMD2015.rds"))
IMD10    <- readRDS(here("data", "IMD2010.rds"))
IMD07    <- readRDS(here("data", "IMD2007.rds"))
IMD04    <- readRDS(here("data", "IMD2004.rds"))
LSOA1121 <- readRDS(here("data", "LSOA1121_lookup.rds"))
LSOA0111 <- readRDS(here("data", "LSOA0111_lookup.rds"))

# Helper: standard age5 bands from numeric age
make_age5 <- function(age) {
  case_when(
    age < 1   ~ "Under 1", age < 5   ~ "1-4",   age < 10  ~ "5-9",
    age < 15  ~ "10-14",  age < 20  ~ "15-19",  age < 25  ~ "20-24",
    age < 30  ~ "25-29",  age < 35  ~ "30-34",  age < 40  ~ "35-39",
    age < 45  ~ "40-44",  age < 50  ~ "45-49",  age < 55  ~ "50-54",
    age < 60  ~ "55-59",  age < 65  ~ "60-64",  age < 70  ~ "65-69",
    age < 75  ~ "70-74",  age < 80  ~ "75-79",  age < 85  ~ "80-84",
    age < 90  ~ "85-89",  age >= 90 ~ "90+"
  )
}

# =============================================================================
# READ POPULATION FILES
# Directly from Oldham AnalysisCode.R
# =============================================================================

message("Reading 2021-2022 populations...")
# Oldham lines 113-133: LSOA 2021 boundaries
Pop2022 <- read_excel(here(pop_dir, "pop_2021_2022.xlsx"),
                      sheet = "Mid-2022 LSOA 2021", range = "C4:GE35676") %>%
  gather(Group, Pop, c(4:185)) %>%
  mutate(Sex  = if_else(substr(Group, 1, 1) == "F", "Female", "Male"),
         Age  = as.numeric(substr(Group, 2, 3)),
         Age5 = make_age5(Age)) %>%
  group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, Sex, Age5) %>%
  summarise(Pop = sum(Pop), .groups = "drop")

Pop2021 <- read_excel(here(pop_dir, "pop_2021_2022.xlsx"),
                      sheet = "Mid-2021 LSOA 2021", range = "C4:GE35676") %>%
  gather(Group, Pop, c(4:185)) %>%
  mutate(Sex  = if_else(substr(Group, 1, 1) == "F", "Female", "Male"),
         Age  = as.numeric(substr(Group, 2, 3)),
         Age5 = make_age5(Age)) %>%
  group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, Sex, Age5) %>%
  summarise(Pop = sum(Pop), .groups = "drop")

message("Reading 2020 population...")
# Oldham lines 140-153: LSOA 2011 boundaries, xlsx (not zip)
Pop2020 <- bind_rows(
  read_excel(here(pop_dir, "pop_2020.xlsx"),
             sheet = "Mid-2020 Males", range = "A5:CT34758") %>%
    gather(Age, Pop, c(8:98)) %>% mutate(Sex = "Male"),
  read_excel(here(pop_dir, "pop_2020.xlsx"),
             sheet = "Mid-2020 Females", range = "A5:CT34758") %>%
    gather(Age, Pop, c(8:98)) %>% mutate(Sex = "Female")
) %>%
  mutate(
    Age = suppressWarnings(as.numeric(gsub("\\+", "", Age))),
    Pop = suppressWarnings(as.numeric(Pop)),
    Age5 = make_age5(Age)
  ) %>%
  filter(!is.na(Age), !is.na(Pop), !is.na(Age5)) %>%
  group_by(`LSOA Code`, `LSOA Name`, Sex, Age5) %>%
  summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop")

# Helper to read standard annual zip format (2013-2019)
read_annual_zip <- function(year, zip_subdir, male_sheet, female_sheet,
                             range_str, lsoa_col, name_col) {
  message("Reading ", year, " population...")
  # Case-insensitive search: catches .xls, .XLS, .xlsx, .XLSX
  all_files <- dir_ls(here(pop_dir, zip_subdir), recurse = TRUE)
  xl_files  <- all_files[grepl("\\.(xls|xlsx)$", all_files, ignore.case = TRUE)]
  if (length(xl_files) == 0) {
    stop("No Excel file found in ", here(pop_dir, zip_subdir),
         "\nFiles present: ", paste(all_files, collapse = ", "))
  }
  xl_file <- xl_files[1]
  message("  Using: ", path_file(xl_file))
  bind_rows(
    read_excel(xl_file, sheet = male_sheet,   range = range_str) %>%
      gather(Age, Pop, c(5:95)) %>% mutate(Sex = "Male"),
    read_excel(xl_file, sheet = female_sheet, range = range_str) %>%
      gather(Age, Pop, c(5:95)) %>% mutate(Sex = "Female")
  ) %>%
    mutate(
      Age = suppressWarnings(as.numeric(gsub("\\+", "", Age))),
      Pop = suppressWarnings(as.numeric(Pop)),
      Age5 = make_age5(Age)
    ) %>%
    filter(!is.na(Age), !is.na(Pop), !is.na(Age5)) %>%
    group_by(.data[[lsoa_col]], .data[[name_col]], Sex, Age5) %>%
    summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop")
}

# 2019 read separately: this file has two extra LA boundary columns
# (cols 5-6: "LA Code 2020 boundaries", "LA name 2020 boundaries")
# not present in other years, shifting age columns from 5:95 to 8:98.
# Range extended to CV to capture the 90+ column.
message("Reading 2019 population...")
all_files_2019 <- dir_ls(here(pop_dir, "pop_2019"), recurse = TRUE)
xl_2019 <- all_files_2019[grepl("\\.(xls|xlsx)$", all_files_2019,
                                  ignore.case = TRUE)][1]
message("  Using: ", path_file(xl_2019))

Pop2019 <- bind_rows(
  read_excel(xl_2019, sheet = "Mid-2019 Males",
             range = "A5:CV34758") %>%
    gather(Age, Pop, c(8:98)) %>% mutate(Sex = "Male"),
  read_excel(xl_2019, sheet = "Mid-2019 Females",
             range = "A5:CV34758") %>%
    gather(Age, Pop, c(8:98)) %>% mutate(Sex = "Female")
) %>%
  mutate(
    Age  = suppressWarnings(as.numeric(gsub("\\+", "", Age))),
    Pop  = suppressWarnings(as.numeric(Pop)),
    Age5 = make_age5(Age)
  ) %>%
  filter(!is.na(Age), !is.na(Pop), !is.na(Age5)) %>%
  group_by(`LSOA Code`, `LSOA Name`, Sex, Age5) %>%
  summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop")

Pop2018 <- read_annual_zip(2018, "pop_2018",
  "Mid-2018 Males", "Mid-2018 Females", "A5:CQ35097",
  "Area Codes", "LSOA")

Pop2017 <- read_annual_zip(2017, "pop_2017",
  "Mid-2017 Males", "Mid-2017 Females", "A5:CQ35106",
  "Area Codes", "...3")

Pop2016 <- read_annual_zip(2016, "pop_2016",
  "Mid-2016 Males", "Mid-2016 Females", "A5:CQ35106",
  "Area Codes", "...3")

Pop2015 <- read_annual_zip(2015, "pop_2015",
  "Mid-2015 Males", "Mid-2015 Females", "A5:CQ35106",
  "Area Codes", "...3")

Pop2014 <- read_annual_zip(2014, "pop_2014",
  "Mid-2014 Males", "Mid-2014 Females", "A5:CQ35106",
  "Area Codes", "...3")

Pop2013 <- read_annual_zip(2013, "pop_2013",
  "Mid-2013 Males", "Mid-2013 Females", "A5:CQ35106",
  "Area Codes", "...3")

Pop2012 <- read_annual_zip(2012, "pop_2012",
  "Mid-2012 Males", "Mid-2012 Females", "A5:CQ35106",
  "Area Codes", "...3")

# 2002-2011: read from the combined male/female zips (Oldham lines 355-473)
message("Reading 2002-2011 populations (combined files)...")
males_dir   <- here(pop_dir, "pop_2002_2011_males")
females_dir <- here(pop_dir, "pop_2002_2011_females")

read_old_sheet <- function(file, sheet, range, sex_label) {
  read_excel(file, sheet = sheet, range = range) %>%
    gather(Age, Pop, c(5:95)) %>%
    mutate(Sex = sex_label)
}

clean_old_pop <- function(df) {
  df %>%
    mutate(Age = gsub("m|f|plus", "", Age),
           Age = suppressWarnings(as.numeric(Age)),
           Pop = suppressWarnings(as.numeric(Pop)),
           Age5 = make_age5(Age)) %>%
    filter(!is.na(Age), !is.na(Pop), !is.na(Age5)) %>%
    group_by(LSOA11CD, Sex, Age5) %>%
    summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop")
}

# Files within the 2002-2011 combined zips
male_file_b <- dir_ls(males_dir, glob = "*2007*to*2010*.xls")[1]
male_file_a <- dir_ls(males_dir, glob = "*2002*to*2006*.xls")[1]
fem_file_b  <- dir_ls(females_dir, glob = "*2007*to*2010*.xls")[1]
fem_file_a  <- dir_ls(females_dir, glob = "*2002*to*2006*.xls")[1]

years_b <- c("2011", "2010", "2009", "2008", "2007")
years_a <- c("2006", "2005", "2004", "2003", "2002")

for (yr in years_b) {
  df <- bind_rows(
    read_old_sheet(male_file_b, paste0("Mid-", yr), "A1:CQ34754", "Male"),
    read_old_sheet(fem_file_b,  paste0("Mid-", yr), "A1:CQ34754", "Female")
  ) %>% clean_old_pop()
  assign(paste0("Pop", yr), df)
}

for (yr in years_a) {
  df <- bind_rows(
    read_old_sheet(male_file_a, paste0("Mid-", yr), "A1:CQ34754", "Male"),
    read_old_sheet(fem_file_a,  paste0("Mid-", yr), "A1:CQ34754", "Female")
  ) %>% clean_old_pop()
  assign(paste0("Pop", yr), df)
}

# 2001 excluded: quinary population file produces unreliable denominators
# (LSOA join failures result in near-zero population totals).
# Series therefore runs 2002-2022. Can be revisited if needed.

# =============================================================================
# JOIN TO IMD AND AGGREGATE TO QUINTILE
# Follows Oldham lines 702-902 exactly
# =============================================================================

message("Joining populations to IMD versions...")

# Decile column names differ by IMD version
imd19_decile_col <- "Index of Multiple Deprivation (IMD) Decile"
imd15_decile_col <- "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)"
imd10_decile_col <- "Index of Multiple Deprivation (IMD) Decile"

summarise_pop <- function(df) {
  df %>% group_by(Age5, Sex, `Index of Multiple Deprivation (IMD) Decile`) %>%
    summarise(Pop = sum(Pop), .groups = "drop")
}

rename_decile <- function(df) {
  df %>% set_names(c("Age5", "Sex", "Index of Multiple Deprivation (IMD) Decile", "Pop"))
}

# 2022 and IMD 2019 (Oldham lines 702-707)
Pop22 <- Pop2022 %>%
  merge(LSOA1121, by.x = "LSOA 2021 Code", by.y = "LSOA21CD") %>%
  merge(IMD19,    by.x = "LSOA11CD", by.y = "LSOA code (2011)") %>%
  summarise_pop()

# 2021 and IMD 2019
Pop21 <- Pop2021 %>%
  merge(LSOA1121, by.x = "LSOA 2021 Code", by.y = "LSOA21CD") %>%
  merge(IMD19,    by.x = "LSOA11CD", by.y = "LSOA code (2011)") %>%
  summarise_pop()

# 2020 and IMD 2019
Pop20 <- Pop2020 %>%
  merge(IMD19, by.x = "LSOA Code", by.y = "LSOA code (2011)") %>%
  summarise_pop()

# 2019 and IMD 2019
Pop19 <- Pop2019 %>%
  merge(IMD19, by.x = "LSOA Code", by.y = "LSOA code (2011)") %>%
  summarise_pop()

# 2018 and IMD 2019
Pop18 <- Pop2018 %>%
  merge(IMD19, by.x = "Area Codes", by.y = "LSOA code (2011)") %>%
  summarise_pop()

# 2017 and IMD 2015
Pop17 <- Pop2017 %>%
  merge(IMD15, by.x = "Area Codes", by.y = "LSOA code (2011)") %>%
  group_by(Age5, Sex, .data[[imd15_decile_col]]) %>%
  summarise(Pop = sum(Pop), .groups = "drop") %>%
  rename_decile()

# 2016 and IMD 2015
Pop16 <- Pop2016 %>%
  merge(IMD15, by.x = "Area Codes", by.y = "LSOA code (2011)") %>%
  group_by(Age5, Sex, .data[[imd15_decile_col]]) %>%
  summarise(Pop = sum(Pop), .groups = "drop") %>%
  rename_decile()

# 2015 and IMD 2015
Pop15 <- Pop2015 %>%
  merge(IMD15, by.x = "Area Codes", by.y = "LSOA code (2011)") %>%
  group_by(Age5, Sex, .data[[imd15_decile_col]]) %>%
  summarise(Pop = sum(Pop), .groups = "drop") %>%
  rename_decile()

# 2014 and IMD 2015
Pop14 <- Pop2014 %>%
  merge(IMD15, by.x = "Area Codes", by.y = "LSOA code (2011)") %>%
  group_by(Age5, Sex, .data[[imd15_decile_col]]) %>%
  summarise(Pop = sum(Pop), .groups = "drop") %>%
  rename_decile()

# 2013 and IMD 2015
Pop13 <- Pop2013 %>%
  merge(IMD15, by.x = "Area Codes", by.y = "LSOA code (2011)") %>%
  group_by(Age5, Sex, .data[[imd15_decile_col]]) %>%
  summarise(Pop = sum(Pop), .groups = "drop") %>%
  rename_decile()

# 2012 and IMD 2010 (needs LSOA01->LSOA11 lookup first)
Pop12 <- Pop2012 %>%
  merge(LSOA0111, by.x = "Area Codes", by.y = "LSOA11CD") %>%
  merge(IMD10, by.x = "LSOA01CD", by.y = "LSOA CODE") %>%
  summarise_pop() %>% rename_decile()

# 2011 and IMD 2010
Pop11 <- Pop2011 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD10, by.x = "LSOA01CD", by.y = "LSOA CODE") %>%
  summarise_pop() %>% rename_decile()

# 2010 and IMD 2010
Pop10 <- Pop2010 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD10, by.x = "LSOA01CD", by.y = "LSOA CODE") %>%
  summarise_pop() %>% rename_decile()

# 2009 and IMD 2010
Pop09 <- Pop2009 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD10, by.x = "LSOA01CD", by.y = "LSOA CODE") %>%
  summarise_pop() %>% rename_decile()

# 2008 and IMD 2007
Pop08 <- Pop2008 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD07, by.x = "LSOA01CD", by.y = "LSOA") %>%
  summarise_pop() %>% rename_decile()

# 2007 and IMD 2007
Pop07 <- Pop2007 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD07, by.x = "LSOA01CD", by.y = "LSOA") %>%
  summarise_pop() %>% rename_decile()

# 2006 and IMD 2007
Pop06 <- Pop2006 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD07, by.x = "LSOA01CD", by.y = "LSOA") %>%
  summarise_pop() %>% rename_decile()

# 2005 and IMD 2004
Pop05 <- Pop2005 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD04, by.x = "LSOA01CD", by.y = "SOA") %>%
  summarise_pop() %>% rename_decile()

# 2004 and IMD 2004
Pop04 <- Pop2004 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD04, by.x = "LSOA01CD", by.y = "SOA") %>%
  summarise_pop() %>% rename_decile()

# 2003 and IMD 2004
Pop03 <- Pop2003 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD04, by.x = "LSOA01CD", by.y = "SOA") %>%
  summarise_pop() %>% rename_decile()

# 2002 and IMD 2004
Pop02 <- Pop2002 %>%
  merge(LSOA0111, by = "LSOA11CD") %>%
  merge(IMD04, by.x = "LSOA01CD", by.y = "SOA") %>%
  summarise_pop() %>% rename_decile()

# 2001 excluded - see note above

# =============================================================================
# COMBINE AND COLLAPSE TO QUINTILES
# Follows Oldham lines 866-902 exactly
# =============================================================================

message("Combining population years and collapsing to quintiles...")

PopFinal <- bind_rows(
  Pop22 %>% mutate(Year = 2022L),
  Pop21 %>% mutate(Year = 2021L),
  Pop20 %>% mutate(Year = 2020L),
  Pop19 %>% mutate(Year = 2019L),
  Pop18 %>% mutate(Year = 2018L),
  Pop17 %>% mutate(Year = 2017L),
  Pop16 %>% mutate(Year = 2016L),
  Pop15 %>% mutate(Year = 2015L),
  Pop14 %>% mutate(Year = 2014L),
  Pop13 %>% mutate(Year = 2013L),
  Pop12 %>% mutate(Year = 2012L),
  Pop11 %>% mutate(Year = 2011L),
  Pop10 %>% mutate(Year = 2010L),
  Pop09 %>% mutate(Year = 2009L),
  Pop08 %>% mutate(Year = 2008L),
  Pop07 %>% mutate(Year = 2007L),
  Pop06 %>% mutate(Year = 2006L),
  Pop05 %>% mutate(Year = 2005L),
  Pop04 %>% mutate(Year = 2004L),
  Pop03 %>% mutate(Year = 2003L),
  Pop02 %>% mutate(Year = 2002L)
  # 2001 excluded: unreliable quinary denominator file
) %>%
  set_names("Age5", "Sex", "IMD_decile", "Pop", "Year") %>%
  # Collapse deciles to quintiles, Q5 = most deprived (Oldham lines 891-896)
  # Raw decile 1 = most deprived -> maps to Q5
  mutate(IMD_q = case_when(
    IMD_decile %in% c(1L, 2L)  ~ "Q5 (most deprived)",
    IMD_decile %in% c(3L, 4L)  ~ "Q4",
    IMD_decile %in% c(5L, 6L)  ~ "Q3",
    IMD_decile %in% c(7L, 8L)  ~ "Q2",
    IMD_decile %in% c(9L, 10L) ~ "Q1 (least deprived)"
  )) %>%
  group_by(Age5, Sex, Year, IMD_q) %>%
  summarise(Pop = sum(Pop), .groups = "drop") %>%
  mutate(Age5 = factor(Age5, levels = c(
    "Under 1", "1-4", "5-9", "10-14", "15-19", "20-24",
    "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
    "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
    "85-89", "90+"
  )))

message("\nPopulation summary:")
message("  Rows:       ", nrow(PopFinal))
message("  Years:      ", paste(sort(unique(PopFinal$Year)), collapse = ", "))
message("  Quintiles:  ", paste(sort(unique(PopFinal$IMD_q)), collapse = ", "))

saveRDS(PopFinal, here("data", "population_clean.rds"))
message("Saved: data/population_clean.rds")
