# =============================================================================
# sanity_check_v2.R
#
# Corrected validation against Oldham et al. (2025).
# Works from cell-level mortality data and re-standardises properly,
# exactly replicating Oldham AnalysisCode.R lines 924-1002.
# =============================================================================

library(tidyverse)
library(here)

mortality <- readRDS(here("data", "mortality_imd_england_2002_2022.rds"))

# 2013 European Standard Population
ESP <- tibble(
  Age5 = c("Under 1", "1-4", "5-9", "10-14", "15-19", "20-24",
            "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
            "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
            "85-89", "90+"),
  ESP_wt = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000,
             7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000)
)

# --- Define alcohol-specific causes (Oldham AnalysisCode.R lines 80-87) ------
is_alcohol_specific <- function(icd10) {
  substr(icd10, 1, 3) == "K70" |                          # Alcoholic liver disease
    icd10 == "F102" |                                       # Alcohol dependence
    substr(icd10, 1, 3) %in% c("F10", "X45", "X65", "Y15") | # Acute causes
    icd10 == "R780" |                                       # Acute causes
    icd10 %in% c("E244", "G312", "G621", "G721", "I426",   # Other alcohol-specific
                 "K292", "K852", "K860", "Q860")
}

# --- Step 1: Filter to alcohol-specific, aggregate deaths across causes -------
# Then compute age-specific rates, then age-standardise
# This exactly replicates Oldham lines 924-1002

alc_cell <- mortality %>%
  filter(is_alcohol_specific(icd10)) %>%
  group_by(Year, Sex, IMD_q, Age5) %>%
  summarise(
    Deaths = sum(Deaths, na.rm = TRUE),
    Pop    = first(Pop),   # Pop is same for all icd10 within a cell
    .groups = "drop"
  ) %>%
  mutate(mx = Deaths * 100000 / Pop)

# --- Step 2: Age-standardise (Oldham lines 993-1002) -------------------------
alc_asr <- alc_cell %>%
  left_join(ESP, by = "Age5") %>%
  group_by(Year, Sex, IMD_q) %>%
  summarise(
    mx     = weighted.mean(mx, ESP_wt, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Pop    = sum(Pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    LowerCI = mx - 1.96 * mx / sqrt(Deaths),
    UpperCI = mx + 1.96 * mx / sqrt(Deaths)
  )

# --- CHECK 1: Overall ASR series ---------------------------------------------
cat("\n=== CHECK 1: Overall alcohol-specific ASR (persons) ===\n")
cat("Oldham Figure 1: ~11-13 per 100,000; pandemic spike ~12-13 in 2020-21\n\n")

overall <- alc_asr %>%
  filter(Sex == "Total", IMD_q == "Overall") %>%
  arrange(Year)

print(overall %>% select(Year, mx, LowerCI, UpperCI, Deaths), n = 22)

# --- CHECK 2: Q5 vs Q1 rates and ratio ---------------------------------------
cat("\n=== CHECK 2: Q5 / Q1 ratio ===\n")
cat("Oldham Figure D: Q5 ~18-25, Q1 ~4-7; ratio ~3:1\n\n")

alc_asr %>%
  filter(Sex == "Total",
         IMD_q %in% c("Q1 (least deprived)", "Q5 (most deprived)")) %>%
  select(Year, IMD_q, mx) %>%
  pivot_wider(names_from = IMD_q, values_from = mx) %>%
  mutate(ratio = `Q5 (most deprived)` / `Q1 (least deprived)`) %>%
  arrange(Year) %>%
  print(n = 22)

# --- CHECK 3: Sex differences ------------------------------------------------
cat("\n=== CHECK 3: Male vs Female ASR ===\n")
alc_asr %>%
  filter(IMD_q == "Overall", Sex %in% c("Male", "Female")) %>%
  select(Year, Sex, mx) %>%
  pivot_wider(names_from = Sex, values_from = mx) %>%
  mutate(ratio = Male / Female) %>%
  arrange(Year) %>%
  print(n = 22)

# --- CHECK 4: Total deaths ---------------------------------------------------
cat("\n=== CHECK 4: Total alcohol-specific deaths per year ===\n")
cat("England: ~7,000-9,000/yr; ~9,600 in 2020, ~9,700 in 2021\n\n")

alc_asr %>%
  filter(Sex == "Total", IMD_q == "Overall") %>%
  select(Year, Deaths) %>%
  arrange(Year) %>%
  print(n = 22)

# --- CHECK 5: Quintile gradient 2019 ----------------------------------------
cat("\n=== CHECK 5: Quintile gradient in 2019 ===\n")
cat("Should increase monotonically Q1 -> Q5\n\n")

alc_asr %>%
  filter(Sex == "Total", Year == 2019, IMD_q != "Overall") %>%
  mutate(IMD_q = factor(IMD_q, levels = c(
    "Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"
  ))) %>%
  arrange(IMD_q) %>%
  select(IMD_q, mx, Deaths) %>%
  print()

# --- DIAGNOSTIC: Check 2001 population totals --------------------------------
cat("\n=== DIAGNOSTIC: Population totals by year ===\n")
cat("England population should be ~49-56 million throughout\n\n")

mortality %>%
  filter(Sex == "Total", IMD_q == "Overall", icd10 == first(icd10)) %>%
  group_by(Year) %>%
  summarise(TotalPop = sum(Pop, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year) %>%
  print(n = 22)

# --- DIAGNOSTIC: Check 2001 deaths total ------------------------------------
cat("\n=== DIAGNOSTIC: Total registered deaths by year ===\n")
cat("England all-cause deaths: ~450,000-530,000 per year\n\n")

mortality %>%
  filter(Sex == "Total", IMD_q == "Overall") %>%
  group_by(Year) %>%
  summarise(TotalDeaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year) %>%
  print(n = 22)

# --- Plot --------------------------------------------------------------------
p <- alc_asr %>%
  filter(Sex == "Total", IMD_q != "Overall") %>%
  mutate(IMD_q = factor(IMD_q, levels = c(
    "Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"
  ))) %>%
  ggplot(aes(x = Year, y = mx, colour = IMD_q, fill = IMD_q)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2001, 2022, 3)) +
  scale_y_continuous(limits = c(0, NA),
                     name = "Age-standardised deaths per 100,000") +
  scale_colour_manual(
    values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"),
    name = "IMD quintile"
  ) +
  scale_fill_manual(
    values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"),
    name = "IMD quintile"
  ) +
  labs(
    title    = "Alcohol-specific mortality by IMD quintile, England 2001-2022",
    subtitle = "Properly age-standardised from cell-level data — compare with Oldham Figure 2D",
    x        = NULL,
    caption  = "Source: ONS ad-hoc 1355 | Method: Oldham et al. 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p)
