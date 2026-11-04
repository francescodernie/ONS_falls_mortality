# =============================================================================
# Falls-related mortality in adults by age, sex, and socioeconomic deprivation:
# an analysis of national mortality data in England, 2002-2022
#
# FORMAL ANALYSIS SCRIPT
# Version 1.0 | April 2026
#
# Protocol: Study_protocol_V1.docx (OSF preregistration)
# Pipeline:  build_mortality_dataset.R (validated April 2026)
#
# STRUCTURE:
#   0. Setup and data loading
#   1. Objective 1 — Temporal trends (Analyses 1.1-1.4)
#   2. Objective 2 — Interactions (Analyses 2.1-2.3)
#   3. Objective 3 — Pandemic and joinpoint (Analyses 3.1-3.2)
#   S. Sensitivity analysis — M80-M85
#
# OUTPUTS:
#   outputs/figures/   — PNG plots (300 dpi)
#   outputs/tables/    — CSV tables for appendix
#
# REFERENCES:
#   Kim HJ et al. Stat Med. 2000;19:335-351 (joinpoint)
#   Dobson AJ et al. Stat Med. 1991;10:457-462 (DSR CIs)
#   PHE Technical Guide: Confidence Intervals. 2018
#   Oldham M et al. Lancet Public Health. 2025
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

library(tidyverse)
library(here)
library(segmented)
library(PHEindicatormethods)
library(scales)

# ── Output directories ────────────────────────────────────────────────────────
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "tables"),  recursive = TRUE, showWarnings = FALSE)

# ── Helper functions ──────────────────────────────────────────────────────────
source(here("R", "00_build_popasr.R"))   # builds PopForASR
source(here("R", "00_make_dsr.R"))       # loads make_dsr() — Dobson's method

# ── Colour palette ────────────────────────────────────────────────────────────
# IMD quintile colours: yellow (least deprived) → dark navy (most deprived)
quintile_colours <- c(
  "Q1 (least deprived)" = "#ffffb2",
  "Q2"                  = "#fecc5c",
  "Q3"                  = "#fd8d3c",
  "Q4"                  = "#f03b20",
  "Q5 (most deprived)"  = "#bd0026"
)

sex_colours <- c(
  "Male"   = "#2166ac",
  "Female" = "#d6604d"
)

# Consistent IMD quintile factor levels throughout
imd_levels <- c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")

# Age group factor levels (broad bands per protocol Section 1.3/2.1)
age_levels <- c("<65", "65-74", "75-84", "85+")

# ── Save helpers ──────────────────────────────────────────────────────────────
save_fig <- function(name, width = 10, height = 6, dpi = 300) {
  ggsave(here("outputs", "figures", paste0(name, ".png")),
         width = width, height = height, dpi = dpi, bg = "white")
  message("Figure saved: ", name, ".png")
}

save_tab <- function(data, name) {
  write_csv(data, here("outputs", "tables", paste0(name, ".csv")))
  message("Table saved: ", name, ".csv")
}

# ── Shared plot theme ─────────────────────────────────────────────────────────
theme_falls <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(size = 10, colour = "grey40"),
      plot.caption      = element_text(size = 8,  colour = "grey50"),
      legend.position   = "right",
      legend.title      = element_text(face = "bold", size = 10),
      axis.title        = element_text(size = 10),
      strip.text        = element_text(face = "bold", size = 10)
    )
}

# ── Standard plot labels ──────────────────────────────────────────────────────
caption_std <- "Source: ONS ad-hoc release 1355 | Direct standardisation, 2013 ESP | Dobson 95% CI"


# =============================================================================
# 0B. LOAD DATA AND BUILD CASE DEFINITION
# =============================================================================

message("\n--- Loading mortality data ---")
mortality <- readRDS(here("data", "mortality_imd_england_2002_2022.rds"))

# ── Primary case definition: W00-W19 (falls), adults aged ≥18 ─────────────────
fall_codes <- paste0("W", sprintf("%02d", 0:19))

# Age groups to exclude (under 18)
exclude_ages <- c("Under 1", "1-4", "5-9", "10-14", "15-19")

# Broad age band mapping (protocol Sections 1.3 and 2.1)
age_band_map <- c(
  "20-24" = "<65", "25-29" = "<65", "30-34" = "<65", "35-39" = "<65",
  "40-44" = "<65", "45-49" = "<65", "50-54" = "<65", "55-59" = "<65",
  "60-64" = "<65",
  "65-69" = "65-74", "70-74" = "65-74",
  "75-79" = "75-84", "80-84" = "75-84",
  "85-89" = "85+",   "90+"   = "85+"
)

# ── Build cell-level falls deaths dataset (adults ≥18 only) ───────────────────
falls_cell <- mortality %>%
  filter(
    substr(icd10, 1, 3) %in% fall_codes,
    !Age5 %in% exclude_ages
  ) %>%
  group_by(Year, Sex, IMD_q, Age5) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  right_join(
    PopForASR %>% filter(!Age5 %in% exclude_ages),
    by = c("Year", "Sex", "IMD_q", "Age5")
  ) %>%
  mutate(Deaths = replace_na(Deaths, 0))

message("Falls cell dataset: ", nrow(falls_cell), " rows")


# =============================================================================
# OBJECTIVE 1: TEMPORAL TRENDS IN FALLS MORTALITY
# =============================================================================

message("\n=== OBJECTIVE 1: Temporal trends ===")

# ── 1.1 Overall trend (all persons, all ages, all IMD) ────────────────────────
message("--- Analysis 1.1: Overall trend ---")

asr_overall <- falls_cell %>%
  filter(Sex == "Total", IMD_q == "Overall") %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q"))

# Figure 1.1
ggplot(asr_overall, aes(x = Year, y = mx)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              fill = "#2c7fb8", alpha = 0.2) +
  geom_line(colour = "#2c7fb8", linewidth = 1.0) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  labs(
    title    = "Figure 1.1 — Fall-related mortality, England 2002–2022",
    subtitle = "All persons, adults ≥18 years, all IMD quintiles combined",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

save_fig("fig1_1_overall_trend")
save_tab(
  asr_overall %>%
    select(Year, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`ASR per 100,000` = mx,
           `Lower 95% CI`    = LowerCI,
           `Upper 95% CI`    = UpperCI,
           `Total deaths`    = Deaths),
  "tab1_1_overall_trend"
)

# ── 1.2 Trend by sex ──────────────────────────────────────────────────────────
message("--- Analysis 1.2: Trend by sex ---")

asr_sex <- falls_cell %>%
  filter(Sex != "Total", IMD_q == "Overall") %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q"))

# Figure 1.2
ggplot(asr_sex, aes(x = Year, y = mx, colour = Sex, fill = Sex)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.0) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  scale_colour_manual(values = sex_colours, name = "Sex") +
  scale_fill_manual(values = sex_colours,   name = "Sex") +
  labs(
    title    = "Figure 1.2 — Fall-related mortality by sex, England 2002–2022",
    subtitle = "Adults ≥18 years, all IMD quintiles combined",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

save_fig("fig1_2_trend_by_sex")
save_tab(
  asr_sex %>%
    select(Year, Sex, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`ASR per 100,000` = mx,
           `Lower 95% CI`    = LowerCI,
           `Upper 95% CI`    = UpperCI,
           `Total deaths`    = Deaths),
  "tab1_2_trend_by_sex"
)

# ── 1.3 Trend by age group ────────────────────────────────────────────────────
message("--- Analysis 1.3: Trend by age group ---")

# Age-specific crude rates (not standardised — per protocol Section 3.2)
asr_age <- falls_cell %>%
  filter(Sex == "Total", IMD_q == "Overall",
         !Age5 %in% exclude_ages) %>%
  mutate(AgeBand = recode(Age5, !!!age_band_map),
         AgeBand = factor(AgeBand, levels = age_levels)) %>%
  group_by(Year, AgeBand) %>%
  summarise(
    Deaths = sum(Deaths, na.rm = TRUE),
    Pop    = sum(Pop,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mx      = Deaths / Pop * 100000,
    LowerCI = mx - 1.96 * mx / sqrt(pmax(Deaths, 1)),
    UpperCI = mx + 1.96 * mx / sqrt(pmax(Deaths, 1))
  )

age_colours <- c(
  "<65"   = "#d9f0a3",
  "65-74" = "#78c679",
  "75-84" = "#238443",
  "85+"   = "#004529"
)

# Figure 1.3
ggplot(asr_age, aes(x = Year, y = mx, colour = AgeBand, fill = AgeBand)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.0) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Crude rate per 100,000") +
  scale_colour_manual(values = age_colours, name = "Age group") +
  scale_fill_manual(values = age_colours,   name = "Age group") +
  labs(
    title    = "Figure 1.3 — Fall-related mortality by age group, England 2002–2022",
    subtitle = "All persons, adults ≥18 years, all IMD quintiles combined | Crude rates",
    x        = NULL,
    caption  = "Source: ONS ad-hoc release 1355 | Crude rates | Byar 95% CI"
  ) +
  theme_falls()

save_fig("fig1_3_trend_by_age")
save_tab(
  asr_age %>%
    select(Year, AgeBand, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`Age group`       = AgeBand,
           `Crude rate per 100,000` = mx,
           `Lower 95% CI`   = LowerCI,
           `Upper 95% CI`   = UpperCI,
           `Total deaths`   = Deaths),
  "tab1_3_trend_by_age"
)

# ── 1.4 Trend by IMD quintile ─────────────────────────────────────────────────
message("--- Analysis 1.4: Trend by IMD quintile ---")

asr_imd <- falls_cell %>%
  filter(Sex == "Total", IMD_q != "Overall") %>%
  mutate(IMD_q = factor(IMD_q, levels = imd_levels)) %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q"))

# Figure 1.4
ggplot(asr_imd, aes(x = Year, y = mx,
                     colour = IMD_q, fill = IMD_q)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.0) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  scale_colour_manual(values = quintile_colours, name = "IMD quintile") +
  scale_fill_manual(values = quintile_colours,   name = "IMD quintile") +
  labs(
    title    = "Figure 1.4 — Fall-related mortality by IMD quintile, England 2002–2022",
    subtitle = "All persons, adults ≥18 years",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

save_fig("fig1_4_trend_by_imd")
save_tab(
  asr_imd %>%
    select(Year, IMD_q, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`IMD quintile`    = IMD_q,
           `ASR per 100,000` = mx,
           `Lower 95% CI`    = LowerCI,
           `Upper 95% CI`    = UpperCI,
           `Total deaths`    = Deaths),
  "tab1_4_trend_by_imd"
)


# =============================================================================
# OBJECTIVE 2: INTERACTIONS
# =============================================================================

message("\n=== OBJECTIVE 2: Interactions ===")

# ── 2.1 Age × IMD interaction in selected years ───────────────────────────────
message("--- Analysis 2.1: Age x IMD interaction ---")

selected_years <- c(2002, 2007, 2012, 2017, 2022)

# Crude rates by age band × IMD quintile × selected year
age_imd <- falls_cell %>%
  filter(Sex == "Total",
         IMD_q != "Overall",
         !Age5 %in% exclude_ages,
         Year %in% selected_years) %>%
  mutate(
    AgeBand = recode(Age5, !!!age_band_map),
    AgeBand = factor(AgeBand, levels = age_levels),
    IMD_q   = factor(IMD_q, levels = imd_levels)
  ) %>%
  group_by(Year, AgeBand, IMD_q) %>%
  summarise(
    Deaths = sum(Deaths, na.rm = TRUE),
    Pop    = sum(Pop,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mx      = Deaths / Pop * 100000,
    LowerCI = mx - 1.96 * mx / sqrt(pmax(Deaths, 1)),
    UpperCI = mx + 1.96 * mx / sqrt(pmax(Deaths, 1))
  )

# Figure 2.1 — faceted by year, age on x-axis, IMD quintiles as lines
ggplot(age_imd, aes(x = AgeBand, y = mx,
                     colour = IMD_q, group = IMD_q)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.0) +
  facet_wrap(~Year, nrow = 1) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Crude rate per 100,000") +
  scale_colour_manual(values = quintile_colours, name = "IMD quintile") +
  labs(
    title    = "Figure 2.1 — Fall-related mortality by age group and IMD quintile, selected years",
    subtitle = "All persons, adults ≥18 years | Crude rates by broad age band",
    x        = "Age group",
    caption  = "Source: ONS ad-hoc release 1355 | Crude rates | Selected years: 2002, 2007, 2012, 2017, 2022"
  ) +
  theme_falls() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_fig("fig2_1_age_imd_interaction", width = 14, height = 6)
save_tab(
  age_imd %>%
    select(Year, AgeBand, IMD_q, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`Year`                   = Year,
           `Age group`              = AgeBand,
           `IMD quintile`           = IMD_q,
           `Crude rate per 100,000` = mx,
           `Lower 95% CI`           = LowerCI,
           `Upper 95% CI`           = UpperCI,
           `Deaths`                 = Deaths),
  "tab2_1_age_imd_interaction"
)

# ── 2.2 Sex × IMD interaction (Q1 vs Q5 by sex) ──────────────────────────────
message("--- Analysis 2.2: Sex x IMD interaction ---")

asr_sex_imd <- falls_cell %>%
  filter(Sex != "Total",
         IMD_q %in% c("Q1 (least deprived)", "Q5 (most deprived)")) %>%
  mutate(IMD_q = factor(IMD_q, levels = imd_levels)) %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q")) %>%
  mutate(
    Group = paste0(Sex, " — ", IMD_q),
    linetype = ifelse(IMD_q == "Q1 (least deprived)", "dashed", "solid")
  )

# Figure 2.2
ggplot(asr_sex_imd,
       aes(x = Year, y = mx,
           colour = Sex, linetype = IMD_q, group = Group)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Sex),
              alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  scale_colour_manual(values = sex_colours, name = "Sex") +
  scale_fill_manual(values = sex_colours,   name = "Sex") +
  scale_linetype_manual(values = c("Q1 (least deprived)" = "dashed",
                                    "Q5 (most deprived)"  = "solid"),
                        name = "IMD quintile") +
  labs(
    title    = "Figure 2.2 — Fall-related mortality Q1 vs Q5 by sex, England 2002–2022",
    subtitle = "Adults ≥18 years | Solid = Q5 (most deprived), Dashed = Q1 (least deprived)",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

save_fig("fig2_2_sex_imd_interaction")
save_tab(
  asr_sex_imd %>%
    select(Year, Sex, IMD_q, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`IMD quintile`    = IMD_q,
           `ASR per 100,000` = mx,
           `Lower 95% CI`    = LowerCI,
           `Upper 95% CI`    = UpperCI,
           `Total deaths`    = Deaths),
  "tab2_2_sex_imd_interaction"
)

# ── 2.3 Rate ratio and rate difference (Q5/Q1) over time ─────────────────────
message("--- Analysis 2.3: IMD rate ratio and rate difference ---")

# Pivot Q1 and Q5 to compute ratio and difference
inequality <- asr_imd %>%
  filter(IMD_q %in% c("Q1 (least deprived)", "Q5 (most deprived)")) %>%
  select(Year, IMD_q, mx, LowerCI, UpperCI) %>%
  pivot_wider(
    names_from  = IMD_q,
    values_from = c(mx, LowerCI, UpperCI),
    names_sep   = "__"
  ) %>%
  rename(
    mx_Q1  = `mx__Q1 (least deprived)`,
    mx_Q5  = `mx__Q5 (most deprived)`,
    lo_Q1  = `LowerCI__Q1 (least deprived)`,
    lo_Q5  = `LowerCI__Q5 (most deprived)`,
    hi_Q1  = `UpperCI__Q1 (least deprived)`,
    hi_Q5  = `UpperCI__Q5 (most deprived)`
  ) %>%
  mutate(
    # Rate ratio (relative inequality)
    RR    = mx_Q5 / mx_Q1,
    # Delta method CI for RR
    RR_lo = exp(log(RR) - 1.96 * sqrt(1/mx_Q5^2 * ((hi_Q5 - lo_Q5)/(2*1.96))^2 +
                                        1/mx_Q1^2 * ((hi_Q1 - lo_Q1)/(2*1.96))^2)),
    RR_hi = exp(log(RR) + 1.96 * sqrt(1/mx_Q5^2 * ((hi_Q5 - lo_Q5)/(2*1.96))^2 +
                                        1/mx_Q1^2 * ((hi_Q1 - lo_Q1)/(2*1.96))^2)),
    # Rate difference (absolute inequality)
    RD    = mx_Q5 - mx_Q1,
    # CI for RD by error propagation
    RD_se = sqrt(((hi_Q5 - lo_Q5)/(2*1.96))^2 + ((hi_Q1 - lo_Q1)/(2*1.96))^2),
    RD_lo = RD - 1.96 * RD_se,
    RD_hi = RD + 1.96 * RD_se
  )

# Figure 2.3a — Rate ratio
p_rr <- ggplot(inequality, aes(x = Year, y = RR)) +
  geom_ribbon(aes(ymin = RR_lo, ymax = RR_hi),
              fill = "#7b3294", alpha = 0.2) +
  geom_line(colour = "#7b3294", linewidth = 1.0) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     name   = "Rate ratio (Q5/Q1)") +
  labs(
    title    = "Figure 2.3a — Relative inequality in fall-related mortality (Q5/Q1 rate ratio)",
    subtitle = "All persons, adults ≥18 years",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

# Figure 2.3b — Rate difference
p_rd <- ggplot(inequality, aes(x = Year, y = RD)) +
  geom_ribbon(aes(ymin = RD_lo, ymax = RD_hi),
              fill = "#1b7837", alpha = 0.2) +
  geom_line(colour = "#1b7837", linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(name = "Rate difference (Q5 − Q1) per 100,000") +
  labs(
    title    = "Figure 2.3b — Absolute inequality in fall-related mortality (Q5−Q1 rate difference)",
    subtitle = "All persons, adults ≥18 years",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

# Save both
print(p_rr); save_fig("fig2_3a_rate_ratio")
print(p_rd); save_fig("fig2_3b_rate_difference")

save_tab(
  inequality %>%
    select(Year, mx_Q1, mx_Q5, RR, RR_lo, RR_hi, RD, RD_lo, RD_hi) %>%
    rename(`ASR Q1`              = mx_Q1,
           `ASR Q5`              = mx_Q5,
           `Rate ratio (Q5/Q1)`  = RR,
           `RR lower 95% CI`     = RR_lo,
           `RR upper 95% CI`     = RR_hi,
           `Rate difference`     = RD,
           `RD lower 95% CI`     = RD_lo,
           `RD upper 95% CI`     = RD_hi),
  "tab2_3_inequality_metrics"
)


# =============================================================================
# OBJECTIVE 3: PANDEMIC AND JOINPOINT ANALYSES
# =============================================================================

message("\n=== OBJECTIVE 3: Pandemic and joinpoint analyses ===")

# ── 3.1 Pre- vs post-pandemic comparison ─────────────────────────────────────
message("--- Analysis 3.1: Pre vs post-pandemic ---")

# Pre-pandemic: 2017-2019 | Pandemic: 2020-2022 (per protocol)
pandemic_periods <- asr_imd %>%
  filter(Year %in% c(2017:2019, 2020:2022)) %>%
  mutate(Period = ifelse(Year <= 2019, "Pre-pandemic\n(2017–2019)",
                                       "Pandemic\n(2020–2022)"),
         Period = factor(Period,
                         levels = c("Pre-pandemic\n(2017–2019)",
                                    "Pandemic\n(2020–2022)"))) %>%
  group_by(Period, IMD_q) %>%
  summarise(
    Mean_mx = mean(mx, na.rm = TRUE),
    .groups = "drop"
  )

# Percentage change pre to pandemic by quintile
pandemic_change <- pandemic_periods %>%
  pivot_wider(names_from = Period, values_from = Mean_mx) %>%
  rename(Pre  = `Pre-pandemic\n(2017–2019)`,
         Post = `Pandemic\n(2020–2022)`) %>%
  mutate(PctChange = (Post - Pre) / Pre * 100)

# Figure 3.1 — grouped bar chart
ggplot(pandemic_periods,
       aes(x = IMD_q, y = Mean_mx,
           fill = Period, group = Period)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  scale_fill_manual(values = c("Pre-pandemic\n(2017–2019)" = "#a6bddb",
                                "Pandemic\n(2020–2022)"    = "#2c7fb8"),
                    name = "Period") +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Mean age-standardised rate per 100,000") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(
    title    = "Figure 3.1 — Fall-related mortality before and during the pandemic by IMD quintile",
    subtitle = "All persons, adults ≥18 years | Pre-pandemic: 2017–2019 | Pandemic: 2020–2022",
    x        = "IMD quintile",
    caption  = caption_std
  ) +
  theme_falls()

save_fig("fig3_1_pandemic_comparison")
save_tab(
  pandemic_change %>%
    rename(`IMD quintile`                        = IMD_q,
           `Mean ASR pre-pandemic (2017–2019)`   = Pre,
           `Mean ASR pandemic (2020–2022)`        = Post,
           `Percentage change (%)`               = PctChange),
  "tab3_1_pandemic_comparison"
)

# Also by sex
pandemic_sex <- falls_cell %>%
  filter(Sex != "Total", IMD_q == "Overall",
         Year %in% c(2017:2019, 2020:2022)) %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q")) %>%
  mutate(Period = ifelse(Year <= 2019, "Pre-pandemic", "Pandemic")) %>%
  group_by(Period, Sex) %>%
  summarise(Mean_mx = mean(mx), .groups = "drop") %>%
  pivot_wider(names_from = Period, values_from = Mean_mx) %>%
  mutate(PctChange = (`Pandemic` - `Pre-pandemic`) / `Pre-pandemic` * 100)

save_tab(
  pandemic_sex %>%
    rename(`Mean ASR pre-pandemic` = `Pre-pandemic`,
           `Mean ASR pandemic`     = `Pandemic`,
           `Percentage change (%)` = PctChange),
  "tab3_1b_pandemic_by_sex"
)

# Also by age group
pandemic_age <- asr_age %>%
  filter(Year %in% c(2017:2019, 2020:2022)) %>%
  mutate(Period = ifelse(Year <= 2019, "Pre-pandemic", "Pandemic")) %>%
  group_by(Period, AgeBand) %>%
  summarise(Mean_mx = mean(mx), .groups = "drop") %>%
  pivot_wider(names_from = Period, values_from = Mean_mx) %>%
  mutate(PctChange = (`Pandemic` - `Pre-pandemic`) / `Pre-pandemic` * 100)

save_tab(
  pandemic_age %>%
    rename(`Age group`             = AgeBand,
           `Mean rate pre-pandemic` = `Pre-pandemic`,
           `Mean rate pandemic`     = `Pandemic`,
           `Percentage change (%)`  = PctChange),
  "tab3_1c_pandemic_by_age"
)

# ── 3.2 Joinpoint regression ──────────────────────────────────────────────────
message("--- Analysis 3.2: Joinpoint regression ---")

# Helper: run joinpoint on a single series, return APC table + fitted values
run_jp <- function(df, label = "") {

  df <- df %>%
    arrange(Year) %>%
    mutate(
      log_mx = log(mx),
      se     = (UpperCI - LowerCI) / (2 * 1.96),
      w      = 1 / se^2
    )

  lm_fit  <- lm(log_mx ~ Year, data = df, weights = w)

  seg_fit <- tryCatch(
    selgmented(lm_fit,
               seg.Z      = ~Year,
               Kmax       = 4,
               type       = "bic",
               bonferroni = TRUE),
    error = function(e) {
      message("  selgmented failed for: ", label, " — using linear model")
      return(NULL)
    }
  )

  if (is.null(seg_fit)) {
    # No joinpoint — single linear model
    apc_val <- (exp(coef(lm_fit)["Year"]) - 1) * 100
    apc_df  <- tibble(
      Label          = label,
      Segment        = 1,
      Joinpoint_year = NA_real_,
      APC            = apc_val,
      APC_lo         = NA_real_,
      APC_hi         = NA_real_
    )
    df$fitted <- exp(fitted(lm_fit))
    return(list(apc = apc_df, fitted = df))
  }

  # Extract joinpoints
  jp_years <- round(seg_fit$psi[, "Est."])
  apc_raw  <- slope(seg_fit)$Year

  apc_df <- tibble(
    Label          = label,
    Segment        = seq_len(nrow(apc_raw)),
    Joinpoint_year = c(NA_real_, as.numeric(jp_years)),
    APC            = (exp(apc_raw[, "Est."])      - 1) * 100,
    APC_lo         = (exp(apc_raw[, "CI(95%).l"]) - 1) * 100,
    APC_hi         = (exp(apc_raw[, "CI(95%).u"]) - 1) * 100
  )

  df$fitted <- exp(fitted(seg_fit))

  return(list(apc = apc_df, fitted = df, psi = jp_years))
}

# ── 3.2a Overall joinpoint ────────────────────────────────────────────────────
message("  Running joinpoint: overall trend")

jp_overall <- run_jp(
  asr_overall %>% filter(Sex == "Total", IMD_q == "Overall"),
  label = "Overall"
)

# Figure 3.2a
jp_years_overall <- jp_overall$psi

ggplot(jp_overall$fitted, aes(x = Year)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              fill = "#2c7fb8", alpha = 0.15) +
  geom_point(aes(y = mx), colour = "#2c7fb8", size = 1.8) +
  geom_line(aes(y = fitted), colour = "#e34a33", linewidth = 1.0) +
  geom_vline(xintercept = jp_years_overall,
             linetype = "dashed", colour = "#e34a33", linewidth = 0.5) +
  annotate("text", x = jp_years_overall + 0.3, y = Inf,
           label = paste0("JP: ", jp_years_overall),
           hjust = 0, vjust = 1.5, size = 3.2, colour = "#e34a33") +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  labs(
    title    = "Figure 3.2a — Joinpoint regression: overall fall-related mortality, England 2002–2022",
    subtitle = paste0("All persons, adults ≥18 years | ",
                      length(jp_years_overall), " joinpoint(s) | BIC selection | Log-transformed"),
    x        = NULL,
    caption  = "Source: ONS ad-hoc release 1355 | segmented::selgmented() | BIC model selection"
  ) +
  theme_falls()

save_fig("fig3_2a_joinpoint_overall")

# ── 3.2b Joinpoint by sex ─────────────────────────────────────────────────────
message("  Running joinpoint: by sex")

jp_sex_results <- asr_sex %>%
  group_by(Sex) %>%
  group_map(~ run_jp(.x, label = unique(.x$Sex)), .keep = TRUE)

jp_sex_apc    <- map_df(jp_sex_results, "apc")
jp_sex_fitted <- map_df(jp_sex_results, "fitted")

# Figure 3.2b — faceted by sex
ggplot(jp_sex_fitted, aes(x = Year)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Sex),
              alpha = 0.15, colour = NA) +
  geom_point(aes(y = mx, colour = Sex), size = 1.5) +
  geom_line(aes(y = fitted, colour = Sex), linewidth = 1.0) +
  facet_wrap(~Sex, nrow = 1) +
  scale_x_continuous(breaks = seq(2002, 2022, 3)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  scale_colour_manual(values = sex_colours) +
  scale_fill_manual(values = sex_colours) +
  labs(
    title    = "Figure 3.2b — Joinpoint regression by sex, England 2002–2022",
    subtitle = "Adults ≥18 years | BIC model selection | Log-transformed",
    x        = NULL,
    caption  = "Source: ONS ad-hoc release 1355 | segmented::selgmented()"
  ) +
  theme_falls() +
  theme(legend.position = "none")

save_fig("fig3_2b_joinpoint_by_sex")

# ── 3.2c Joinpoint by IMD quintile ────────────────────────────────────────────
message("  Running joinpoint: by IMD quintile")

jp_imd_results <- asr_imd %>%
  filter(Sex == "Total") %>%
  group_by(IMD_q) %>%
  group_map(~ run_jp(.x, label = as.character(unique(.x$IMD_q))), .keep = TRUE)

jp_imd_apc    <- map_df(jp_imd_results, "apc")
jp_imd_fitted <- map_df(jp_imd_results, "fitted") %>%
  mutate(IMD_q = factor(IMD_q, levels = imd_levels))

# Figure 3.2c — faceted by IMD quintile
ggplot(jp_imd_fitted, aes(x = Year)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = IMD_q),
              alpha = 0.15, colour = NA) +
  geom_point(aes(y = mx, colour = IMD_q), size = 1.2) +
  geom_line(aes(y = fitted, colour = IMD_q), linewidth = 0.9) +
  facet_wrap(~IMD_q, nrow = 1,
             labeller = labeller(IMD_q = label_wrap_gen(12))) +
  scale_x_continuous(breaks = seq(2002, 2022, 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  scale_colour_manual(values = quintile_colours) +
  scale_fill_manual(values = quintile_colours) +
  labs(
    title    = "Figure 3.2c — Joinpoint regression by IMD quintile, England 2002–2022",
    subtitle = "All persons, adults ≥18 years | BIC model selection | Log-transformed",
    x        = NULL,
    caption  = "Source: ONS ad-hoc release 1355 | segmented::selgmented()"
  ) +
  theme_falls() +
  theme(legend.position = "none")

save_fig("fig3_2c_joinpoint_by_imd", width = 16, height = 5)

# ── Save APC tables ───────────────────────────────────────────────────────────
all_apc <- bind_rows(
  jp_overall$apc,
  jp_sex_apc,
  jp_imd_apc
) %>%
  rename(
    `Series`          = Label,
    `Segment`         = Segment,
    `Joinpoint year`  = Joinpoint_year,
    `APC (%)`         = APC,
    `APC lower 95% CI` = APC_lo,
    `APC upper 95% CI` = APC_hi
  )

save_tab(all_apc, "tab3_2_joinpoint_apc_all_series")


# =============================================================================
# SENSITIVITY ANALYSIS: M80-M85 (disorders of bone density and structure)
# =============================================================================

message("\n=== SENSITIVITY ANALYSIS: M80-M85 ===")

# ── Build M80-M85 cell dataset ────────────────────────────────────────────────
m80_codes <- paste0("M", 80:85)

m80_cell <- mortality %>%
  filter(
    substr(icd10, 1, 3) %in% m80_codes,
    !Age5 %in% exclude_ages
  ) %>%
  group_by(Year, Sex, IMD_q, Age5) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  right_join(
    PopForASR %>% filter(!Age5 %in% exclude_ages),
    by = c("Year", "Sex", "IMD_q", "Age5")
  ) %>%
  mutate(Deaths = replace_na(Deaths, 0))

# Code breakdown (for appendix)
m80_breakdown <- mortality %>%
  filter(substr(icd10, 1, 3) %in% m80_codes,
         !Age5 %in% exclude_ages,
         Sex == "Total", IMD_q == "Overall") %>%
  group_by(icd10) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Deaths))

save_tab(m80_breakdown, "tabS_m8085_code_breakdown")

# ── SA 1.1 Overall trend (M80-M85) ───────────────────────────────────────────
asr_m80_overall <- m80_cell %>%
  filter(Sex == "Total", IMD_q == "Overall") %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q"))

# ── SA 1.4 Trend by IMD quintile (M80-M85) ───────────────────────────────────
asr_m80_imd <- m80_cell %>%
  filter(Sex == "Total", IMD_q != "Overall") %>%
  mutate(IMD_q = factor(IMD_q, levels = imd_levels)) %>%
  make_dsr(group_vars = c("Year", "Sex", "IMD_q"))

# Figure S1 — M80-M85 overall trend
ggplot(asr_m80_overall, aes(x = Year, y = mx)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              fill = "#756bb1", alpha = 0.2) +
  geom_line(colour = "#756bb1", linewidth = 1.0) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  labs(
    title    = "Figure S1 — Sensitivity analysis: M80–M85 mortality, England 2002–2022",
    subtitle = "ICD-10 M80–M85 (disorders of bone density and structure) | All persons, adults ≥18",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

save_fig("figS1_m8085_overall_trend")

# Figure S2 — M80-M85 by IMD quintile
ggplot(asr_m80_imd, aes(x = Year, y = mx,
                          colour = IMD_q, fill = IMD_q)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.0) +
  scale_x_continuous(breaks = seq(2002, 2022, 2)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_number(accuracy = 0.1),
                     name   = "Age-standardised rate per 100,000") +
  scale_colour_manual(values = quintile_colours, name = "IMD quintile") +
  scale_fill_manual(values = quintile_colours,   name = "IMD quintile") +
  labs(
    title    = "Figure S2 — Sensitivity analysis: M80–M85 mortality by IMD quintile, England 2002–2022",
    subtitle = "ICD-10 M80–M85 | All persons, adults ≥18 years",
    x        = NULL,
    caption  = caption_std
  ) +
  theme_falls()

save_fig("figS2_m8085_by_imd")

# Save sensitivity tables
save_tab(
  asr_m80_overall %>%
    select(Year, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`ASR per 100,000` = mx,
           `Lower 95% CI`    = LowerCI,
           `Upper 95% CI`    = UpperCI,
           `Total deaths`    = Deaths),
  "tabS1_m8085_overall_trend"
)

save_tab(
  asr_m80_imd %>%
    select(Year, IMD_q, mx, LowerCI, UpperCI, Deaths) %>%
    rename(`IMD quintile`    = IMD_q,
           `ASR per 100,000` = mx,
           `Lower 95% CI`    = LowerCI,
           `Upper 95% CI`    = UpperCI,
           `Total deaths`    = Deaths),
  "tabS2_m8085_by_imd"
)


# =============================================================================
# COMPLETION SUMMARY
# =============================================================================

message("\n", strrep("=", 60))
message("ANALYSIS COMPLETE")
message(strrep("=", 60))
message("\nFigures saved to: outputs/figures/")
message("Tables saved to:  outputs/tables/")
message("\nFigures produced:")
message("  Primary analysis:")
message("    fig1_1_overall_trend")
message("    fig1_2_trend_by_sex")
message("    fig1_3_trend_by_age")
message("    fig1_4_trend_by_imd")
message("    fig2_1_age_imd_interaction")
message("    fig2_2_sex_imd_interaction")
message("    fig2_3a_rate_ratio")
message("    fig2_3b_rate_difference")
message("    fig3_1_pandemic_comparison")
message("    fig3_2a_joinpoint_overall")
message("    fig3_2b_joinpoint_by_sex")
message("    fig3_2c_joinpoint_by_imd")
message("  Sensitivity analysis:")
message("    figS1_m8085_overall_trend")
message("    figS2_m8085_by_imd")
message(strrep("=", 60))
