# Fall-related mortality and deprivation in England, 2002–2022

Analysis code for:

> **Trends and socioeconomic inequalities in fall-related deaths: an observational study of national mortality data in England, 2002–2022**
> Francesco Dernie, Adam Carter, Bryony Elliott

Presented at the British Geriatrics Society Autumn Meeting, Nottingham, November 2026.

**Pre-registered protocol:** OSF - [_click here_](https://osf.io/cqauh/overview)

**Data source:** - [_click here_](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1355deathsbysexsingleyearofageunderlyingcauseicd10codeanddeprivationdecileengland2001to2022)

---

## What this repository contains

The code builds a national dataset of deaths and population by year, sex, age, and deprivation, then analyses fall-related mortality. **No data are stored here.** All inputs are public ONS and MHCLG files that the scripts download automatically.

```
build_mortality_dataset.R      Step 1: run this to download and build the dataset
R/
  00_build_popasr.R            Complete population table for age-standardisation
  00_make_dsr.R                make_dsr(): age-standardised rates with Dobson 95% CIs
  01_download_deaths.R         ONS ad-hoc release 1355 (deaths)
  02_process_deaths.R          Reshape deaths; collapse to 5-year age bands
  03_download_population.R     ONS LSOA mid-year population estimates
  04_download_imd.R            IMD 2004/2007/2010/2015/2019 and LSOA lookups
  05_build_population.R        Join population to IMD; aggregate to quintiles
  06_combine.R                 Join deaths to population; compute rates
falls_analysis.R               Step 2: the pre-registered falls analysis
validation/
  sanity_check_v2.R            Validation against Oldham et al. (2025)
```

## How to run

1. Open the project folder in RStudio (or set it as the working directory). Paths use the `here` package, so they work on any computer.
2. Run `build_mortality_dataset.R`. This downloads about 30 source files and writes the dataset to `data/`. It takes some time on first run.
3. Run `falls_analysis.R`. Figures go to `outputs/figures/` and tables to `outputs/tables/`.

## Data sources

| Component | Source |
|---|---|
| Deaths | ONS ad-hoc release 1355: deaths by sex, single year of age, four-digit ICD-10 underlying cause and IMD decile, England, 2001–2022 |
| Population | ONS LSOA mid-year population estimates, by sex and single year of age |
| Deprivation | English Index of Multiple Deprivation, LSOA level: IMD 2004 (2002–05), 2007 (2006–08), 2010 (2009–12), 2015 (2013–17), 2019 (2018–22) |
| Standard population | 2013 European Standard Population |

## Methods summary

- **Case definition:** ICD-10 W00–W19 (falls) as the underlying cause of death, adults aged 18 and over. Sensitivity analysis: M80–M85.
- **Deprivation:** IMD quintiles, Q1 least deprived to Q5 most deprived, using the IMD version appropriate to each registration year.
- **Age-standardised rates:** direct method, 2013 ESP, with 95% CIs by Dobson's method (`PHEindicatormethods`).
- **Age-band rates:** crude rates within pre-specified bands (<65, 65–74, 75–84, 85+).
- **Trends:** joinpoint regression on log-transformed rates (`segmented`), with BIC model selection; annual percentage change = 100 × (exp(β) − 1).
- **Inequality:** Q5/Q1 rate ratio and Q5 − Q1 rate difference.

## Software

R 4.5.3. Packages: tidyverse, here, fs, curl, readxl, data.table, gtools, PHEindicatormethods, segmented, scales, MASS.

Exact package versions are recorded in `sessionInfo.txt` _(to add after the final run)_.

## Acknowledgement

The data pipeline adapts the approach and code of Oldham M, Jackson S, Brown J, et al. Trends in alcohol-specific deaths in England, 2001–22: an observational study. *Lancet Public Health* 2025. doi:10.1016/S2468-2667(25)00047-7 ([code](https://github.com/VictimOfMaths/Publications)).

## Licence

MIT
