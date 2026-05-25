# outbreakTools — Outbreak Analysis Tools for Field Epidemiology

[![jamovi module](https://img.shields.io/badge/jamovi-module-blue)](https://www.jamovi.org)
[![FETP](https://img.shields.io/badge/Turkey%20FETP-Outbreak%20Analysis-red)](https://hsgm.saglik.gov.tr)
[![Version](https://img.shields.io/badge/version-1.1.0-green)](https://github.com/drmuammer/outbreakTools/releases/latest)
[![License: GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-blue)](LICENSE)
[![Platforms](https://img.shields.io/badge/platforms-Windows%20%E2%9C%85%20%7C%20macOS%20%F0%9F%94%9C%20%7C%20Linux%20%F0%9F%94%9C-lightgrey)](#installation)

> **Developed by Gülser Doğan Türkçelik & Muammer Beslen — Turkey FETP**
> Field Epidemiology Training Programme, Turkey
> *"Epidemiological expertise supported by reproducible analytics"*

---

## Overview

**outbreakTools** is a comprehensive jamovi module designed for Field Epidemiology Training Programme (FETP) practitioners conducting outbreak investigations. It bridges the capability gap between Epi Info and full R programming by providing point-and-click access to publication-grade epidemiological analyses within jamovi's familiar interface.

The module implements statistically rigorous methods following international field epidemiology standards, with full bibliographic references embedded in each analysis output.

---

## Installation

### For end users — install the prebuilt module

Download the platform-specific `.jmo` file from the **[latest release](https://github.com/drmuammer/outbreakTools/releases/latest)** and sideload it in jamovi.

| Platform | File | Status |
|---|---|---|
| **Windows (64-bit)** | `outbreakTools_1.1.0_win64.jmo` | ✅ Available |
| **macOS (Apple Silicon)** | `outbreakTools_1.1.0_macos_arm64.jmo` | 🔜 Coming soon |
| **macOS (Intel)** | `outbreakTools_1.1.0_macos_intel.jmo` | 🔜 Coming soon |
| **Linux (x86_64)** | `outbreakTools_1.1.0_linux.jmo` | 🔜 Coming soon |

**Steps:**

1. Download the `.jmo` file matching your operating system from the releases page
2. Open jamovi
3. Click the **⊕ Modules** icon in the top-right corner
4. Choose **Sideload module** (or **Install from file** in older versions)
5. Select the downloaded `.jmo` file
6. After a brief installation, the **Outbreak Tools** menu appears in the analyses ribbon

**Requirements:** jamovi ≥ 2.3.0 ([download here](https://www.jamovi.org/download.html))

### For developers — build from source

If your platform's binary isn't yet available, or you want to modify the module, see [INSTALL.md](INSTALL.md) for full build instructions.

Quick start:

```r
# In R / RStudio:
install.packages('jmvtools',
  repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))

# Set working directory to the cloned repo, then:
jmvtools::install(home = "<path-to-your-jamovi-installation>")
```

Platform-specific jamovi paths:

- **Windows:** `C:/Program Files/jamovi <version>`
- **macOS:** `/Applications/jamovi.app/Contents/Frameworks/jamovi`
- **Linux (flatpak):** `~/.var/app/org.jamovi.jamovi/data/jamovi`

---

## Analyses Included

| # | Analysis | Menu | Key Outputs |
|---|---|---|---|
| 1 | **Data Quality & Linelist Check** *(v1.1: ★ OpenRefine-style cleaning)* | 01. Data Management | Duplicates, missing, date errors, range checks, **fingerprint clustering**, **date-format diversity**, **copy-paste R cleaning recipes** |
| 2 | **Descriptive Epidemiology** | 02. Descriptive Analysis | Freq tables, attack rates, Wilson 95% CI |
| 3 | **Epidemic Curve** | 02. Descriptive Analysis | ISO epi-week histogram, case table, group stacking |
| 4 | **Cohort Study — Risk Analysis** | 03. Analytic Epidemiology | RR, AR, PAR, chi-square |
| 5 | **Case-Control Study — Odds Ratio** | 03. Analytic Epidemiology | OR, Woolf CI, Fisher exact |
| 6 | **Stratified Analysis (Mantel-Haenszel)** | 03. Analytic Epidemiology | MH-RR/OR, Breslow-Day test |
| 7 | **Logistic Regression** | 03. Analytic Epidemiology | Crude/adjusted OR, forest plot, model fit |
| 8 | **Sample Size Calculator** | 04. Study Design | N by design, power curve |

### What's new in v1.1.0

The **Data Quality & Linelist Check** analysis gained a **Smart Data
Cleaning Assistant** inspired by [OpenRefine](https://openrefine.org/):

- **Fingerprint clustering** — automatically groups string variants
  (`"İstanbul"`, `"istanbul"`, `"ISTANBUL"`, `"Ist."` → one cluster) with
  Turkish-character awareness (`İ → i`, `ı → i`, `ş → s`, `ç → c`, ...).
- **n-gram clustering** — catches near-typos (`"Ankara"` vs `"Ankra"`).
- **Date format diversity detector** — flags columns where multiple date
  formats (ISO, DMY, Turkish DD.MM.YYYY, Excel serial, ...) are mixed.
- **Copy-paste R recipes** — every detected issue gets a ready-to-run R
  code snippet you can paste into **Rj Editor**, RStudio, or RGui to clean
  the data, including merge-cluster code with auto-suggested canonical
  values.

Additional fixes:

- Sample Size Calculator no longer crashes on launch
- Pedagogically ordered menu: Data Management → Descriptive → Analytic → Study Design
- Wide-span epidemic curves now use smarter axis labels (no more overlap)
- Cross-platform encoding stability for non-ASCII data

---

## Statistical Methods & References

### Confidence Intervals for Proportions
- **Wilson score interval** (default, recommended): Wilson EB (1927). Probable inference, the law of succession, and statistical inference. *Journal of the American Statistical Association* 22:209–212. doi:10.2307/2276774
- **Clopper-Pearson exact**: Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika* 26:404–413.
- **Brown et al. (2001)**: Brown LD, Cai TT, DasGupta A. Interval estimation for a binomial proportion. *Statistical Science* 16(2):101–133.

### Risk Ratio (Cohort Studies)
- Log-method Wald confidence interval: **Rothman KJ, Greenland S, Lash TL (2008)**. *Modern Epidemiology*, 3rd ed. Lippincott Williams & Wilkins. pp.254–260.
- Attributable Risk and Population AR (Levin's formula): **Levin ML (1953)**. The occurrence of lung cancer in man. *Acta Unio Int Contra Cancrum* 9:531–541.

### Odds Ratio (Case-Control Studies)
- Woolf log CI: **Woolf B (1955)**. On estimating the relation between blood group and disease. *Annals of Human Genetics* 19:251–253.
- Haldane-Anscombe correction: **Anscombe FJ (1956)**. On estimating binomial response relations. *Biometrika* 43:461–464.
- **Schlesselman JJ (1982)**. *Case-Control Studies: Design, Conduct, Analysis*. Oxford University Press.

### Mantel-Haenszel Stratified Analysis
- MH formula: **Mantel N, Haenszel W (1959)**. Statistical aspects of the analysis of data from retrospective studies of disease. *Journal of the National Cancer Institute* 22:719–748.
- Variance of MH-RR: **Greenland S, Robins JM (1985)**. Estimation of a common effect parameter from sparse follow-up data. *American Journal of Epidemiology* 121:885–900.
- Variance of MH-OR: **Robins J, Breslow N, Greenland S (1986)**. Estimators of the Mantel-Haenszel variance consistent in both sparse data and large-strata limiting models. *American Journal of Epidemiology* 124:719–723.
- Breslow-Day test: **Breslow NE, Day NE (1980)**. *Statistical Methods in Cancer Research, Vol. I: The Analysis of Case-Control Studies*. IARC Scientific Publications No. 32. Lyon: International Agency for Research on Cancer. pp.142–146.

### Logistic Regression
- **Hosmer DW, Lemeshow S (2000)**. *Applied Logistic Regression*, 2nd ed. John Wiley & Sons.
- Variable screening (p < 0.25): Hosmer & Lemeshow (2000) p.95.
- Profile likelihood CI: **Venables WN, Ripley BD (2002)**. *Modern Applied Statistics with S*, 4th ed. Springer.
- 10% change-in-estimate rule: Rothman, Greenland & Lash (2008) p.254.

### Sample Size
- Cohort design: **Kelsey JL, Thompson WD, Evans AS (1996)**. *Methods in Observational Epidemiology*, 2nd ed. Oxford University Press. Table 4-7.
- Case-control design: **Schlesselman JJ (1982)**. *Case-Control Studies*. Oxford University Press. pp.144–150.

### Field Epidemiology Guidelines
- **CDC (2012)**. *Principles of Epidemiology in Public Health Practice*, 3rd ed. US Department of Health and Human Services.
- **Gregg MB, ed. (2002)**. *Field Epidemiology*, 2nd ed. Oxford University Press.
- **Porta M, ed. (2014)**. *A Dictionary of Epidemiology*, 6th ed. Oxford University Press.

---

## Demo Dataset

A simulated foodborne outbreak linelist (`inst/extdata/foodborne_outbreak.csv`, N=500: 250 cases + 250 controls) is included with the module.

To regenerate or modify:
```r
source("data-raw/generate_demo_data.R")
```

The dataset includes intentional data quality issues for training:
- Duplicate case IDs (3 pairs)
- Future onset dates (2 records)
- Out-of-range age values (-3, 155)
- Inconsistent sex coding ("Male", "male", "MALE", "M")
- Combined age/sex column (`65/M` format)
- Mixed date formats in `date_report`
- Missing values in multiple variables

### Suggested training workflow

1. **Data Quality Check** → `case_id`, `date_onset`, `sex`, `district`, `age_years`
2. **Descriptive Epi** → Case Status = `case_control` (Case), Person vars = `sex`, `age_group`, `district`
3. **Epidemic Curve** → Date = `date_onset`, Group = `case_control`, Unit = Day
4. **Cohort Analysis** → Exposure = `ate_chicken` (Yes), Outcome = `case_control` (Case)
5. **Case-Control** → Exposure = `ate_salad` (Yes), Case/Control = `case_control` (Case)
6. **Stratified MH** → Exposure = `ate_chicken`, Outcome = `case_control`, Stratum = `sex`
7. **Logistic Regression** → Outcome = `case_control` (Case), Predictors = `ate_chicken` + `ate_salad` + `drank_juice` + `age_group` + `sex`
8. **Sample Size** → Case-control, p₀=35%, OR=2.5, α=0.05, power=80%

---

## File Structure

```
outbreakTools/
├── DESCRIPTION                      ← R package metadata & authorship
├── NAMESPACE                        ← R namespace declarations
├── LICENSE                          ← GPL-3.0 full text
├── README.md                        ← This document
├── INSTALL.md                       ← Cross-platform build guide
├── CONTRIBUTORS.md                  ← Contribution credits
│
├── R/                               ← R analysis implementations
│   ├── utils.R                      ← Core stats + Smart Cleaning helpers
│   ├── obtclean.b.R / .h.R          ← Data Quality & Linelist Check
│   ├── obtdescriptive.b.R / .h.R    ← Descriptive Epi
│   ├── obtepicurve.b.R / .h.R       ← Epidemic Curve
│   ├── obtcohort.b.R / .h.R         ← Cohort Study
│   ├── obtcasecontrol.b.R / .h.R    ← Case-Control Study
│   ├── obtstratified.b.R / .h.R     ← Stratified Analysis (MH)
│   ├── obtlogistic.b.R / .h.R       ← Logistic Regression
│   └── obtsamplesize.b.R / .h.R     ← Sample Size Calculator
│
├── jamovi/                          ← jamovi UI and results definitions
│   ├── 0000.yaml                    ← Module manifest
│   ├── obt*.a.yaml                  ← Analysis options (one per analysis)
│   ├── obt*.r.yaml                  ← Result table specifications
│   └── obt*.u.yaml                  ← UI layouts (auto-generated)
│
├── inst/extdata/
│   └── foodborne_outbreak.csv       ← Bundled demo dataset
│
└── data-raw/
    └── generate_demo_data.R         ← Script to regenerate the demo
```

---

## Citation

If using this module in outbreak reports, publications, or FETP theses:

**APA (7th edition)**
> Doğan Türkçelik, G., & Beslen, M. (2026). *outbreakTools: Outbreak Analysis Tools for Field Epidemiology* (Version 1.1.0) [jamovi module]. Turkey FETP. https://github.com/drmuammer/outbreakTools

**Vancouver**
> Doğan Türkçelik G, Beslen M. outbreakTools: Outbreak Analysis Tools for Field Epidemiology [Internet]. Version 1.1.0. Turkey FETP; 2026. Available from: https://github.com/drmuammer/outbreakTools

**In-text (English)**
> "Analyses were conducted using the outbreakTools jamovi module (v1.1.0; Doğan Türkçelik & Beslen, 2026, Turkey FETP)."

**In-text (Turkish)**
> "Analizler, Gülser Doğan Türkçelik ve Muammer Beslen tarafından Türkiye FETP kapsamında geliştirilen outbreakTools jamovi modülü (v1.1.0) kullanılarak yürütülmüştür."

---

## Bug Reports & Contributions

Found an issue or want to suggest an improvement?

- **Bug reports:** [Open an issue](https://github.com/drmuammer/outbreakTools/issues/new)
- **Feature suggestions:** Same — issues with the `enhancement` label
- **Pull requests** are welcome; see `CONTRIBUTORS.md` for guidelines

---

## License

This module is licensed under the **GNU General Public License v3.0** — see [LICENSE](LICENSE) for the full text.

You may freely use, modify, and redistribute this software, provided that any derivative work is also distributed under GPL-3.0.

---

## Acknowledgements

Developed within the framework of the **Turkey Field Epidemiology Training Programme (FETP)**. Statistical methodology based on WHO, CDC, and peer-reviewed epidemiological literature standards.

Special thanks to the jamovi development team for the excellent R6 / jmvcore framework that makes modules like this possible.
