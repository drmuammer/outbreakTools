#!/usr/bin/env Rscript
# =============================================================================
# outbreakTools — Simulated Foodborne Outbreak Linelist
# File: data-raw/generate_demo_data.R
# Authors: Gülser Doğan Türkçelik & Muammer Beslen — Türkiye FETP
#
# Dataset: Point-source foodborne outbreak (250 cases + 250 controls)
# Intentional data quality issues included for training purposes.
# =============================================================================

set.seed(2025)
N <- 500   # 250 cases + 250 controls

# ── EXPOSURE (food items) ────────────────────────────────────────────────────
set.seed(2025)
n_cases <- 250; n_ctrl <- 250

# True exposure-outcome associations (for demonstration):
# ate_chicken: RR ~2.5, ate_salad: RR ~1.8, drank_juice: RR ~1.2 (confounder)
# age_group confounds chicken exposure

# Generate cases
case_df <- data.frame(
  case_control   = "Case",
  ate_chicken    = sample(c("Yes","No"), n_cases, TRUE, c(0.70, 0.30)),
  ate_salad      = sample(c("Yes","No"), n_cases, TRUE, c(0.55, 0.45)),
  ate_dessert    = sample(c("Yes","No"), n_cases, TRUE, c(0.60, 0.40)),
  drank_juice    = sample(c("Yes","No"), n_cases, TRUE, c(0.65, 0.35)),
  drank_alcohol  = sample(c("Yes","No"), n_cases, TRUE, c(0.30, 0.70)),
  age_years      = round(rnorm(n_cases, 35, 12)),
  sex            = sample(c("Male","Female"), n_cases, TRUE, c(0.48, 0.52)),
  district       = sample(paste0("District_", LETTERS[1:6]), n_cases, TRUE,
                           c(0.25,0.20,0.18,0.15,0.12,0.10)),
  occupation     = sample(c("Student","Health worker","Office worker",
                             "Farmer","Other"), n_cases, TRUE),
  vaccination_status = sample(c("Vaccinated","Not vaccinated","Unknown"),
                               n_cases, TRUE, c(0.40,0.50,0.10)),
  stringsAsFactors = FALSE
)

# Onset dates — point source: cluster around day 2-4 of event
onset_days <- pmax(0, pmin(10, round(rnorm(n_cases, 3, 1.2))))
event_date <- as.Date("2024-06-15")
case_df$date_onset  <- event_date + onset_days
case_df$date_report <- case_df$date_onset + sample(0:3, n_cases, TRUE)

# Generate controls (lower exposure prevalence)
ctrl_df <- data.frame(
  case_control   = "Control",
  ate_chicken    = sample(c("Yes","No"), n_ctrl, TRUE, c(0.35, 0.65)),
  ate_salad      = sample(c("Yes","No"), n_ctrl, TRUE, c(0.35, 0.65)),
  ate_dessert    = sample(c("Yes","No"), n_ctrl, TRUE, c(0.40, 0.60)),
  drank_juice    = sample(c("Yes","No"), n_ctrl, TRUE, c(0.40, 0.60)),
  drank_alcohol  = sample(c("Yes","No"), n_ctrl, TRUE, c(0.25, 0.75)),
  age_years      = round(rnorm(n_ctrl, 35, 12)),
  sex            = sample(c("Male","Female"), n_ctrl, TRUE, c(0.48, 0.52)),
  district       = sample(paste0("District_", LETTERS[1:6]), n_ctrl, TRUE,
                           c(0.20,0.20,0.18,0.18,0.12,0.12)),
  occupation     = sample(c("Student","Health worker","Office worker",
                             "Farmer","Other"), n_ctrl, TRUE),
  vaccination_status = sample(c("Vaccinated","Not vaccinated","Unknown"),
                               n_ctrl, TRUE, c(0.45,0.45,0.10)),
  stringsAsFactors = FALSE
)
ctrl_df$date_onset  <- NA_character_
ctrl_df$date_report <- event_date + sample(-2:2, n_ctrl, TRUE)

# Combine
linelist <- rbind(case_df, ctrl_df)
linelist$case_id <- paste0("OB2024-", sprintf("%04d", seq_len(nrow(linelist))))

# Age group
linelist$age_group <- cut(linelist$age_years,
  breaks = c(-Inf, 4, 14, 24, 44, 64, Inf),
  labels = c("<5","5-14","15-24","25-44","45-64","65+"), right=TRUE)

# Symptoms (cases more likely)
linelist$symptom_vomiting  <- ifelse(linelist$case_control=="Case",
  sample(c("Yes","No"), N, TRUE, c(0.72,0.28)),
  sample(c("Yes","No"), N, TRUE, c(0.20,0.80)))
linelist$symptom_diarrhoea <- ifelse(linelist$case_control=="Case",
  sample(c("Yes","No"), N, TRUE, c(0.85,0.15)),
  sample(c("Yes","No"), N, TRUE, c(0.15,0.85)))
linelist$symptom_fever     <- ifelse(linelist$case_control=="Case",
  sample(c("Yes","No"), N, TRUE, c(0.45,0.55)),
  sample(c("Yes","No"), N, TRUE, c(0.10,0.90)))
linelist$symptom_nausea    <- ifelse(linelist$case_control=="Case",
  sample(c("Yes","No"), N, TRUE, c(0.68,0.32)),
  sample(c("Yes","No"), N, TRUE, c(0.18,0.82)))

# ── INTRODUCE DATA QUALITY ISSUES (for training) ─────────────────────────────

# 1. Inconsistent sex coding
bad_sex_idx <- sample(which(linelist$sex=="Male"), 10)
linelist$sex[bad_sex_idx] <- sample(c("male","MALE","M"), 10, TRUE)
bad_sex_idx2 <- sample(which(linelist$sex=="Female"), 8)
linelist$sex[bad_sex_idx2] <- sample(c("female","FEMALE","F"), 8, TRUE)

# 2. Future dates (2 records)
linelist$date_onset[c(5, 42)] <- as.Date(c("2026-03-15", "2027-01-01"))

# 3. Impossible/out-of-range ages
linelist$age_years[c(15, 88)] <- c(-3, 155)

# 4. Missing values
linelist$ate_chicken[sample(N, 15)]      <- NA
linelist$vaccination_status[sample(N,25)] <- NA
linelist$district[sample(N, 12)]          <- NA
linelist$age_years[sample(N, 18)]         <- NA

# 5. Duplicate IDs (3 pairs)
linelist$case_id[c(20, 21)]  <- linelist$case_id[c(20, 20)]
linelist$case_id[c(150, 151)] <- linelist$case_id[c(150, 150)]

# 6. Combined column (age_sex, for cleaning exercise)
linelist$age_sex <- paste0(linelist$age_years, "/",
                           toupper(substr(linelist$sex, 1, 1)))
linelist$age_sex[sample(N, 5)] <- NA

# 7. Mixed date formats in date_report
mixed_idx <- sample(N, 30)
linelist$date_report[mixed_idx] <- format(
  as.Date(linelist$date_report[mixed_idx]), "%d/%m/%Y")

# Outcome variable (binary, from case_control)
linelist$outcome <- ifelse(linelist$case_control == "Case", 1L, 0L)

# Final column order
linelist <- linelist[, c("case_id", "case_control", "outcome",
                          "date_onset", "date_report",
                          "age_years", "age_group", "age_sex", "sex",
                          "district", "occupation", "vaccination_status",
                          "ate_chicken", "ate_salad", "ate_dessert",
                          "drank_juice", "drank_alcohol",
                          "symptom_vomiting", "symptom_diarrhoea",
                          "symptom_fever", "symptom_nausea")]

dir.create("data", showWarnings=FALSE)
write.csv(linelist, "data/foodborne_outbreak.csv", row.names=FALSE, na="")

cat("✅ Demo dataset: data/foodborne_outbreak.csv\n")
cat(sprintf("   N=%d | Cases=%d | Controls=%d\n",
    nrow(linelist),
    sum(linelist$case_control=="Case", na.rm=TRUE),
    sum(linelist$case_control=="Control", na.rm=TRUE)))
cat("   Intentional data quality issues embedded for training.\n")
