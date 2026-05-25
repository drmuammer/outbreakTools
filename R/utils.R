# =============================================================================
# outbreakTools — Core Statistical Utility Functions
# File: R/utils.R
# Authors: Gülser Doğan Türkçelik & Muammer Beslen — Türkiye FETP
# Version: 1.0.0
#
# Statistical References:
#   - Rothman KJ, Greenland S, Lash TL (2008). Modern Epidemiology, 3rd ed.
#     Lippincott Williams & Wilkins.
#   - Schlesselman JJ (1982). Case-Control Studies. Oxford University Press.
#   - Wilson EB (1927). Probable inference, the law of succession, and
#     statistical inference. JASA 22:209-212.
#   - Mantel N, Haenszel W (1959). Statistical aspects of the analysis of
#     data from retrospective studies of disease. JNCI 22:719-748.
#   - Breslow NE, Day NE (1980). Statistical Methods in Cancer Research.
#     Vol. 1. IARC Scientific Publications No. 32. Lyon: IARC.
#   - CDC (2012). Principles of Epidemiology in Public Health Practice, 3rd ed.
#   - Kelsey JL et al. (1996). Methods in Observational Epidemiology, 2nd ed.
# =============================================================================


# -----------------------------------------------------------------------------
# NULL-COALESCING OPERATOR (compatibility shim)
# Defined in base R from 4.4.0 onwards; we declare it locally so the module
# also works on jamovi instances bundling R 4.1–4.3.
# -----------------------------------------------------------------------------
if (!exists("%||%", envir = baseenv(), inherits = FALSE)) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}


# -----------------------------------------------------------------------------
# DATE PARSING
# -----------------------------------------------------------------------------

#' Parse a vector to Date class using multiple format strategies
#'
#' Attempts to parse character/numeric vectors into Date objects.
#' Handles Excel serial dates, ISO 8601, DD/MM/YYYY, MM/DD/YYYY,
#' and mixed-separator formats.
#'
#' @param x     Character, numeric, or Date vector.
#' @param format One of "ymd", "dmy", "mdy", "auto". Default "auto".
#' @return Date vector. Unparseable values become NA with a warning attribute.
.obt_parseDate <- function(x, format = "auto") {
  if (inherits(x, "Date")) return(x)

  # Excel serial date (Windows 1900 epoch)
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  x <- as.character(x)
  x <- stringr::str_trim(x)
  # Standardise common missing-value codes
  x[x %in% c("", "NA", "N/A", ".", "99/99/99", "00/00/0000",
              "99-99-9999", "Unknown", "unknown")] <- NA

  if (format == "auto") {
    fns <- list(
      lubridate::ymd,
      lubridate::dmy,
      lubridate::mdy
    )
    results <- lapply(fns, function(f) suppressWarnings(f(x, quiet = TRUE)))
    n_valid  <- sapply(results, function(r) sum(!is.na(r)))
    return(results[[which.max(n_valid)]])
  }

  fn <- switch(format,
    ymd  = lubridate::ymd,
    dmy  = lubridate::dmy,
    mdy  = lubridate::mdy,
    lubridate::ymd
  )
  suppressWarnings(fn(x, quiet = TRUE))
}


#' Floor a Date vector to the start of an epidemiological period
#'
#' @param dates Date vector.
#' @param unit  One of "day", "week" (ISO Mon-start), "week_sun", "month".
#' @return Date vector.
.obt_floorDate <- function(dates, unit = "week") {
  switch(unit,
    day      = lubridate::floor_date(dates, "day"),
    week     = lubridate::floor_date(dates, "week", week_start = 1),
    week_sun = lubridate::floor_date(dates, "week", week_start = 7),
    month    = lubridate::floor_date(dates, "month"),
    lubridate::floor_date(dates, "week", week_start = 1)
  )
}


#' Format period label for display
.obt_periodLabel <- function(d, unit = "week") {
  switch(unit,
    day      = format(d, "%d %b %Y"),
    week     = paste0("EW ", lubridate::isoweek(d), "/",
                      lubridate::isoyear(d)),
    week_sun = paste0("Wk ", format(d, "%d %b %Y")),
    month    = format(d, "%b %Y"),
    format(d, "%d %b %Y")
  )
}


# -----------------------------------------------------------------------------
# CONFIDENCE INTERVALS FOR PROPORTIONS
# Reference: Wilson (1927); Brown et al. (2001) Statistical Science 16:101-133
# -----------------------------------------------------------------------------

#' Wilson score confidence interval for a proportion
#'
#' Recommended for field epidemiology due to better coverage than Wald,
#' especially at extreme proportions and small sample sizes.
#' Reference: Wilson (1927); Brown, Cai & DasGupta (2001).
#'
#' @param x     Integer. Number of successes (cases).
#' @param n     Integer. Total observations.
#' @param level Numeric. Confidence level (default 0.95).
#' @return Named numeric vector: lower, upper.
.obt_wilsonCI <- function(x, n, level = 0.95) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  z      <- stats::qnorm(1 - (1 - level) / 2)
  p      <- x / n
  denom  <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half   <- (z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / denom
  c(lower = max(0, centre - half), upper = min(1, centre + half))
}


#' Clopper-Pearson exact confidence interval
#' Reference: Clopper CJ, Pearson ES (1934). Biometrika 26:404-413.
.obt_exactCI <- function(x, n, level = 0.95) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  alpha <- 1 - level
  lower <- if (x == 0) 0 else stats::qbeta(alpha / 2, x, n - x + 1)
  upper <- if (x == n) 1 else stats::qbeta(1 - alpha / 2, x + 1, n - x)
  c(lower = lower, upper = upper)
}


#' Dispatch to the selected CI method
#' @param method One of "wilson", "exact", "wald".
.obt_propCI <- function(x, n, method = "wilson", level = 0.95) {
  fn <- switch(method,
    wilson = .obt_wilsonCI,
    exact  = .obt_exactCI,
    wald   = function(x, n, level) {
      p <- x / n; z <- stats::qnorm(1 - (1 - level) / 2)
      se <- sqrt(p * (1 - p) / n)
      c(lower = max(0, p - z * se), upper = min(1, p + z * se))
    },
    .obt_wilsonCI
  )
  fn(x, n, level)
}


# -----------------------------------------------------------------------------
# RISK RATIO (RELATIVE RISK)
# Reference: Rothman & Greenland (2008) pp. 254-260; Kelsey (1996)
# -----------------------------------------------------------------------------

#' Compute Risk Ratio with log-method Wald CI and chi-square p-value
#'
#' Cell notation (standard 2×2):
#'   Exposed   Cases=a, Non-cases=b, Total n1=a+b
#'   Unexposed Cases=c, Non-cases=d, Total n0=c+d
#'
#' RR = (a/n1) / (c/n0)
#' SE(log RR) = sqrt(b/(a*n1) + d/(c*n0))  [Rothman & Greenland 2008 p.255]
#'
#' @return Named numeric: rr, ci_low, ci_high, p_value, ar, par
.obt_RR <- function(a, b, c, d, level = 0.95) {
  n1 <- a + b; n0 <- c + d; n <- n1 + n0
  if (n1 == 0 || n0 == 0 || (a == 0 && c == 0)) {
    return(list(rr=NA, ci_low=NA, ci_high=NA, p_value=NA,
                ar=NA, ar_ci_low=NA, ar_ci_high=NA, par=NA))
  }
  r1 <- a / n1; r0 <- c / n0
  rr  <- r1 / r0
  z   <- stats::qnorm(1 - (1 - level) / 2)

  # SE of log(RR): Woolf method
  if (a == 0 || c == 0) {
    a_ <- a + 0.5; b_ <- b + 0.5; c_ <- c + 0.5; d_ <- d + 0.5
    n1_ <- a_ + b_; n0_ <- c_ + d_
    rr   <- (a_/n1_) / (c_/n0_)
    se_log <- sqrt(b_/(a_*n1_) + d_/(c_*n0_))
  } else {
    se_log <- sqrt(b/(a*n1) + d/(c*n0))
  }

  log_rr <- log(rr)
  ci_low  <- exp(log_rr - z * se_log)
  ci_high <- exp(log_rr + z * se_log)

  # Pearson chi-square (uncorrected) for p-value
  expected <- c((a+c)*n1/n, (a+c)*n0/n, (b+d)*n1/n, (b+d)*n0/n)
  observed <- c(a, c, b, d)
  chi2 <- sum((observed - expected)^2 / expected)
  p_val <- stats::pchisq(chi2, df = 1, lower.tail = FALSE)

  # Attributable Risk (AR = RD = r1 - r0)
  ar      <- r1 - r0
  se_ar   <- sqrt(r1*(1-r1)/n1 + r0*(1-r0)/n0)
  ar_low  <- ar - z * se_ar
  ar_high <- ar + z * se_ar

  # Population Attributable Risk (PAR) — Levin's formula
  # PAR = (r_total - r0) / r_total
  r_total <- (a + c) / n
  par     <- if (r_total > 0) (r_total - r0) / r_total else NA_real_

  list(rr=rr, ci_low=ci_low, ci_high=ci_high, p_value=p_val,
       ar=ar, ar_ci_low=ar_low, ar_ci_high=ar_high, par=par,
       r1=r1, r0=r0, chi2=chi2)
}


# -----------------------------------------------------------------------------
# ODDS RATIO
# Reference: Schlesselman (1982); Rothman & Greenland (2008) pp. 146-152
# -----------------------------------------------------------------------------

#' Compute Odds Ratio with Woolf log CI and chi-square p-value
#'
#' OR = (a*d) / (b*c)
#' SE(log OR) = sqrt(1/a + 1/b + 1/c + 1/d)  [Woolf 1955]
#' Haldane-Anscombe correction (+0.5) applied if any cell = 0.
#'
#' @return Named list: or, ci_low, ci_high, p_value, chi2
.obt_OR <- function(a, b, c, d, level = 0.95) {
  # Haldane-Anscombe correction for zero cells
  if (any(c(a, b, c, d) == 0)) {
    a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5
    message("Zero cell: Haldane-Anscombe +0.5 correction applied.")
  }
  or  <- (a * d) / (b * c)
  se_log <- sqrt(1/a + 1/b + 1/c + 1/d)
  z   <- stats::qnorm(1 - (1 - level) / 2)
  log_or  <- log(or)
  ci_low  <- exp(log_or - z * se_log)
  ci_high <- exp(log_or + z * se_log)

  # Mantel-Haenszel chi-square for p-value
  n <- a + b + c + d
  chi2 <- (abs(a * d - b * c) - n / 2)^2 * n /
          ((a+b) * (c+d) * (a+c) * (b+d))
  p_val <- stats::pchisq(chi2, df = 1, lower.tail = FALSE)

  # Fisher exact
  mat  <- matrix(c(a, c, b, d), nrow = 2)
  ft   <- suppressWarnings(stats::fisher.test(mat))

  list(or=or, ci_low=ci_low, ci_high=ci_high, p_value=p_val,
       chi2=chi2, fisher_p=ft$p.value)
}


# -----------------------------------------------------------------------------
# MANTEL-HAENSZEL STRATIFIED ANALYSIS
# Reference: Mantel & Haenszel (1959); Rothman & Greenland (2008) pp. 258-264
# Breslow-Day test: Breslow & Day (1980) Vol. 1, pp. 142-146
# -----------------------------------------------------------------------------

#' Extract 2×2 cell counts from a stratum data frame
.obt_cells <- function(df, exp_col, exp_lvl, out_col, case_lvl) {
  exposed  <- df[[exp_col]] == exp_lvl & !is.na(df[[exp_col]])
  cases    <- df[[out_col]] == case_lvl & !is.na(df[[out_col]])
  a <- sum( exposed &  cases)
  b <- sum( exposed & !cases)
  c <- sum(!exposed &  cases)
  d <- sum(!exposed & !cases)
  list(a=a, b=b, c=c, d=d, n=a+b+c+d, n1=a+b, n0=c+d)
}


#' Mantel-Haenszel pooled Risk Ratio
#' Formula: RR_MH = sum(a_i * n0_i / n_i) / sum(c_i * n1_i / n_i)
#' Reference: Mantel & Haenszel (1959); Rothman & Greenland (2008) p.260
.obt_MH_RR <- function(cells_list) {
  num <- sapply(cells_list, function(x)
    if (x$n > 0) x$a * x$n0 / x$n else 0)
  den <- sapply(cells_list, function(x)
    if (x$n > 0) x$c * x$n1 / x$n else 0)
  rr_mh <- sum(num) / sum(den)

  # Greenland-Robins variance for log(RR_MH)
  # Reference: Greenland & Robins (1985) Am J Epidemiol 121:885-900
  P_i <- sapply(cells_list, function(x)
    if (x$n > 0) (x$a + x$c) * (x$a * x$n0 + x$c * x$n1) / x$n^2 else 0)
  Q_i <- sapply(cells_list, function(x)
    if (x$n > 0) (x$b + x$d) * (x$a * x$n0 + x$c * x$n1) / x$n^2 else 0)
  R_i <- num
  S_i <- den

  var_log_rr <- (sum(P_i) / (2 * sum(R_i)^2)) -
                ((sum(P_i) + sum(Q_i)) / (2 * sum(R_i) * sum(S_i))) +
                (sum(Q_i) / (2 * sum(S_i)^2))
  se_log_rr <- sqrt(var_log_rr)
  list(rr_mh=rr_mh, se_log=se_log_rr)
}


#' Mantel-Haenszel pooled Odds Ratio
#' Formula: OR_MH = sum(a_i*d_i/n_i) / sum(b_i*c_i/n_i)
#' Reference: Mantel & Haenszel (1959)
.obt_MH_OR <- function(cells_list) {
  num <- sapply(cells_list, function(x)
    if (x$n > 0) x$a * x$d / x$n else 0)
  den <- sapply(cells_list, function(x)
    if (x$n > 0) x$b * x$c / x$n else 0)
  or_mh <- sum(num) / sum(den)

  # Robins-Breslow-Greenland variance
  # Reference: Robins J et al. (1986) Am J Epidemiol 124:719-723
  P_i <- sapply(cells_list, function(x) {
    if (x$n == 0) return(0)
    (x$a + x$d) * x$a * x$d / x$n^2
  })
  Q_i <- sapply(cells_list, function(x) {
    if (x$n == 0) return(0)
    ((x$a + x$d) * x$b * x$c + (x$b + x$c) * x$a * x$d) / x$n^2
  })
  R_i <- sapply(cells_list, function(x) {
    if (x$n == 0) return(0)
    (x$b + x$c) * x$b * x$c / x$n^2
  })
  var_log_or <- sum(P_i) / (2 * sum(num)^2) +
                sum(Q_i) / (2 * sum(num) * sum(den)) +
                sum(R_i) / (2 * sum(den)^2)
  se_log_or <- sqrt(var_log_or)
  list(or_mh=or_mh, se_log=se_log_or)
}


#' Breslow-Day test for homogeneity of OR across strata
#' Reference: Breslow & Day (1980) IARC Sci Pub No.32, pp.142-146
.obt_breslowDay <- function(cells_list, or_mh) {
  chi_bd <- sapply(cells_list, function(x) {
    if (x$n < 4) return(NA_real_)
    # Expected a_i under common OR (Breslow-Day formula)
    n1 <- x$n1; n0 <- x$n0; n <- x$n
    mc <- x$a + x$c
    # Solve quadratic: A^2*(OR-1) + A*(n1+mc*(1-OR)+OR*(n+mc)) ...
    # Using iterative approach for expected value
    f <- function(A) {
      B <- n1 - A; C <- mc - A; D <- n0 - C
      if (B < 0 || C < 0 || D < 0) return(Inf)
      (A * D) / (B * C) - or_mh
    }
    A_exp <- tryCatch(
      stats::uniroot(f, lower = max(0, mc + n1 - n),
                     upper = min(n1, mc))$root,
      error = function(e) NA_real_
    )
    if (is.na(A_exp)) return(NA_real_)
    B_exp <- n1 - A_exp; C_exp <- mc - A_exp; D_exp <- n0 - C_exp
    var_A <- 1 / (1/A_exp + 1/B_exp + 1/C_exp + 1/D_exp)
    (x$a - A_exp)^2 / var_A
  })
  chi_bd <- chi_bd[!is.na(chi_bd)]
  total_chi <- sum(chi_bd)
  df <- length(chi_bd) - 1
  p_bd <- stats::pchisq(total_chi, df = df, lower.tail = FALSE)
  list(chi2=total_chi, df=df, p_value=p_bd)
}


# -----------------------------------------------------------------------------
# LOGISTIC REGRESSION HELPERS
# Reference: Hosmer & Lemeshow (2000). Applied Logistic Regression, 2nd ed.
# -----------------------------------------------------------------------------

#' Screen variables for inclusion in multivariable model
#' Uses "10% change-in-estimate" rule (Rothman & Greenland 2008 p.254)
#' and p < 0.25 threshold (Hosmer & Lemeshow 2000 p.95) for initial screening
.obt_screenVars <- function(data, outcome, candidates,
                             screen_p = 0.25, change_est = 0.10) {
  results <- list()
  for (v in candidates) {
    tryCatch({
      frm <- stats::as.formula(paste0(outcome, " ~ ", v))
      mod <- stats::glm(frm, data = data, family = stats::binomial)
      sm  <- summary(mod)$coefficients
      # Take the first non-intercept row
      if (nrow(sm) < 2) next
      p_v <- sm[2, 4]
      results[[v]] <- p_v
    }, error = function(e) NULL)
  }
  # Return variables with p < screen_p
  names(Filter(function(p) !is.na(p) && p < screen_p, results))
}


#' OR table from glm object with 95% CI
.obt_orTable <- function(model, level = 0.95) {
  coefs <- stats::coef(model)
  ci    <- suppressMessages(stats::confint(model, level = level))
  if (is.null(dim(ci))) ci <- matrix(ci, ncol = 2)
  z_p   <- summary(model)$coefficients[, 4]
  data.frame(
    term    = names(coefs),
    or      = exp(coefs),
    ci_low  = exp(ci[, 1]),
    ci_high = exp(ci[, 2]),
    p_value = z_p,
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# SAMPLE SIZE FORMULAS
# Reference: Kelsey (1996) pp.43-50; Schlesselman (1982) pp.144-150
# -----------------------------------------------------------------------------

#' Sample size for cohort study (two independent proportions)
#' Reference: Kelsey et al. (1996) Methods in Observational Epidemiology,
#'            2nd ed., Table 4-7.
.obt_ss_cohort <- function(p0, rr, alpha = 0.05, power = 0.80,
                            ratio = 1) {
  p1   <- p0 * rr
  p_bar <- (p0 + ratio * p1) / (1 + ratio)
  z_a  <- stats::qnorm(1 - alpha / 2)
  z_b  <- stats::qnorm(power)
  n0   <- ((z_a * sqrt((1 + 1/ratio) * p_bar * (1 - p_bar)) +
             z_b * sqrt(p0 * (1 - p0) + p1 * (1 - p1) / ratio))^2) /
          (p1 - p0)^2
  list(n_unexposed = ceiling(n0),
       n_exposed   = ceiling(n0 * ratio),
       n_total     = ceiling(n0 * (1 + ratio)),
       p1=p1, p0=p0)
}


#' Sample size for case-control study
#' Reference: Schlesselman (1982) Case-Control Studies, pp.144-150.
.obt_ss_casecontrol <- function(p0, or, alpha = 0.05, power = 0.80,
                                 ratio = 1) {
  p1   <- (or * p0) / (1 - p0 + or * p0)
  p_bar <- (p0 + ratio * p1) / (1 + ratio)
  z_a  <- stats::qnorm(1 - alpha / 2)
  z_b  <- stats::qnorm(power)
  n_cases <- ((z_a * sqrt((1 + 1/ratio) * p_bar * (1 - p_bar)) +
                z_b * sqrt(p1 * (1 - p1) + p0 * (1 - p0) / ratio))^2) /
             (p1 - p0)^2
  list(n_cases    = ceiling(n_cases),
       n_controls = ceiling(n_cases * ratio),
       n_total    = ceiling(n_cases * (1 + ratio)),
       p1=p1, p0=p0)
}


# -----------------------------------------------------------------------------
# COLOUR PALETTE — WHO/CDC compliant
# -----------------------------------------------------------------------------
.obt_palette <- function(n = 8) {
  cols <- c("#1F4E79","#C00000","#375623","#7030A0",
            "#C55A11","#2E75B6","#70AD47","#FF0000")
  if (n <= length(cols)) return(cols[seq_len(n)])
  grDevices::colorRampPalette(cols)(n)
}


# -----------------------------------------------------------------------------
# GGPLOT2 THEME — publication-quality FETP style
# -----------------------------------------------------------------------------
.obt_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face="bold", size=13, hjust=0),
      plot.subtitle = ggplot2::element_text(size=10, colour="grey40"),
      plot.caption  = ggplot2::element_text(size=8, colour="grey55", hjust=1),
      axis.title    = ggplot2::element_text(size=10, face="bold"),
      axis.text     = ggplot2::element_text(size=9),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(size=9, face="bold"),
      legend.text     = ggplot2::element_text(size=8),
      panel.grid.major.y = ggplot2::element_line(colour="grey92"),
      plot.margin   = ggplot2::margin(10, 15, 10, 10)
    )
}


# -----------------------------------------------------------------------------
# HTML HELPERS FOR RESULT BOXES
# -----------------------------------------------------------------------------
.obt_infoBox <- function(title, content, colour = "#1F4E79") {
  sprintf(
    '<div style="font-family:\'Segoe UI\',Arial,sans-serif;font-size:12.5px;
     line-height:1.85;border-left:4px solid %s;
     padding:8px 14px;margin:6px 0;background:#f8f9fb;">
     <b>%s</b><br>%s</div>',
    colour, title, content
  )
}

.obt_warnBox <- function(msg) {
  sprintf(
    '<div style="font-family:Arial,sans-serif;font-size:12px;color:#7B2C2C;
     background:#FFF3F3;border-left:4px solid #C00000;padding:8px 12px;
     margin:4px 0;">⚠️ %s</div>', msg
  )
}

.obt_okBox <- function(msg) {
  sprintf(
    '<div style="font-family:Arial,sans-serif;font-size:12px;color:#1E4620;
     background:#F0FFF2;border-left:4px solid #375623;padding:8px 12px;
     margin:4px 0;">✅ %s</div>', msg
  )
}


# =============================================================================
# OPENREFINE-STYLE DATA CLEANING HELPERS
# Added in v1.1.0 — Smart Data Cleaning Assistant
# =============================================================================

# Turkish-aware character normalization
# Replaces İ ı ş ç ğ ü ö (and their uppercase variants) with ASCII equivalents
# Uses chartr() which is platform-safe (works on Windows/Mac/Linux UTF-8 locales)
.obt_turkishFold <- function(x) {
  if (length(x) == 0) return(character(0))
  s <- as.character(x)
  # First: Turkish-locale lowercase to handle İ→i, I→ı correctly
  s <- stringi::stri_trans_tolower(s, locale = "tr_TR")
  # Then: fold Turkish-specific characters to ASCII
  s <- chartr("\u0131\u0130\u015F\u015E\u00E7\u00C7\u011F\u011E\u00FC\u00DC\u00F6\u00D6\u00E2\u00C2\u00EE\u00CE\u00FB\u00DB",
              "iissccgguuooaaiiuu", s)
  # Final safety: any remaining non-ASCII via stringi general transliterator
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s
}


#' OpenRefine "fingerprint" key for clustering string variants
#' Algorithm: lowercase + Turkish-fold + strip punctuation +
#'            split tokens + dedupe + sort + rejoin
#' Examples: "İstanbul", "istanbul ", "İSTANBUL", "Istanbul,"
#'           all map to fingerprint "istanbul"
.obt_fingerprint <- function(x) {
  if (length(x) == 0) return(character(0))
  s <- as.character(x)
  isna <- is.na(s) | !nzchar(trimws(s))
  s[isna] <- NA_character_

  # Normalize case + fold Turkish chars
  s_norm <- .obt_turkishFold(s)
  # Strip punctuation, normalize whitespace
  s_norm <- gsub("[[:punct:]]+", " ", s_norm)
  s_norm <- gsub("\\s+", " ", trimws(s_norm))

  out <- vapply(s_norm, function(v) {
    if (is.na(v) || !nzchar(v)) return(NA_character_)
    toks <- unique(strsplit(v, " ", fixed = TRUE)[[1]])
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) return(NA_character_)
    paste(sort(toks), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
  out
}


#' n-gram fingerprint — catches near-typos ("ankra" vs "ankara")
#' Builds sorted unique character n-grams as the cluster key.
.obt_ngramFingerprint <- function(x, n = 2) {
  if (length(x) == 0) return(character(0))
  s <- as.character(x)
  s <- .obt_turkishFold(s)
  s <- gsub("[[:punct:]]|\\s+", "", s)

  out <- vapply(s, function(v) {
    if (is.na(v) || nchar(v) < n) return(NA_character_)
    L <- nchar(v)
    grams <- unique(substring(v, seq_len(L - n + 1), seq_len(L - n + 1) + n - 1))
    paste(sort(grams), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  out
}


#' Cluster string variants in a vector
#' Returns a tidy data frame of clusters with size > 1 (only ambiguous groups).
#' Each row: one original value + its cluster ID + cluster size + occurrence count.
.obt_cluster <- function(x, method = c("fingerprint", "ngram"), ngram_n = 2) {
  method <- match.arg(method)

  vals <- as.character(x)
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (length(vals) == 0) {
    return(data.frame(cluster_id = integer(0),
                      original = character(0),
                      n_occurrences = integer(0),
                      cluster_size = integer(0),
                      stringsAsFactors = FALSE))
  }

  keys <- if (method == "fingerprint")
            .obt_fingerprint(vals)
          else
            .obt_ngramFingerprint(vals, n = ngram_n)

  # Count occurrences per unique original
  occ_tab <- table(vals)
  unique_vals <- names(occ_tab)
  unique_keys <- if (method == "fingerprint")
                   .obt_fingerprint(unique_vals)
                 else
                   .obt_ngramFingerprint(unique_vals, n = ngram_n)

  df <- data.frame(
    original      = unique_vals,
    key           = unique_keys,
    n_occurrences = as.integer(occ_tab),
    stringsAsFactors = FALSE
  )
  # Drop rows with missing keys
  df <- df[!is.na(df$key) & nzchar(df$key), ]
  if (nrow(df) == 0)
    return(data.frame(cluster_id = integer(0), original = character(0),
                      n_occurrences = integer(0), cluster_size = integer(0),
                      stringsAsFactors = FALSE))

  # Cluster size = number of distinct originals sharing the same key
  sz <- as.integer(table(df$key)[df$key])
  df$cluster_size <- sz

  # Only keep clusters where >1 distinct variant exists
  df <- df[df$cluster_size > 1, , drop = FALSE]
  if (nrow(df) == 0)
    return(data.frame(cluster_id = integer(0), original = character(0),
                      n_occurrences = integer(0), cluster_size = integer(0),
                      stringsAsFactors = FALSE))

  # Assign sequential cluster IDs (larger clusters first, then by total count)
  cluster_summary <- aggregate(n_occurrences ~ key, data = df, FUN = sum)
  cluster_summary <- cluster_summary[order(-cluster_summary$n_occurrences), ]
  cluster_summary$cluster_id <- seq_len(nrow(cluster_summary))
  df <- merge(df, cluster_summary[, c("key", "cluster_id")], by = "key")
  df <- df[order(df$cluster_id, -df$n_occurrences), ]
  rownames(df) <- NULL

  df[, c("cluster_id", "original", "n_occurrences", "cluster_size")]
}


#' Detect mixed date formats in a character/factor column
#' Returns a frequency table of recognised format patterns + 1-2 examples.
.obt_detectDateFormats <- function(x) {
  empty <- data.frame(pattern = character(0), n = integer(0),
                       examples = character(0), stringsAsFactors = FALSE)
  if (length(x) == 0) return(empty)

  s <- as.character(x)
  s <- trimws(s)
  s <- s[!is.na(s) & nzchar(s) &
         !s %in% c("NA", "N/A", ".", "99/99/99", "00/00/0000")]
  if (length(s) == 0) return(empty)

  classify <- function(v) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", v))                       "YYYY-MM-DD (ISO)"
    else if (grepl("^\\d{4}/\\d{2}/\\d{2}$", v))                  "YYYY/MM/DD"
    else if (grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", v))              "DD.MM.YYYY (Turkish)"
    else if (grepl("^\\d{2}/\\d{2}/\\d{4}$", v))                  "DD/MM/YYYY or MM/DD/YYYY"
    else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", v))                  "DD-MM-YYYY"
    else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", v))              "D/M/YY (2-digit year)"
    else if (grepl("^\\d{8}$", v))                                "YYYYMMDD (no separator)"
    else if (grepl("^\\d{1,2}\\s+[A-Za-z\u00C0-\u017F]+\\s+\\d{2,4}$", v))
                                                                  "D Month YYYY (text month)"
    else if (grepl("^\\d+(\\.\\d+)?$", v))                        "Numeric (Excel serial?)"
    else                                                          "Other / unrecognized"
  }

  patterns <- vapply(s, classify, character(1), USE.NAMES = FALSE)
  tab <- sort(table(patterns), decreasing = TRUE)
  data.frame(
    pattern  = names(tab),
    n        = as.integer(tab),
    examples = vapply(names(tab), function(p) {
      ex <- head(unique(s[patterns == p]), 2)
      paste(ex, collapse = " | ")
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# R RECIPE GENERATORS — produce copy-paste R code for common cleaning tasks
# -----------------------------------------------------------------------------

#' Build an HTML code block for the cleaning report
.obt_codeBlock <- function(title, code) {
  # HTML-encode any < > & in the code itself
  code_safe <- gsub("&", "&amp;", code, fixed = TRUE)
  code_safe <- gsub("<", "&lt;", code_safe, fixed = TRUE)
  code_safe <- gsub(">", "&gt;", code_safe, fixed = TRUE)
  sprintf(
    '<div style="margin:10px 0;">
       <div style="background:#1F4E79;color:white;padding:6px 10px;font-weight:bold;
                   font-size:12px;border-radius:4px 4px 0 0;">%s</div>
       <pre style="background:#F4F4F4;color:#1A1A1A;padding:10px 12px;margin:0;
                   border:1px solid #D0D0D0;border-radius:0 0 4px 4px;
                   font-family:Consolas,Monaco,monospace;font-size:11.5px;
                   line-height:1.5;white-space:pre-wrap;overflow-x:auto;">%s</pre>
     </div>',
    title, code_safe
  )
}

#' Recipe: normalize a text column (trim + Turkish-aware title case)
.obt_recipeNormalize <- function(varname) {
  code <- sprintf(
'# Normalize "%s": trim whitespace + Turkish-aware title case
data$%s_clean <- stringr::str_squish(as.character(data$%s))
data$%s_clean <- stringi::stri_trans_totitle(
  data$%s_clean, locale = "tr_TR"
)',
    varname, varname, varname, varname, varname)
  .obt_codeBlock(sprintf("Recipe: normalize text — '%s'", varname), code)
}

#' Recipe: merge a cluster of variants into one canonical value
.obt_recipeMergeCluster <- function(varname, variants, canonical) {
  vals_str <- paste0('"', variants, '"', collapse = ", ")
  code <- sprintf(
'# Merge "%s" variants into a single canonical value
data$%s[data$%s %%in%% c(%s)] <- "%s"',
    varname, varname, varname, vals_str, canonical)
  .obt_codeBlock(sprintf("Recipe: merge variants in '%s'", varname), code)
}

#' Recipe: parse a column as Date with multiple format fallbacks
.obt_recipeParseDate <- function(varname) {
  code <- sprintf(
'# Parse "%s" as Date — tries multiple formats
data$%s_date <- lubridate::parse_date_time(
  data$%s,
  orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY")
)
# Inspect rows that failed to parse:
# View(data[is.na(data$%s_date) & !is.na(data$%s), c("%s", "%s_date")])',
    varname, varname, varname, varname, varname, varname, varname)
  .obt_codeBlock(sprintf("Recipe: parse '%s' as date", varname), code)
}

#' Recipe: flag implausible numeric values outside an expected range
.obt_recipeFlagOutliers <- function(varname, min_val, max_val) {
  code <- sprintf(
'# Flag implausible values in "%s" (outside [%g, %g])
data$%s_flag <- ifelse(
  is.na(data$%s) | (data$%s >= %g & data$%s <= %g),
  "OK", "OUT_OF_RANGE"
)
# Inspect flagged rows:
# View(data[data$%s_flag == "OUT_OF_RANGE", ])',
    varname, min_val, max_val,
    varname, varname, varname, min_val, varname, max_val, varname)
  .obt_codeBlock(sprintf("Recipe: flag outliers in '%s'", varname), code)
}

#' Recipe: replace common missing-value codes with proper NA
.obt_recipeRecodeNA <- function(varname) {
  code <- sprintf(
'# Recode common missing-value placeholders to NA in "%s"
na_codes <- c("", "NA", "N/A", ".", "99", "999", "Unknown", "unknown", "-")
data$%s[data$%s %%in%% na_codes] <- NA',
    varname, varname, varname)
  .obt_codeBlock(sprintf("Recipe: recode missing values — '%s'", varname), code)
}
