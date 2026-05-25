# =============================================================================
# outbreakTools - Stratified (MH), Logistic Regression, Sample Size
# File: R/obt_stratified.b.R
# Authors: Gulser Dogan Turkcelik & Muammer Beslen - Turkiye FETP
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# STRATIFIED ANALYSIS - MANTEL-HAENSZEL
# Reference: Mantel & Haenszel (1959); Rothman & Greenland (2008) pp.258-264
#            Breslow & Day (1980) IARC Vol.1 pp.142-146
# ─────────────────────────────────────────────────────────────────────────────

obtstratifiedClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class("obtstratifiedClass",
    inherit = obtstratifiedBase,
    private = list(.run = function() {
      opts <- self$options
      if (is.null(opts$exposureVar) || is.null(opts$outcomeVar) ||
          is.null(opts$stratVar)) {
        self$results$mhNote$setContent(
          .obt_infoBox("Instructions",
            "Assign <b>Exposure</b>, <b>Outcome</b>, and <b>Stratification</b> variables.",
            "#1F4E79"))
        return(invisible(NULL))
      }
      data    <- self$data
      exp_col <- opts$exposureVar; exp_lvl <- opts$exposedLevel
      out_col <- opts$outcomeVar;  cas_lvl <- opts$caseLevel
      str_col <- opts$stratVar
      lev     <- opts$ciLevel
      design  <- opts$studyDesign

      # Remove rows with any missing in key variables
      valid <- !is.na(data[[exp_col]]) & !is.na(data[[out_col]]) &
               !is.na(data[[str_col]])
      df    <- data[valid, , drop=FALSE]
      df[[exp_col]] <- as.character(df[[exp_col]])
      df[[out_col]] <- as.character(df[[out_col]])
      df[[str_col]] <- as.character(df[[str_col]])

      # CRUDE estimate
      cells_crude <- .obt_cells(df, exp_col, exp_lvl, out_col, cas_lvl)
      a_c <- cells_crude$a; b_c <- cells_crude$b
      c_c <- cells_crude$c; d_c <- cells_crude$d

      if (design == "cohort") {
        res_crude <- .obt_RR(a_c, b_c, c_c, d_c, level=lev)
        cr_est <- res_crude$rr; cr_lo <- res_crude$ci_low
        cr_hi  <- res_crude$ci_high; cr_p  <- res_crude$p_value
        label  <- "Risk Ratio (RR)"
      } else {
        res_crude <- .obt_OR(a_c, b_c, c_c, d_c, level=lev)
        cr_est <- res_crude$or; cr_lo <- res_crude$ci_low
        cr_hi  <- res_crude$ci_high; cr_p  <- res_crude$p_value
        label  <- "Odds Ratio (OR)"
      }

      ct <- self$results$crudeTable; ct$deleteRows()
      ct$addRow(rowKey="crude", values=list(measure=paste("Crude", label),
                estimate=cr_est, ci_low=cr_lo, ci_high=cr_hi, p_value=cr_p))

      # STRATUM-SPECIFIC
      strata    <- sort(unique(df[[str_col]]))
      cells_lst <- lapply(strata, function(s) {
        ds <- df[df[[str_col]] == s, , drop=FALSE]
        .obt_cells(ds, exp_col, exp_lvl, out_col, cas_lvl)
      })
      names(cells_lst) <- strata

      st <- self$results$strataTable; st$deleteRows()
      z  <- stats::qnorm(1 - (1 - lev) / 2)
      for (s in strata) {
        x <- cells_lst[[s]]
        if (design == "cohort") {
          rs <- .obt_RR(x$a, x$b, x$c, x$d, level=lev)
          est <- rs$rr; lo <- rs$ci_low; hi <- rs$ci_high
        } else {
          rs <- .obt_OR(x$a, x$b, x$c, x$d, level=lev)
          est <- rs$or; lo <- rs$ci_low; hi <- rs$ci_high
        }
        st$addRow(rowKey=s, values=list(stratum=s,
                  a=x$a, b=x$b, c=x$c, d=x$d,
                  estimate=est, ci_low=lo, ci_high=hi))
      }

      # MH ADJUSTED
      if (design == "cohort") {
        mh_res <- .obt_MH_RR(cells_lst)
        mh_est <- mh_res$rr_mh
      } else {
        mh_res <- .obt_MH_OR(cells_lst)
        mh_est <- mh_res$or_mh
      }
      mh_lo <- exp(log(mh_est) - z * mh_res$se_log)
      mh_hi <- exp(log(mh_est) + z * mh_res$se_log)

      # MH chi-square for p-value (summary MH test)
      mc_num <- sapply(cells_lst, function(x)
        if(x$n>0) x$a - x$n1*(x$a+x$c)/x$n else 0)
      mc_den <- sapply(cells_lst, function(x)
        if(x$n>1) x$n1*x$n0*(x$a+x$c)*(x$b+x$d)/(x$n^2*(x$n-1)) else 0)
      mh_chi <- (abs(sum(mc_num)) - 0.5)^2 / sum(mc_den)
      mh_p   <- stats::pchisq(mh_chi, df=1, lower.tail=FALSE)

      mht <- self$results$mhTable; mht$deleteRows()
      mht$addRow(rowKey="mh", values=list(measure=paste("MH Adjusted", label),
                 crude=cr_est, adjusted=mh_est,
                 ci_low=mh_lo, ci_high=mh_hi, p_value=mh_p))

      # BRESLOW-DAY TEST
      bd  <- .obt_breslowDay(cells_lst, mh_est)
      bdt <- self$results$bdTable; bdt$deleteRows()
      interp <- if (is.na(bd$p_value)) "Insufficient data" else
                if (bd$p_value < 0.05)
                  "[!] Significant - effect modification likely (report stratum-specific estimates)"
                else "[OK] Non-significant - no evidence of effect modification (use MH adjusted estimate)"
      bdt$addRow(rowKey="bd", values=list(test="Breslow-Day test (homogeneity)",
                 stat=bd$chi2, df=as.integer(bd$df),
                 p=bd$p_value, interp=interp))

      note <- sprintf(
        '<div style="font-family:Arial,sans-serif;font-size:12px;line-height:1.85;">
         <b>Crude %s:</b> %.2f (%.0f%% CI: %.2f-%.2f)<br>
         <b>MH Adjusted %s:</b> %.2f (%.0f%% CI: %.2f-%.2f, p=%.3f)<br>
         <b>Breslow-Day test:</b> chi-squared=%.2f, df=%d, p=%.3f - %s<br>
         <b>Strata:</b> %s (%d strata)<br>
         <b>Methods:</b> MH formula - Mantel &amp; Haenszel (1959);
         Variance - Greenland &amp; Robins (1985);
         Breslow-Day - Breslow &amp; Day (1980) IARC Vol.1.<br>
         <b>Developed by:</b> Gulser Dogan Turkcelik &amp; Muammer Beslen - Turkiye FETP
         | outbreakTools v1.1.0</div>',
        label, cr_est, lev*100, cr_lo, cr_hi,
        label, mh_est, lev*100, mh_lo, mh_hi, mh_p,
        bd$chi2, bd$df, bd$p_value,
        if(bd$p_value < 0.05 && !is.na(bd$p_value))
          "effect modification present" else "no effect modification",
        paste(strata, collapse=", "), length(strata)
      )
      self$results$mhNote$setContent(note)
    })
  )

