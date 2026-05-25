# =============================================================================
# outbreakTools — Descriptive Epidemiology
# File: R/obt_descriptive.b.R
# =============================================================================

obtdescriptiveClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class("obtdescriptiveClass",
    inherit = obtdescriptiveBase,
    private = list(.run = function() {
      data     <- self$data
      opts     <- self$options
      caseCol  <- opts$caseVar
      caseLvl  <- opts$caseLevel
      method   <- opts$ciMethod
      lev      <- opts$ciLevel
      n_total  <- nrow(data)

      # Identify cases
      if (!is.null(caseCol)) {
        cv       <- as.character(data[[caseCol]])
        is_case  <- cv == caseLvl & !is.na(cv)
      } else {
        is_case  <- rep(TRUE, n_total)
      }
      n_cases <- sum(is_case)

      # Age summary
      if (!is.null(opts$ageVar)) {
        age_v <- suppressWarnings(as.numeric(data[[opts$ageVar]][is_case]))
        age_v <- age_v[!is.na(age_v)]
        stats_list <- list(
          c("N with age",        as.character(length(age_v))),
          c("Median [IQR]",      sprintf("%.0f [%.0f–%.0f]",
                                          stats::median(age_v),
                                          stats::quantile(age_v, 0.25),
                                          stats::quantile(age_v, 0.75))),
          c("Mean (SD)",         sprintf("%.1f (%.1f)", mean(age_v), stats::sd(age_v))),
          c("Range",             sprintf("%.0f – %.0f", min(age_v), max(age_v))),
          c("Missing age (cases)", as.character(sum(is.na(
            suppressWarnings(as.numeric(data[[opts$ageVar]][is_case]))))))
        )
        at <- self$results$ageTable; at$deleteRows()
        for (r in stats_list)
          at$addRow(rowKey=r[[1]], values=list(stat=r[[1]], value=r[[2]]))
      }

      # Frequency tables
      pvars <- opts$personVars
      if (length(pvars) > 0) {
        fa <- self$results$freqTables
        for (vname in pvars) {
          tbl <- fa$get(key=vname); tbl$deleteRows()
          col_all  <- as.character(data[[vname]])
          col_case <- col_all[is_case]
          lvls <- sort(unique(col_all[!is.na(col_all)]))
          if (opts$showMissing) lvls <- c(lvls, NA)
          for (lv in lvls) {
            if (is.na(lv)) {
              n_c <- sum(is.na(col_case))
              n_t <- sum(is.na(col_all))
              lab <- "(Missing)"
            } else {
              n_c <- sum(col_case == lv, na.rm=TRUE)
              n_t <- sum(col_all  == lv, na.rm=TRUE)
              lab <- lv
            }
            pct_v <- if (n_cases > 0) n_c / n_cases else NA_real_
            ar_v  <- if (opts$showAR && n_t > 0) n_c / n_t else NA_real_
            ci    <- if (opts$showAR && n_t > 0)
              .obt_propCI(n_c, n_t, method, lev) else c(lower=NA_real_, upper=NA_real_)
            tbl$addRow(rowKey=lab, values=list(
              level=lab, n=as.integer(n_c), total=as.integer(n_t),
              pct=pct_v, ar=ar_v, ci_low=ci[["lower"]], ci_high=ci[["upper"]]))
          }
          # Total row
          tbl$addRow(rowKey="TOTAL", values=list(
            level="TOTAL", n=as.integer(n_cases), total=as.integer(n_total),
            pct=1, ar=n_cases/n_total, ci_low=NA_real_, ci_high=NA_real_))
        }
      }

      note_html <- sprintf(
        '<div style="font-family:Arial,sans-serif;font-size:11.5px;color:#444;
         line-height:1.8;border-top:1px solid #ddd;padding-top:8px;margin-top:8px;">
         <b>Statistical methods:</b> Attack rates are row proportions (cases / stratum total).
         Confidence intervals calculated by the <b>%s method</b> (level: %.0f%%).
         Wilson score interval recommended for proportions near 0 or 1 and small samples
         (Wilson 1927; Brown et al. 2001).<br>
         <b>Reference:</b> Rothman KJ, Greenland S, Lash TL (2008). Modern Epidemiology, 3rd ed.
         Lippincott Williams &amp; Wilkins.<br>
         <b>Developed by:</b> Gülser Doğan Türkçelik &amp; Muammer Beslen — Türkiye FETP
         | outbreakTools v1.0.0</div>',
        stringr::str_to_title(method), lev*100
      )
      self$results$descNote$setContent(note_html)
    })
  )


