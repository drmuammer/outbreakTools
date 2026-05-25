obtcasecontrolClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class("obtcasecontrolClass",
    inherit = obtcasecontrolBase,
    private = list(
      .estData = NULL,

      .run = function() {
        if (is.null(self$options$exposureVar) || is.null(self$options$outcomeVar)) {
          self$results$ccNote$setContent(
            .obt_infoBox("Instructions",
              "Assign <b>Exposure</b> and <b>Case/Control</b> variables.", "#1F4E79"))
          return(invisible(NULL))
        }
        data   <- self$data; opts <- self$options
        exp_v  <- as.character(data[[opts$exposureVar]])
        out_v  <- as.character(data[[opts$outcomeVar]])
        valid  <- !is.na(exp_v) & !is.na(out_v)
        exp_v  <- exp_v[valid]; out_v <- out_v[valid]
        exposed <- exp_v == opts$exposedLevel
        cases   <- out_v == opts$caseLevel

        a <- sum( exposed &  cases); b <- sum( exposed & !cases)
        c <- sum(!exposed &  cases); d <- sum(!exposed & !cases)
        n_cases <- a+c; n_ctrl <- b+d; n <- n_cases+n_ctrl

        ct <- self$results$crossTab; ct$deleteRows()
        ct$addRow(rowKey="exp",  values=list(exposure="Exposed",
                  cases=a, controls=b, total=a+b,
                  exp_pct=if((a+b)>0) round(100*a/(a+b),1) else NA_real_))
        ct$addRow(rowKey="unexp",values=list(exposure="Unexposed",
                  cases=c, controls=d, total=c+d,
                  exp_pct=if((c+d)>0) round(100*c/(c+d),1) else NA_real_))
        ct$addRow(rowKey="tot",  values=list(exposure="Total",
                  cases=n_cases, controls=n_ctrl, total=n,
                  exp_pct=if(n>0) round(100*(a+b)/n,1) else NA_real_))

        res <- .obt_OR(a, b, c, d, level=opts$ciLevel)
        mt  <- self$results$measuresTable; mt$deleteRows()
        mt$addRow(rowKey="OR", values=list(measure="Odds Ratio (OR)",
                  estimate=res$or, ci_low=res$ci_low, ci_high=res$ci_high,
                  p_value=res$p_value))

        cht <- self$results$chiTable; cht$deleteRows()
        mat <- matrix(c(a,c,b,d), nrow=2)
        cs  <- suppressWarnings(stats::chisq.test(mat, correct=FALSE))
        cht$addRow(rowKey="mh", values=list(test="Mantel-Haenszel chi-square",
                   stat=res$chi2, df=1L, p=res$p_value))
        cht$addRow(rowKey="pearson", values=list(test="Pearson chi-square",
                   stat=cs$statistic[[1]], df=as.integer(cs$parameter[[1]]), p=cs$p.value))
        cht$addRow(rowKey="fisher", values=list(test="Fisher's exact test",
                   stat=NA_real_, df=NA_integer_, p=res$fisher_p))

        private$.estData <- list(or=res$or, ci_low=res$ci_low, ci_high=res$ci_high)
        sig_txt <- if (!is.na(res$p_value) && res$p_value < 0.05)
          "statistically significant" else "not statistically significant"
        note <- sprintf(
          '<div style="font-family:Arial,sans-serif;font-size:12px;line-height:1.85;">
           <b>2x2 table:</b> a=%d, b=%d, c=%d, d=%d (N=%d)<br>
           <b>OR:</b> %.2f (%.0f%% CI: %.2f-%.2f) - %s (p=%.3f)<br>
           <b>Method:</b> Woolf log CI. Haldane-Anscombe +0.5 if any cell=0.<br>
           <b>Developed by:</b> Gulser Dogan Turkcelik &amp; Muammer Beslen - Turkiye FETP</div>',
          a, b, c, d, n, res$or, opts$ciLevel*100, res$ci_low, res$ci_high,
          sig_txt, res$p_value)
        self$results$ccNote$setContent(note)
      },

      .plotForest = function(image, ggtheme, theme, ...) {
        if (is.null(private$.estData)) return(FALSE)
        est <- private$.estData
        df  <- data.frame(label="Odds Ratio", est=est$or, lo=est$ci_low, hi=est$ci_high)
        p <- ggplot2::ggplot(df, ggplot2::aes(y=label, x=est, xmin=lo, xmax=hi)) +
          ggplot2::geom_vline(xintercept=1, linetype="dashed", colour="grey50") +
          ggplot2::geom_errorbarh(height=0.12, colour="#1F4E79", linewidth=0.9) +
          ggplot2::geom_point(size=4, colour="#C00000") +
          ggplot2::scale_x_log10() +
          ggplot2::labs(x="Odds Ratio (log scale)", y=NULL,
                        title="Odds Ratio with 95% CI",
                        caption="outbreakTools v1.1.0 | Turkiye FETP") +
          .obt_theme()
        print(p); TRUE
      }
    )
  )
