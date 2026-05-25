obtcohortClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class("obtcohortClass",
    inherit = obtcohortBase,
    private = list(
      .estData = NULL,

      .run = function() {
        if (is.null(self$options$exposureVar) || is.null(self$options$outcomeVar)) {
          self$results$cohortNote$setContent(
            .obt_infoBox("Instructions",
              "Assign <b>Exposure</b> and <b>Outcome</b> variables to run the cohort analysis.",
              "#1F4E79"))
          return(invisible(NULL))
        }
        data   <- self$data; opts <- self$options
        exp_v  <- as.character(data[[opts$exposureVar]])
        out_v  <- as.character(data[[opts$outcomeVar]])
        valid  <- !is.na(exp_v) & !is.na(out_v)
        exp_v  <- exp_v[valid]; out_v <- out_v[valid]
        exposed <- exp_v == opts$exposedLevel
        cases   <- out_v == opts$caseLevel

        a <- sum( exposed &  cases)
        b <- sum( exposed & !cases)
        c <- sum(!exposed &  cases)
        d <- sum(!exposed & !cases)
        n1 <- a+b; n0 <- c+d; n <- n1+n0

        ct <- self$results$crossTab; ct$deleteRows()
        ct$addRow(rowKey="exp",   values=list(exposure=paste0("Exposed (n=",n1,")"),
                  cases=a, nonCases=b, total=n1,
                  ar_pct=if(n1>0) round(100*a/n1,1) else NA_real_))
        ct$addRow(rowKey="unexp", values=list(exposure=paste0("Unexposed (n=",n0,")"),
                  cases=c, nonCases=d, total=n0,
                  ar_pct=if(n0>0) round(100*c/n0,1) else NA_real_))
        ct$addRow(rowKey="tot",   values=list(exposure=paste0("Total (n=",n,")"),
                  cases=a+c, nonCases=b+d, total=n,
                  ar_pct=if(n>0) round(100*(a+c)/n,1) else NA_real_))

        res <- .obt_RR(a, b, c, d, level=opts$ciLevel)
        mt  <- self$results$measuresTable; mt$deleteRows()
        mt$addRow(rowKey="RR", values=list(measure="Risk Ratio (RR)",
                  estimate=res$rr, ci_low=res$ci_low, ci_high=res$ci_high,
                  p_value=res$p_value))
        if (opts$showAR) {
          mt$addRow(rowKey="AR", values=list(measure="Attributable Risk (AR)",
                    estimate=res$ar, ci_low=res$ar_ci_low, ci_high=res$ar_ci_high,
                    p_value=NA_real_))
        }
        if (opts$showPAR && !is.na(res$par)) {
          mt$addRow(rowKey="PAR", values=list(measure="Population AR (PAR)",
                    estimate=res$par, ci_low=NA_real_, ci_high=NA_real_, p_value=NA_real_))
        }

        cht <- self$results$chiTable; cht$deleteRows()
        mat <- matrix(c(a,c,b,d), nrow=2)
        cs  <- suppressWarnings(stats::chisq.test(mat, correct=FALSE))
        csy <- suppressWarnings(stats::chisq.test(mat, correct=TRUE))
        cht$addRow(rowKey="pearson", values=list(test="Pearson Chi-square",
                   stat=cs$statistic[[1]], df=as.integer(cs$parameter[[1]]), p=cs$p.value))
        cht$addRow(rowKey="yates",   values=list(test="Yates correction",
                   stat=csy$statistic[[1]], df=as.integer(csy$parameter[[1]]), p=csy$p.value))
        if (any(c(a,b,c,d) < 5)) {
          ft <- stats::fisher.test(mat)
          cht$addRow(rowKey="fisher", values=list(test="Fisher exact (cell <5)",
                     stat=ft$estimate[[1]], df=NA_integer_, p=ft$p.value))
        }

        private$.estData <- list(rr=res$rr, ci_low=res$ci_low,
                                  ci_high=res$ci_high, label="Risk Ratio")
        sig_txt <- if (!is.na(res$p_value) && res$p_value < 0.05)
          "statistically significant (p < 0.05)" else "not statistically significant (p >= 0.05)"
        note <- sprintf(
          '<div style="font-family:Arial,sans-serif;font-size:12px;line-height:1.85;">
           <b>2x2 table:</b> a=%d, b=%d, c=%d, d=%d (N=%d)<br>
           <b>RR:</b> %.2f (%.0f%% CI: %.2f-%.2f) - %s<br>
           <b>Method:</b> Log-method Wald CI for RR (Rothman &amp; Greenland 2008).<br>
           <b>Developed by:</b> Gulser Dogan Turkcelik &amp; Muammer Beslen - Turkiye FETP</div>',
          a, b, c, d, n, res$rr, opts$ciLevel*100, res$ci_low, res$ci_high, sig_txt)
        self$results$cohortNote$setContent(note)
      },

      .plotForest = function(image, ggtheme, theme, ...) {
        if (is.null(private$.estData)) return(FALSE)
        est <- private$.estData
        df  <- data.frame(label=est$label, est=est$rr, lo=est$ci_low, hi=est$ci_high)
        p <- ggplot2::ggplot(df, ggplot2::aes(y=label, x=est, xmin=lo, xmax=hi)) +
          ggplot2::geom_vline(xintercept=1, linetype="dashed", colour="grey50") +
          ggplot2::geom_errorbarh(height=0.15, colour="#1F4E79", linewidth=0.9) +
          ggplot2::geom_point(size=4, colour="#C00000") +
          ggplot2::scale_x_log10() +
          ggplot2::labs(x="Risk Ratio (log scale)", y=NULL,
                        title="Risk Ratio with 95% CI",
                        caption="outbreakTools v1.1.0 | Turkiye FETP") +
          .obt_theme()
        print(p); TRUE
      }
    )
  )
