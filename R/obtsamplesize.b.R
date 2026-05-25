obtsamplesizeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "obtsamplesizeClass",
    inherit = obtsamplesizeBase,
    private = list(
      .powerData = NULL,

      .run = function() {
        opts   <- self$options
        p0     <- opts$p0 / 100
        effect <- opts$effect
        alpha  <- as.numeric(opts$alpha)
        power  <- as.numeric(opts$power)
        ratio  <- opts$ratio
        design <- opts$studyType

        if (design %in% c("cohort", "crosssectional")) {
          res <- .obt_ss_cohort(p0=p0, rr=effect, alpha=alpha,
                                 power=power, ratio=ratio)
          effect_label <- "Expected RR"
          n1_label     <- "N Exposed"
          n0_label     <- "N Unexposed"
        } else {
          res <- .obt_ss_casecontrol(p0=p0, or=effect, alpha=alpha,
                                      power=power, ratio=ratio)
          effect_label <- "Expected OR"
          n1_label     <- "N Cases"
          n0_label     <- "N Controls"
        }

        st <- self$results$ssTable; st$deleteRows()
        rows <- list(
          c("Study design",                    stringr::str_to_title(design)),
          c("Background risk/prevalence (p0)", sprintf("%.1f%%", p0*100)),
          c("Expected risk in exposed (p1)",   sprintf("%.1f%%", res$p1*100)),
          c(effect_label,                      sprintf("%.2f", effect)),
          c("Type I error (alpha, two-sided)", sprintf("%.2f", alpha)),
          c("Statistical power (1-beta)",      sprintf("%.0f%%", power*100)),
          c("Control:Case ratio",              sprintf("%.0f:1", ratio)),
          c(n1_label,                          as.character(res[[1]])),
          c(n0_label,                          as.character(res[[2]])),
          c("TOTAL sample size required",      as.character(res$n_total))
        )
        for (r in rows)
          st$addRow(rowKey=r[[1]], values=list(parameter=r[[1]], value=r[[2]]))

        effects <- seq(1.1, max(5, effect*1.5), by=0.2)
        pt <- self$results$powerTable; pt$deleteRows()
        power_data <- list()
        for (eff in effects) {
          if (design %in% c("cohort","crosssectional")) {
            r80 <- .obt_ss_cohort(p0, eff, alpha, 0.80, ratio)$n_total
            r90 <- .obt_ss_cohort(p0, eff, alpha, 0.90, ratio)$n_total
          } else {
            r80 <- .obt_ss_casecontrol(p0, eff, alpha, 0.80, ratio)$n_total
            r90 <- .obt_ss_casecontrol(p0, eff, alpha, 0.90, ratio)$n_total
          }
          pt$addRow(rowKey=as.character(eff),
                    values=list(effect=eff, n80=as.integer(r80), n90=as.integer(r90)))
          power_data[[length(power_data)+1]] <- list(eff=eff, n80=r80, n90=r90)
        }
        private$.powerData <- power_data

        note <- sprintf(
          '<div style="font-family:Arial,sans-serif;font-size:12px;line-height:1.85;">
           <b>Result:</b> %d total participants required (%s=%d, %s=%d).<br>
           <b>Assumptions:</b> p0=%.1f%%, p1=%.1f%%, %s=%.2f, alpha=%.2f, power=%.0f%%.<br>
           <b>Method:</b> %s<br>
           <b>Developed by:</b> Gulser Dogan Turkccelik &amp; Muammer Beslen — Turkiye FETP
           | outbreakTools v1.0.0</div>',
          res$n_total, n1_label, res[[1]], n0_label, res[[2]],
          p0*100, res$p1*100, effect_label, effect, alpha, power*100,
          if (design %in% c("cohort","crosssectional"))
            "Two-proportion z-test (Kelsey et al. 1996)"
          else "Schlesselman (1982) case-control formula"
        )
        self$results$ssNote$setContent(note)
      },

      .plotPower = function(image, ggtheme, theme, ...) {
        if (is.null(private$.powerData)) return(FALSE)
        df <- do.call(rbind, lapply(private$.powerData, function(r)
          data.frame(effect=r$eff, n80=r$n80, n90=r$n90)))
        df_long <- tidyr::pivot_longer(df, cols=c(n80, n90),
                                        names_to="power_level", values_to="n")
        df_long$power_level <- ifelse(df_long$power_level=="n80","80% power","90% power")
        p <- ggplot2::ggplot(df_long, ggplot2::aes(x=effect, y=n,
                             colour=power_level, linetype=power_level)) +
          ggplot2::geom_line(linewidth=1.1) +
          ggplot2::geom_point(size=2) +
          ggplot2::scale_colour_manual(values=c("80% power"="#1F4E79","90% power"="#C00000")) +
          ggplot2::scale_y_continuous(labels=scales::comma) +
          ggplot2::labs(x="Effect Size (RR or OR)", y="Total Sample Size Required",
                        title="Power Curve",
                        colour="Power", linetype="Power",
                        caption="outbreakTools v1.0.0 | Turkiye FETP") +
          .obt_theme()
        print(p); TRUE
      }
    )
)
