# =============================================================================
# outbreakTools — Epidemic Curve
# File: R/obt_epicurve.b.R
# Authors: Gülser Doğan Türkçelik & Muammer Beslen — Türkiye FETP
# =============================================================================

obtepicurveClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class("obtepicurveClass",
    inherit = obtepicurveBase,
    private = list(

      .plotData = NULL,

      .run = function() {
        if (is.null(self$options$dateVar)) {
          self$results$epicurveSummary$setContent(
            .obt_infoBox("Instructions",
              "Assign a <b>Date of Symptom Onset</b> variable to generate the epidemic curve.",
              "#1F4E79"))
          return(invisible(NULL))
        }

        data    <- self$data
        dcol    <- self$options$dateVar
        gcol    <- self$options$groupVar
        unit    <- self$options$timeUnit
        fmt     <- self$options$dateFormat

        dates   <- .obt_parseDate(data[[dcol]], format = fmt)
        n_total <- nrow(data)
        n_valid <- sum(!is.na(dates))
        n_miss  <- sum(is.na(dates))

        if (n_valid == 0) {
          self$results$epicurveSummary$setContent(
            .obt_warnBox("No dates could be parsed. Check the Input Date Format setting."))
          return(invisible(NULL))
        }

        periods <- .obt_floorDate(dates, unit = unit)

        df <- data.frame(period = periods, stringsAsFactors = FALSE)
        if (!is.null(gcol) && gcol != "") {
          df$group <- as.character(data[[gcol]])
          df$group[is.na(df$group)] <- "(Missing)"
        }
        df_valid <- df[!is.na(df$period), , drop = FALSE]

        # Case count table
        ct <- sort(unique(df_valid$period))
        counts <- sapply(ct, function(p) sum(df_valid$period == p))
        pct    <- counts / n_valid
        cumN   <- cumsum(counts)
        cumPct <- cumN / n_valid

        if (self$options$showTable) {
          tbl <- self$results$caseTable
          tbl$deleteRows()
          for (i in seq_along(ct)) {
            tbl$addRow(rowKey = i, values = list(
              period = .obt_periodLabel(ct[i], unit),
              epiWk  = paste0("EW ", lubridate::isoweek(ct[i]), "/",
                              lubridate::isoyear(ct[i])),
              n      = as.integer(counts[i]),
              pct    = pct[i],
              cumN   = as.integer(cumN[i]),
              cumPct = cumPct[i]
            ))
          }
        }

        peak_idx  <- which.max(counts)
        peak_date <- ct[peak_idx]
        peak_n    <- counts[peak_idx]

        html <- sprintf(
          '<div style="font-family:\'Segoe UI\',Arial,sans-serif;font-size:12.5px;
           line-height:1.9;">
           <b>Total cases:</b> %d &nbsp;|&nbsp; <b>Dates parsed:</b> %d
           &nbsp;|&nbsp; <b>Missing dates:</b> %d<br>
           <b>Outbreak period:</b> %s — %s &nbsp;|&nbsp;
           <b>Duration:</b> %d %s(s)<br>
           <b>Peak period:</b> %s (n = %d)<br>
           <span style="color:#555;font-size:11px;">Time unit: %s &nbsp;|&nbsp;
           EW = ISO 8601 epi-week (Monday start)</span></div>',
          n_total, n_valid, n_miss,
          .obt_periodLabel(min(ct), unit),
          .obt_periodLabel(max(ct), unit),
          length(ct), unit,
          .obt_periodLabel(peak_date, unit), peak_n,
          unit
        )
        self$results$epicurveSummary$setContent(html)

        private$.plotData <- list(df=df_valid, unit=unit, n=n_valid,
                                   gcol=gcol, peak_date=peak_date)
      },

      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(private$.plotData)) return(FALSE)
        pd   <- private$.plotData
        opts <- self$options

        fill_map <- c(navy="steelblue", red="#C00000", green="#375623",
                      steel="#2E75B6", orange="#C55A11")
        fill_col <- fill_map[[opts$fillColour]]

        span_days <- as.numeric(diff(range(pd$df$period, na.rm=TRUE)))
        # Smarter break intervals — prevent label crowding on wide spans
        brk_unit  <- if (span_days <= 14)    "1 day"    else
                     if (span_days <= 60)    "1 week"   else
                     if (span_days <= 180)   "2 weeks"  else
                     if (span_days <= 365)   "1 month"  else
                     if (span_days <= 730)   "3 months" else
                     if (span_days <= 1825)  "6 months" else
                     if (span_days <= 3650)  "1 year"   else
                                             "2 years"
        date_fmt  <- if (pd$unit == "month" || span_days > 365) "%b %Y" else "%d %b\n%Y"

        bar_w <- switch(pd$unit, day=0.85, week=6.4, week_sun=6.4, month=27, 7)

        if (!is.null(pd$gcol) && pd$gcol != "") {
          p <- ggplot2::ggplot(pd$df, ggplot2::aes(x=period, fill=group)) +
            ggplot2::geom_bar(width=bar_w, colour="white", linewidth=0.2) +
            ggplot2::scale_fill_manual(
              values = .obt_palette(length(unique(pd$df$group))),
              name   = stringr::str_to_title(gsub("_"," ", pd$gcol)))
        } else {
          p <- ggplot2::ggplot(pd$df, ggplot2::aes(x=period)) +
            ggplot2::geom_bar(fill=fill_col, colour="white",
                              width=bar_w, linewidth=0.2)
        }

        if (opts$showPeakLine && !is.na(pd$peak_date)) {
          p <- p + ggplot2::geom_vline(xintercept=as.numeric(pd$peak_date),
                                        linetype="dashed", colour="#C00000",
                                        linewidth=0.6, alpha=0.7)
        }

        p <- p +
          ggplot2::scale_x_date(date_breaks=brk_unit, date_labels=date_fmt,
                                 expand=ggplot2::expansion(add=c(bar_w*0.5, bar_w*0.5))) +
          ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,0.08)),
                                       breaks=scales::breaks_pretty()) +
          ggplot2::labs(
            title    = opts$plotTitle,
            subtitle = if (opts$showPeakLine)
              paste("Peak:", .obt_periodLabel(pd$peak_date, pd$unit)) else NULL,
            x        = opts$xLabel,
            y        = opts$yLabel,
            caption  = paste0(
              if (opts$showNLabel) paste0("N = ", pd$n, "   ") else "",
              "Developed by Gulser Dogan Turkcelik & Muammer Beslen - Turkiye FETP | outbreakTools v1.1.0"
            )
          ) +
          .obt_theme() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
          )

        print(p)
        TRUE
      }
    )
  )
