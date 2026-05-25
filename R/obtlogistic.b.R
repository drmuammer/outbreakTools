obtlogisticClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class("obtlogisticClass",
    inherit = obtlogisticBase,
    private = list(
      .forestData = NULL,

      .run = function() {
        opts   <- self$options
        if (is.null(opts$outcomeVar) || length(opts$exposures) == 0) {
          self$results$logisticNote$setContent(
            .obt_infoBox("Instructions",
              "Assign <b>Outcome</b> and at least one <b>Exposure/Predictor</b> variable.",
              "#1F4E79"))
          return(invisible(NULL))
        }
        data   <- self$data
        out_c  <- opts$outcomeVar; cas_l <- opts$caseLevel
        lev    <- opts$ciLevel
        exps   <- opts$exposures

        data$.outcome <- as.integer(as.character(data[[out_c]]) == cas_l &
                                    !is.na(data[[out_c]]))

        for (v in exps) {
          if (is.character(data[[v]]) || is.factor(data[[v]])) {
            data[[v]] <- as.factor(data[[v]])
          }
        }

        ut <- self$results$univarTable; ut$deleteRows()
        screened_in <- character(0)

        for (v in exps) {
          tryCatch({
            frm <- stats::as.formula(paste0(".outcome ~ `", v, "`"))
            mod <- stats::glm(frm, data=data, family=stats::binomial)
            sm  <- summary(mod)$coefficients
            if (nrow(sm) < 2) return(NULL)
            p_screen <- min(sm[-1, 4], na.rm=TRUE)
            in_mv    <- if (opts$autoScreen) p_screen < 0.25 else TRUE
            if (in_mv) screened_in <- c(screened_in, v)
            for (i in 2:nrow(sm)) {
              term_nm <- rownames(sm)[i]
              or_v    <- exp(sm[i, 1])
              ci      <- suppressMessages(stats::confint(mod, level=lev))
              ci_lo   <- if (nrow(ci) >= i) exp(ci[i, 1]) else NA_real_
              ci_hi   <- if (nrow(ci) >= i) exp(ci[i, 2]) else NA_real_
              ut$addRow(rowKey=paste0(v,"_",i),
                        values=list(variable=v, level=term_nm,
                                    or=or_v, ci_low=ci_lo, ci_high=ci_hi,
                                    p_value=sm[i,4],
                                    screened=if(in_mv) "Yes" else "No (p>=0.25)"))
            }
          }, error=function(e) NULL)
        }

        if (length(screened_in) == 0) screened_in <- exps
        mv_frm <- stats::as.formula(
          paste0(".outcome ~ ", paste0("`", screened_in, "`", collapse=" + ")))
        mv_mod <- tryCatch(
          stats::glm(mv_frm, data=data, family=stats::binomial),
          error=function(e) NULL)

        mvt <- self$results$multivarTable; mvt$deleteRows()
        forest_rows <- list()

        if (!is.null(mv_mod)) {
          sm_mv <- summary(mv_mod)$coefficients
          ci_mv <- suppressMessages(stats::confint(mv_mod, level=lev))
          for (i in 2:nrow(sm_mv)) {
            term_nm <- rownames(sm_mv)[i]
            or_v    <- exp(sm_mv[i,1])
            ci_lo   <- if (i <= nrow(ci_mv)) exp(ci_mv[i,1]) else NA_real_
            ci_hi   <- if (i <= nrow(ci_mv)) exp(ci_mv[i,2]) else NA_real_
            mvt$addRow(rowKey=term_nm,
                       values=list(variable=term_nm, or=or_v,
                                   ci_low=ci_lo, ci_high=ci_hi, p_value=sm_mv[i,4]))
            forest_rows[[term_nm]] <- list(label=term_nm, or=or_v, lo=ci_lo, hi=ci_hi)
          }

          if (opts$showFit) {
            fit_t <- self$results$fitTable; fit_t$deleteRows()
            null_dev <- mv_mod$null.deviance
            res_dev  <- mv_mod$deviance
            df_null  <- mv_mod$df.null
            df_res   <- mv_mod$df.residual
            lrt_chi  <- null_dev - res_dev
            lrt_df   <- df_null - df_res
            lrt_p    <- stats::pchisq(lrt_chi, df=lrt_df, lower.tail=FALSE)
            n_obs    <- nrow(mv_mod$model)
            null_mod <- stats::glm(.outcome ~ 1, data=data, family=stats::binomial)
            r2_mf    <- 1 - (stats::logLik(mv_mod) / stats::logLik(null_mod))
            rows_fit <- list(
              c("N observations",    as.character(n_obs)),
              c("Null deviance",     sprintf("%.2f (df=%d)", null_dev, df_null)),
              c("Residual deviance", sprintf("%.2f (df=%d)", res_dev, df_res)),
              c("LRT chi-square",    sprintf("%.2f (df=%d, p=%.3f)", lrt_chi, lrt_df, lrt_p)),
              c("McFadden R2",       sprintf("%.3f", as.numeric(r2_mf))),
              c("AIC",               sprintf("%.2f", stats::AIC(mv_mod))),
              c("BIC",               sprintf("%.2f", stats::BIC(mv_mod)))
            )
            for (r in rows_fit)
              fit_t$addRow(rowKey=r[[1]], values=list(metric=r[[1]], value=r[[2]]))
          }
          private$.forestData <- forest_rows
        }

        note <- sprintf(
          '<div style="font-family:Arial,sans-serif;font-size:12px;line-height:1.85;">
           <b>Outcome:</b> %s = %s | <b>Predictors:</b> %d<br>
           <b>Variables in MV model:</b> %s<br>
           <b>Method:</b> Binary logistic regression (binomial GLM, logit link).<br>
           <b>Developed by:</b> Gulser Dogan Turkcelik &amp; Muammer Beslen - Turkiye FETP</div>',
          opts$outcomeVar, opts$caseLevel, length(exps),
          paste(screened_in, collapse=", "))
        self$results$logisticNote$setContent(note)
      },

      .plotForest = function(image, ggtheme, theme, ...) {
        if (is.null(private$.forestData) || length(private$.forestData) == 0)
          return(FALSE)
        df <- do.call(rbind, lapply(private$.forestData, function(r)
          data.frame(label=r$label, or=r$or, lo=r$lo, hi=r$hi,
                     stringsAsFactors=FALSE)))
        df$label <- factor(df$label, levels=rev(df$label))
        p <- ggplot2::ggplot(df, ggplot2::aes(y=label, x=or, xmin=lo, xmax=hi)) +
          ggplot2::geom_vline(xintercept=1, linetype="dashed", colour="grey50") +
          ggplot2::geom_errorbarh(height=0.25, colour="#1F4E79", linewidth=0.8) +
          ggplot2::geom_point(size=3.5, colour="#C00000") +
          ggplot2::scale_x_log10() +
          ggplot2::labs(x="Adjusted Odds Ratio (log scale)", y=NULL,
                        title="Multivariable Logistic Regression - Adjusted OR",
                        caption="outbreakTools v1.0.0 | Turkiye FETP") +
          .obt_theme()
        print(p); TRUE
      }
    )
  )
