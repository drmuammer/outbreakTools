# =============================================================================
# outbreakTools - Data Quality & Linelist Check
# File: R/obt_clean.b.R
# Authors: Gülser Doğan Türkçelik & Muammer Beslen - Türkiye FETP
# =============================================================================

obtcleanClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class("obtcleanClass",
    inherit = obtcleanBase,
    private = list(

      .run = function() {
        data  <- self$data
        opts  <- self$options
        issues <- character(0)
        n_rows <- nrow(data)
        n_cols <- ncol(data)

        # ── OVERVIEW ────────────────────────────────────────────────────────
        ov <- self$results$overviewTable
        ov$deleteRows()
        complete_n <- sum(complete.cases(data))
        rows_ov <- list(
          c("Total records (rows)",          as.character(n_rows)),
          c("Total variables (columns)",     as.character(n_cols)),
          c("Complete cases (no missing)",   as.character(complete_n)),
          c("Records with ≥1 missing value", as.character(n_rows - complete_n)),
          c("Completeness (%)",              sprintf("%.1f%%", 100*complete_n/n_rows))
        )
        for (r in rows_ov)
          ov$addRow(rowKey = r[[1]], values = list(metric=r[[1]], value=r[[2]]))

        # ── MISSING DATA ─────────────────────────────────────────────────────
        if (opts$showMissing) {
          mt <- self$results$missingTable
          mt$deleteRows()
          thresh <- opts$missingThreshold / 100
          for (vname in names(data)) {
            col <- data[[vname]]
            n_miss <- sum(is.na(col) |
                          (is.character(col) & col %in% c("","NA","N/A",".",
                           "99/99/99","Unknown","unknown")))
            pct_miss <- n_miss / n_rows
            flag_txt <- if (pct_miss > thresh) {
              issues <- c(issues, sprintf(
                "Variable <b>%s</b>: %.0f%% missing (threshold %.0f%%)",
                vname, pct_miss*100, opts$missingThreshold))
              paste0("[!] >", opts$missingThreshold, "% missing")
            } else if (pct_miss > 0) "[i] Some missing" else "[OK] Complete"
            mt$addRow(rowKey = vname,
                      values = list(variable=vname, nMissing=as.integer(n_miss),
                                    pctMissing=pct_miss, flag=flag_txt))
          }
        }

        # ── DUPLICATE IDs ────────────────────────────────────────────────────
        if (opts$showDuplicates && !is.null(opts$idVar)) {
          dt <- self$results$duplicateTable
          dt$deleteRows()
          ids   <- as.character(data[[opts$idVar]])
          id_tb <- table(ids)
          dupes <- id_tb[id_tb > 1]
          if (length(dupes) == 0) {
            dt$addRow(rowKey="ok",
                      values=list(id="[OK] No duplicate IDs found", count=0L))
          } else {
            issues <- c(issues, sprintf(
              "<b>%d duplicate ID(s)</b> detected in <b>%s</b>", length(dupes), opts$idVar))
            for (i in seq_along(dupes))
              dt$addRow(rowKey=names(dupes)[i],
                        values=list(id=names(dupes)[i], count=as.integer(dupes[[i]])))
          }
        }

        # ── DATE AUDIT ───────────────────────────────────────────────────────
        if (length(opts$dateVars) > 0) {
          dat_t <- self$results$dateTable
          dat_t$deleteRows()
          today <- Sys.Date()
          ref_start <- if (nchar(trimws(opts$refStartDate)) > 0)
            suppressWarnings(lubridate::ymd(opts$refStartDate)) else NULL
          row_key <- 1L

          for (dvar in opts$dateVars) {
            parsed <- .obt_parseDate(data[[dvar]], format = opts$dateFormat)

            # Future dates
            n_fut <- sum(parsed > today, na.rm = TRUE)
            if (n_fut > 0) {
              ex <- paste(format(head(sort(parsed[!is.na(parsed) & parsed > today],
                                          decreasing=TRUE), 2), "%Y-%m-%d"),
                          collapse=", ")
              dat_t$addRow(rowKey=row_key,
                           values=list(variable=dvar, issue="Future date (after today)",
                                       nAffected=as.integer(n_fut), examples=ex))
              row_key <- row_key + 1L
              issues <- c(issues, sprintf("<b>%s</b>: %d future date(s)", dvar, n_fut))
            }
            # Before outbreak start
            if (!is.null(ref_start)) {
              n_early <- sum(!is.na(parsed) & parsed < ref_start)
              if (n_early > 0) {
                ex <- paste(format(head(sort(parsed[!is.na(parsed) & parsed < ref_start]), 2),
                                   "%Y-%m-%d"), collapse=", ")
                dat_t$addRow(rowKey=row_key,
                             values=list(variable=dvar,
                                         issue=paste0("Before outbreak start (", opts$refStartDate, ")"),
                                         nAffected=as.integer(n_early), examples=ex))
                row_key <- row_key + 1L
              }
            }
            # Parse failures
            raw_na  <- is.na(data[[dvar]]) | (is.character(data[[dvar]]) &
                         data[[dvar]] %in% c("","NA","N/A","."))
            parse_fail <- is.na(parsed) & !raw_na
            n_fail <- sum(parse_fail)
            if (n_fail > 0) {
              ex <- paste(head(as.character(data[[dvar]][parse_fail]), 3), collapse=", ")
              dat_t$addRow(rowKey=row_key,
                           values=list(variable=dvar, issue="Could not parse as date",
                                       nAffected=as.integer(n_fail), examples=ex))
              row_key <- row_key + 1L
              issues <- c(issues, sprintf("<b>%s</b>: %d unparseable date(s) - check format", dvar, n_fail))
            }
            if (n_fut == 0 && n_fail == 0 && (is.null(ref_start) ||
                sum(!is.na(parsed) & parsed < ref_start) == 0)) {
              dat_t$addRow(rowKey=row_key,
                           values=list(variable=dvar, issue="[OK] No date issues",
                                       nAffected=0L, examples=""))
              row_key <- row_key + 1L
            }
          }
        }

        # ── NUMERIC AUDIT ────────────────────────────────────────────────────
        if (length(opts$numVars) > 0) {
          num_t <- self$results$numTable
          num_t$deleteRows()
          for (nvar in opts$numVars) {
            vals  <- suppressWarnings(as.numeric(data[[nvar]]))
            min_v <- min(vals, na.rm=TRUE); max_v <- max(vals, na.rm=TRUE)
            mn_v  <- mean(vals, na.rm=TRUE)
            n_mis <- sum(is.na(vals))
            flag  <- ""
            if (!is.null(opts$ageVar) && length(opts$ageVar) > 0 && nvar == opts$ageVar) {
              if (min_v < opts$ageMin || max_v > opts$ageMax) {
                flag <- sprintf("[!] Out of range [%d-%d]", opts$ageMin, opts$ageMax)
                issues <- c(issues, sprintf(
                  "<b>%s</b>: values outside plausible age range [%d-%d]",
                  nvar, opts$ageMin, opts$ageMax))
              }
            }
            num_t$addRow(rowKey=nvar,
                         values=list(variable=nvar,
                                     min_val=min_v, max_val=max_v, mean_val=mn_v,
                                     n_miss=as.integer(n_mis), flag=flag))
          }
        }

        # ── CATEGORICAL AUDIT ────────────────────────────────────────────────
        if (opts$showCatIssues && length(opts$catVars) > 0) {
          cat_t <- self$results$catTable
          cat_t$deleteRows()
          row_key <- 1L
          for (cvar in opts$catVars) {
            col  <- as.character(data[[cvar]])
            tab  <- sort(table(col, useNA="always"), decreasing=TRUE)
            lvls <- tolower(names(tab)[!is.na(names(tab))])
            case_inconsistent <- length(lvls) != length(unique(lvls))
            if (case_inconsistent) {
              issues <- c(issues, sprintf(
                "<b>%s</b>: inconsistent capitalisation detected (e.g. 'Male' vs 'male')", cvar))
            }
            for (i in seq_along(tab)) {
              lvl_nm <- names(tab)[i]
              n_lvl  <- as.integer(tab[i])
              pct_v  <- n_lvl / n_rows
              flag   <- ""
              if (is.na(lvl_nm)) {
                lvl_nm <- "(Missing)"
                if (pct_v > opts$missingThreshold/100) flag <- "[!] High missing"
              } else if (n_lvl == 1) {
                flag <- "[!] Singleton (n=1)"
              } else if (case_inconsistent) {
                flag <- "[!] Check capitalisation"
              }
              cat_t$addRow(rowKey=row_key,
                           values=list(variable=cvar, level=lvl_nm,
                                       n=n_lvl, pct=pct_v, flag=flag))
              row_key <- row_key + 1L
            }
          }
        }

        # ── CLUSTER DETECTION (OpenRefine-style) ─────────────────────────────
        # Detects likely duplicate string variants:
        # "İSTANBUL", "istanbul", "ISTANBUL", "Ist." → same cluster
        clusters_per_var <- list()
        if (isTRUE(opts$showClustering) && length(opts$catVars) > 0) {
          ct <- self$results$clusterTable
          ct$deleteRows()
          row_key <- 1L
          for (cvar in opts$catVars) {
            cl <- tryCatch(
              .obt_cluster(data[[cvar]],
                            method = opts$clusterMethod %||% "fingerprint"),
              error = function(e) NULL
            )
            if (is.null(cl) || nrow(cl) == 0) next

            clusters_per_var[[cvar]] <- cl

            # For each cluster, the most-frequent variant becomes the suggestion
            cl_split <- split(cl, cl$cluster_id)
            for (gid in names(cl_split)) {
              g <- cl_split[[gid]]
              suggestion <- g$original[which.max(g$n_occurrences)]
              for (i in seq_len(nrow(g))) {
                ct$addRow(rowKey = row_key,
                          values = list(
                            variable   = cvar,
                            cluster_id = as.integer(g$cluster_id[i]),
                            variant    = g$original[i],
                            n          = as.integer(g$n_occurrences[i]),
                            suggestion = suggestion))
                row_key <- row_key + 1L
              }
            }
            issues <- c(issues, sprintf(
              "<b>%s</b>: %d duplicate-variant cluster(s) detected (%d total variants)",
              cvar, length(cl_split), nrow(cl)))
          }
          # If no clusters anywhere, add a friendly placeholder row
          if (row_key == 1L) {
            ct$addRow(rowKey = "ok", values = list(
              variable = "-",
              cluster_id = 0L,
              variant = "[OK] No duplicate-variant clusters detected",
              n = 0L,
              suggestion = ""))
          }
        }

        # ── DATE FORMAT DIVERSITY ────────────────────────────────────────────
        # Detects mixed date formats within the same column
        if (isTRUE(opts$showDateDiversity) && length(opts$dateVars) > 0) {
          dft <- self$results$dateFormatTable
          dft$deleteRows()
          row_key <- 1L
          for (dvar in opts$dateVars) {
            fmt <- tryCatch(
              .obt_detectDateFormats(data[[dvar]]),
              error = function(e) NULL
            )
            if (is.null(fmt) || nrow(fmt) == 0) next
            for (i in seq_len(nrow(fmt))) {
              dft$addRow(rowKey = row_key,
                         values = list(
                           variable = dvar,
                           pattern  = fmt$pattern[i],
                           n        = as.integer(fmt$n[i]),
                           examples = fmt$examples[i]))
              row_key <- row_key + 1L
            }
            if (nrow(fmt) > 1) {
              issues <- c(issues, sprintf(
                "<b>%s</b>: %d different date format(s) found - inconsistent data entry",
                dvar, nrow(fmt)))
            }
          }
        }

        # ── R CLEANING RECIPES (copy-paste code blocks) ──────────────────────
        if (isTRUE(opts$showRCode)) {
          recipes <- character(0)

          # Intro block
          recipes <- c(recipes, sprintf(
            '<div style="font-family:\'Segoe UI\',Arial,sans-serif;font-size:12.5px;
                        background:#FFF8E5;border-left:4px solid #C55A11;
                        padding:10px 14px;margin:6px 0 14px 0;">
              <b>How to use these recipes:</b><br>
              Copy any code block below and paste into <b>Rj Editor</b>
              (Modules -> jamovi-library -> Rj), <b>RStudio</b>, or <b>RGui</b>.
              Each recipe creates a new <i>_clean</i> column instead of
              overwriting the original, so you can review changes before
              committing. Replace <code>data</code> with your actual data
              frame name if different.
            </div>'))

          # Recipe per categorical variable (normalize + merge clusters)
          if (length(opts$catVars) > 0) {
            for (cvar in opts$catVars) {
              recipes <- c(recipes, .obt_recipeNormalize(cvar))
              # If clusters exist, suggest merges
              cl <- clusters_per_var[[cvar]]
              if (!is.null(cl) && nrow(cl) > 0) {
                cl_split <- split(cl, cl$cluster_id)
                for (g in cl_split) {
                  canonical <- g$original[which.max(g$n_occurrences)]
                  variants  <- g$original[g$original != canonical]
                  if (length(variants) > 0) {
                    recipes <- c(recipes,
                      .obt_recipeMergeCluster(cvar, variants, canonical))
                  }
                }
              }
              recipes <- c(recipes, .obt_recipeRecodeNA(cvar))
            }
          }

          # Recipe per date variable
          if (length(opts$dateVars) > 0) {
            for (dvar in opts$dateVars) {
              recipes <- c(recipes, .obt_recipeParseDate(dvar))
            }
          }

          # Recipe per numeric variable (with range)
          if (length(opts$numVars) > 0) {
            for (nvar in opts$numVars) {
              # Default to age range if this is the age variable
              if (!is.null(opts$ageVar) && nvar == opts$ageVar) {
                recipes <- c(recipes,
                  .obt_recipeFlagOutliers(nvar, opts$ageMin, opts$ageMax))
              } else {
                vals <- suppressWarnings(as.numeric(data[[nvar]]))
                if (any(!is.na(vals))) {
                  q <- stats::quantile(vals, c(0.01, 0.99), na.rm = TRUE)
                  recipes <- c(recipes,
                    .obt_recipeFlagOutliers(nvar,
                      round(q[1], 2), round(q[2], 2)))
                }
              }
            }
          }

          if (length(recipes) <= 1) {
            recipes <- c(recipes,
              .obt_okBox("No automated recipes generated. Select categorical, date, or numeric variables in the options panel to get cleaning code snippets."))
          }

          self$results$rRecipes$setContent(paste(recipes, collapse = "\n"))
        }

        # ── CLEANING REPORT ──────────────────────────────────────────────────
        if (length(issues) == 0) {
          body <- .obt_okBox("No issues detected. Linelist passed all automated checks.")
        } else {
          li <- paste0("<li style='margin-bottom:4px;'>", issues, "</li>", collapse="")
          body <- sprintf('<p style="color:#7B2C2C;font-weight:bold;">%d issue(s) found:</p>
                           <ul style="line-height:1.9;padding-left:18px;">%s</ul>',
                          length(issues), li)
        }

        html <- sprintf(
          '<div style="font-family:\'Segoe UI\',Arial,sans-serif;font-size:13px;">
           <div style="background:#1F4E79;color:white;padding:10px 14px;border-radius:4px 4px 0 0;margin-bottom:8px;">
           <b>[REPORT] Linelist Cleaning Report</b> &nbsp;|&nbsp;
           outbreakTools v1.0.0 &nbsp;|&nbsp; %s</div>
           <p><b>Records:</b> %d &nbsp;|&nbsp; <b>Variables:</b> %d &nbsp;|&nbsp;
           <b>Complete cases:</b> %d (%.0f%%)</p>%s
           <p style="color:#888;font-size:10.5px;margin-top:14px;border-top:1px solid #eee;padding-top:6px;">
           Developed by Gülser Doğan Türkçelik &amp; Muammer Beslen - Türkiye FETP</p></div>',
          format(Sys.time(), "%Y-%m-%d %H:%M"),
          n_rows, n_cols, complete_n, 100*complete_n/n_rows, body
        )
        self$results$cleaningReport$setContent(html)
      }
    )
  )
