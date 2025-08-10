# ===========================================================
# perform_eval.R
# ESRD outcome, CKD stage classification, and KFRE performance metrics
# ===========================================================

# -----------------------------------------------------------
# ESRD outcome
# -----------------------------------------------------------
class_esrd_outcome <- function(df,
                               col,
                               years,
                               duration_col,
                               prefix = NULL,
                               create_years_col = TRUE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(col), length(col) == 1)
  stopifnot(is.numeric(years), length(years) == 1)
  stopifnot(is.character(duration_col), length(duration_col) == 1)

  if (!col %in% names(df)) stop(sprintf("Column '%s' not found.", col))
  if (!duration_col %in% names(df)) {
    stop(sprintf(
      "Column '%s' not found.",
      duration_col
    ))
  }

  years_col <- duration_col
  if (isTRUE(create_years_col)) {
    years_col <- "ESRD_duration_years"
    df[[years_col]] <- df[[duration_col]] / 365.25
  }

  column_name <- if (is.null(prefix)) {
    sprintf("%s_year_outcome", years)
  } else {
    sprintf("%s_%s_year_outcome", prefix, years)
  }

  if (column_name %in% names(df)) df[[column_name]] <- NULL
  df[[column_name]] <- ifelse(df[[col]] == 1 & df[[years_col]] <= years, 1L, 0L)
  df
}

# -----------------------------------------------------------
# CKD stages
# -----------------------------------------------------------
class_ckd_stages <- function(df,
                             egfr_col = "eGFR",
                             stage_col = NULL,
                             combined_stage_col = NULL) {
  stopifnot(is.data.frame(df))
  if (!egfr_col %in% names(df)) stop(sprintf("Column '%s' not found.", egfr_col))

  if (!is.null(stage_col)) {
    eg <- df[[egfr_col]]
    choices <- c(
      "CKD Stage 1", "CKD Stage 2", "CKD Stage 3a", "CKD Stage 3b",
      "CKD Stage 4", "CKD Stage 5"
    )
    conds <- list(
      eg >= 90,
      eg >= 60 & eg < 90,
      eg >= 45 & eg < 60,
      eg >= 30 & eg < 45,
      eg >= 15 & eg < 30,
      eg < 15
    )
    out <- rep("Not Classified", nrow(df))
    for (i in seq_along(conds)) out[conds[[i]]] <- choices[i]
    df[[stage_col]] <- out
  }

  if (!is.null(combined_stage_col)) {
    eg <- df[[egfr_col]]
    df[[combined_stage_col]] <- ifelse(eg < 60, "CKD Stage 3 - 5",
      "Not Classified"
    )
  }

  df
}

# -----------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------

# Fast AUC via rank-sum
.auc_fast <- function(y_true, y_prob) {
  o <- order(y_prob, decreasing = TRUE)
  y <- as.integer(y_true[o] > 0)
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  if (n_pos == 0L || n_neg == 0L) {
    return(NA_real_)
  }
  r <- rank(y_prob[o], ties.method = "average")
  (sum(r[y == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

# sklearn-compatible Average Precision, no envelope
.ap_like_sklearn <- function(y_true, y_prob) {
  if (length(y_true) == 0L) {
    return(NA_real_)
  }
  y <- as.integer(y_true > 0)
  n_pos <- sum(y == 1L)
  if (n_pos == 0L) {
    return(0)
  }

  # sort scores descending
  o <- order(y_prob, decreasing = TRUE)
  y <- y[o]
  s <- y_prob[o]

  # group by unique score thresholds
  uniq <- sort(unique(s), decreasing = TRUE)
  key <- match(s, uniq)

  pos_at_t <- as.integer(tapply(y, key, function(v) sum(v == 1L)))
  neg_at_t <- as.integer(tapply(1L - y, key, function(v) sum(v == 1L)))
  pos_at_t[is.na(pos_at_t)] <- 0L
  neg_at_t[is.na(neg_at_t)] <- 0L

  tp <- cumsum(pos_at_t)
  fp <- cumsum(neg_at_t)

  precision <- tp / pmax(tp + fp, 1L)
  recall <- tp / n_pos

  # prepend anchor (recall 0, precision 1), which sklearn's PR curve includes
  precision <- c(1, precision)
  recall <- c(0, recall)

  # integrate with the CURRENT precision at each step
  sum((recall[-1] - recall[-length(recall)]) * precision[-1])
}


# -----------------------------------------------------------
# Metrics table
# -----------------------------------------------------------
eval_kfre_metrics <- function(df,
                              n_var_list,
                              outcome_years = 2,
                              decimal_places = 6) {
  stopifnot(is.data.frame(df))

  if (is.numeric(outcome_years)) outcome_years <- as.integer(outcome_years)
  if (length(outcome_years) == 1L) outcome_years <- list(outcome_years)
  outcome_years <- as.integer(unlist(outcome_years, use.names = FALSE))

  n_var_list <- as.integer(n_var_list)

  valid_vars <- c(4L, 6L, 8L)
  if (any(!(n_var_list %in% valid_vars))) {
    stop("Invalid variable number in n_var_list. Valid options are 4, 6, 8.")
  }
  valid_outcome_years <- c(2L, 5L)
  if (any(!(outcome_years %in% valid_outcome_years))) {
    stop("Invalid value for outcome_years. Use 2, 5, or both.")
  }

  # collect truth columns
  y_true <- list()
  outcomes <- character(0)
  for (yr in outcome_years) {
    cols <- grep(sprintf(".*%d_year_outcome", yr), names(df), value = TRUE)
    if (length(cols) == 0L) {
      stop(sprintf("%d_year_outcome must exist to derive these metrics.", yr))
    }
    y_true[[length(y_true) + 1L]] <- as.integer(df[[cols[1L]]] > 0)
    outcomes <- c(outcomes, sprintf("%d_year", yr))
  }

  # predictions by n_var and year
  preds_n_var <- list()
  for (n in n_var_list) {
    preds <- vector("list", length(outcome_years))
    for (i in seq_along(outcome_years)) {
      yr <- outcome_years[i]
      col <- sprintf("kfre_%dvar_%dyear", n, yr)
      if (!col %in% names(df)) stop(sprintf("Missing column: %s", col))
      preds[[i]] <- as.numeric(df[[col]])
    }
    preds_n_var[[as.character(n)]] <- preds
  }

  rows <- list()
  for (k in seq_along(preds_n_var)) {
    n <- as.integer(names(preds_n_var)[k])
    preds <- preds_n_var[[k]]
    for (i in seq_along(outcomes)) {
      yt <- y_true[[i]]
      yp <- preds[[i]]

      # thresholded metrics at 0.5
      yhat <- as.integer(yp > 0.5)
      tp <- sum(yhat == 1L & yt == 1L)
      fp <- sum(yhat == 1L & yt == 0L)
      tn <- sum(yhat == 0L & yt == 0L)
      fn <- sum(yhat == 0L & yt == 1L)

      precision <- if ((tp + fp) == 0L) 0 else tp / (tp + fp)
      sensitivity <- if ((tp + fn) == 0L) NA_real_ else tp / (tp + fn)
      specificity <- if ((tn + fp) == 0L) NA_real_ else tn / (tn + fp)

      auc_roc <- .auc_fast(yt, yp)
      brier <- mean((yp - yt)^2)
      average_precision <- .ap_like_sklearn(yt, yp)

      rows[[length(rows) + 1L]] <- data.frame(
        `Precision/PPV` = round(precision, decimal_places),
        `Average Precision` = round(average_precision, decimal_places),
        Sensitivity = round(sensitivity, decimal_places),
        Specificity = round(specificity, decimal_places),
        `AUC ROC` = round(auc_roc, decimal_places),
        `Brier Score` = round(brier, decimal_places),
        Outcome = sprintf("%s_%d_var_kfre", outcomes[i], n),
        check.names = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    out <- data.frame(check.names = FALSE)
    rownames(out) <- character(0)
    return(out)
  }

  out <- do.call(rbind, rows)
  out <- t(as.matrix(out[, setdiff(names(out), "Outcome"), drop = FALSE]))
  colnames(out) <- vapply(rows, function(x) x$Outcome[1], character(1))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$Metrics <- rownames(out)
  rownames(out) <- out$Metrics
  out$Metrics <- NULL
  out
}

# -----------------------------------------------------------
# Plotting: ROC and PR curves with a dedicated bottom legend panel
# -----------------------------------------------------------

plot_kfre_metrics <- function(df,
                              num_vars,
                              fig_size = c(12, 6),
                              mode = "both",
                              image_path_png = NULL,
                              image_path_svg = NULL,
                              image_prefix = NULL,
                              bbox_inches = "tight", # accepted for back compat, ignored by ggsave
                              plot_type = "all_plots",
                              save_plots = FALSE,
                              show_years = c(2, 5),
                              plot_combinations = FALSE,
                              show_subplots = FALSE, # accepted for back compat, not used by gg engine
                              decimal_places = 2,
                              open_new_device = FALSE,
                              keep_aspect = TRUE) {
  # ---- validation, identical spirit to old version ----
  valid_years <- c(2L, 5L)
  if (length(show_years) == 1L) show_years <- as.integer(show_years)
  if (any(!(as.integer(show_years) %in% valid_years))) {
    stop(sprintf(
      "The 'show_years' parameter must be any of %s.",
      paste(valid_years, collapse = ", ")
    ))
  }

  if (length(num_vars) == 1L) num_vars <- as.integer(num_vars)
  valid_plot_types <- c("auc_roc", "precision_recall", "all_plots")
  if (!(plot_type %in% valid_plot_types)) {
    stop(sprintf(
      "The 'plot_type' parameter must be one of %s.",
      paste(valid_plot_types, collapse = ", ")
    ))
  }
  if (isTRUE(save_plots) && is.null(image_path_png) && is.null(image_path_svg)) {
    stop("To save plots, 'image_path_png' or 'image_path_svg' must be specified.")
  }
  if (!is.character(bbox_inches) && !is.null(bbox_inches)) {
    stop("The 'bbox_inches' parameter must be a string or NULL.")
  }

  # required prob columns
  missing <- character(0)
  for (yr in as.integer(show_years)) {
    for (n in as.integer(num_vars)) {
      col <- sprintf("kfre_%dvar_%dyear", n, yr)
      if (!(col %in% names(df))) missing <- c(missing, col)
    }
  }
  if (length(missing) > 0L) {
    stop(sprintf(
      "Must derive KFRE probabilities before generating performance metrics. Missing: %s",
      paste(missing, collapse = ", ")
    ))
  }

  # truth and outcomes
  y_true <- list()
  outcomes <- character(0)
  for (yr in as.integer(show_years)) {
    cols <- grep(sprintf(".*%d_year_outcome$", yr), names(df), value = TRUE)
    if (length(cols) == 0L) {
      stop(sprintf("No outcome columns found matching pattern for %d-year outcomes.", yr))
    }
    y_true[[length(y_true) + 1L]] <- df[[cols[1L]]]
    outcomes <- c(outcomes, sprintf("%d-year", yr))
  }

  # preds
  preds <- list()
  for (n in as.integer(num_vars)) {
    preds[[sprintf("%dvar", n)]] <- lapply(
      as.integer(show_years),
      function(yr) df[[sprintf("kfre_%dvar_%dyear", n, yr)]]
    )
  }
  result <- list(y_true = y_true, preds = preds, outcomes = outcomes)

  # honor mode = "prep"
  if (identical(mode, "prep")) {
    return(result)
  }

  # ---- helpers ----
  auc_fast <- function(yt, yp) {
    o <- order(yp, decreasing = TRUE)
    y <- as.integer(yt[o] > 0)
    n_pos <- sum(y == 1L)
    n_neg <- sum(y == 0L)
    if (n_pos == 0L || n_neg == 0L) {
      return(NA_real_)
    }
    r <- rank(yp[o], ties.method = "average")
    (sum(r[y == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  }

  # sklearn-like AP, no envelope
  ap_like_sklearn <- function(yt, yp) {
    if (length(yt) == 0L) {
      return(NA_real_)
    }
    y <- as.integer(yt > 0)
    n_pos <- sum(y == 1L)
    if (n_pos == 0L) {
      return(0)
    }
    o <- order(yp, decreasing = TRUE)
    y <- y[o]
    s <- yp[o]
    uniq <- sort(unique(s), decreasing = TRUE)
    key <- match(s, uniq)
    pos_at_t <- as.integer(tapply(y, key, function(v) sum(v == 1L)))
    pos_at_t[is.na(pos_at_t)] <- 0L
    neg_at_t <- as.integer(tapply(1L - y, key, function(v) sum(v == 1L)))
    neg_at_t[is.na(neg_at_t)] <- 0L
    tp <- cumsum(pos_at_t)
    fp <- cumsum(neg_at_t)
    precision <- tp / pmax(tp + fp, 1L)
    recall <- tp / n_pos
    precision <- c(1, precision)
    recall <- c(0, recall)
    sum((recall[-1] - recall[-length(recall)]) * precision[-1])
  }

  # base R label wrapper so we avoid adding suggests
  label_wrap <- function(x, width = 28) {
    vapply(
      x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
      character(1)
    )
  }

  make_roc_df <- function(curr_nums) {
    lst <- list()
    for (n in as.integer(curr_nums)) {
      pred_list <- preds[[sprintf("%dvar", n)]]
      for (i in seq_along(y_true)) {
        yt <- as.integer(y_true[[i]] > 0)
        yp <- as.numeric(pred_list[[i]])
        o <- order(yp, decreasing = TRUE)
        y <- yt[o]
        tps <- cumsum(y == 1L)
        fps <- cumsum(y == 0L)
        tpr <- tps / max(1L, sum(y == 1L))
        fpr <- fps / max(1L, sum(y == 0L))
        lbl <- sprintf(
          "%d-variable %s outcome (AUC = %.*f)",
          n, outcomes[i], decimal_places, auc_fast(yt, yp)
        )
        lst[[length(lst) + 1L]] <- data.frame(
          fpr = fpr, tpr = tpr, model = lbl,
          stringsAsFactors = FALSE
        )
      }
    }
    do.call(rbind, lst)
  }

  make_pr_df <- function(curr_nums) {
    lst <- list()
    for (n in as.integer(curr_nums)) {
      pred_list <- preds[[sprintf("%dvar", n)]]
      for (i in seq_along(y_true)) {
        yt <- as.integer(y_true[[i]] > 0)
        yp <- as.numeric(pred_list[[i]])
        o <- order(yp, decreasing = TRUE)
        y <- yt[o]
        tps <- cumsum(y == 1L)
        fps <- cumsum(y == 0L)
        precision <- tps / pmax(tps + fps, 1L)
        recall <- tps / max(1L, sum(y == 1L))
        precision <- c(1, precision)
        recall <- c(0, recall)
        lbl <- sprintf(
          "%d-variable %s outcome (AP = %.*f)",
          n, outcomes[i], decimal_places, ap_like_sklearn(yt, yp)
        )
        lst[[length(lst) + 1L]] <- data.frame(
          recall = recall,
          precision = precision,
          model = lbl,
          stringsAsFactors = FALSE
        )
      }
    }
    do.call(rbind, lst)
  }

  # plotting engine
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' must be installed for plotting.")
  }
  gg <- ggplot2::ggplot

  # dynamic legend columns
  legend_ncol_for <- function(n_items) {
    # square-ish grid: 2 columns up to 4 items, 3 up to 9, then 4
    if (n_items <= 4) 2 else if (n_items <= 9) 3 else 4
  }

  # choose color palette per plot using Dark2 hues
  color_scale_for <- function(n_items) {
    ggplot2::scale_color_manual(
      values = grDevices::hcl.colors(n_items, palette = "Dark2"),
      labels = function(l) label_wrap(l, width = if (fig_size[1] <= 8) 26 else 34)
    )
  }

  build_roc_plot <- function(dat, title_suffix) {
    n_items <- length(unique(dat$model))
    p <- gg(dat, ggplot2::aes(x = fpr, y = tpr, color = model)) +
      ggplot2::geom_path(linewidth = 1) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
      {
        if (isTRUE(keep_aspect)) {
          ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
        } else {
          ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
        }
      } +
      color_scale_for(n_items) +
      ggplot2::labs(
        title = sprintf("AUC ROC: %s", title_suffix),
        x = "1 - Specificity", y = "Sensitivity", color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = ggplot2::margin(t = 6),
        legend.text = ggplot2::element_text(size = 10.5),
        plot.title = ggplot2::element_text(face = "bold")
      )
    p + ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_ncol_for(n_items)))
  }

  build_pr_plot <- function(dat, title_suffix) {
    n_items <- length(unique(dat$model))
    p <- gg(dat, ggplot2::aes(x = recall, y = precision, color = model)) +
      ggplot2::geom_path(linewidth = 1) +
      {
        if (isTRUE(keep_aspect)) {
          ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
        } else {
          ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
        }
      } +
      color_scale_for(n_items) +
      ggplot2::labs(
        title = sprintf("Precision-Recall: %s", title_suffix),
        x = "Recall", y = "Precision", color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = ggplot2::margin(t = 6),
        legend.text = ggplot2::element_text(size = 10.5),
        plot.title = ggplot2::element_text(face = "bold")
      )
    p + ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_ncol_for(n_items)))
  }

  # open device if asked
  if (isTRUE(open_new_device)) {
    width <- if (length(fig_size) >= 1) fig_size[1] else 12
    height <- if (length(fig_size) >= 2) fig_size[2] else 6
    grDevices::dev.new(width = width, height = height)
  }

  # ---- plot paths ----
  if (mode %in% c("plot", "both")) {
    title_suffix <- if (length(num_vars) > 1) {
      sprintf("%s Variable KFRE", paste(num_vars, collapse = ", "))
    } else {
      sprintf("%d Variable KFRE", as.integer(num_vars)[1])
    }

    # combined
    if (isTRUE(plot_combinations)) {
      if (plot_type %in% c("auc_roc", "all_plots")) {
        roc_df <- make_roc_df(num_vars)
        p_roc <- build_roc_plot(roc_df, title_suffix)
        print(p_roc)
        if (isTRUE(save_plots)) {
          if (!is.null(image_path_png)) {
            dir.create(image_path_png, recursive = TRUE, showWarnings = FALSE)
            ggplot2::ggsave(
              filename = file.path(
                image_path_png,
                sprintf(
                  "%s_auc_roc_curve_combined.png",
                  if (is.null(image_prefix)) "auc_roc_curve_combined" else image_prefix
                )
              ),
              plot = p_roc, width = fig_size[1], height = fig_size[2],
              units = "in", dpi = 300
            )
          }
          if (!is.null(image_path_svg)) {
            dir.create(image_path_svg, recursive = TRUE, showWarnings = FALSE)
            ggplot2::ggsave(
              filename = file.path(
                image_path_svg,
                sprintf(
                  "%s_auc_roc_curve_combined.svg",
                  if (is.null(image_prefix)) "auc_roc_curve_combined" else image_prefix
                )
              ),
              plot = p_roc, width = fig_size[1], height = fig_size[2],
              units = "in"
            )
          }
        }
      }

      if (plot_type %in% c("precision_recall", "all_plots")) {
        pr_df <- make_pr_df(num_vars)
        p_pr <- build_pr_plot(pr_df, title_suffix)
        print(p_pr)
        if (isTRUE(save_plots)) {
          if (!is.null(image_path_png)) {
            dir.create(image_path_png, recursive = TRUE, showWarnings = FALSE)
            ggplot2::ggsave(
              filename = file.path(
                image_path_png,
                sprintf(
                  "%s_pr_curve_combined.png",
                  if (is.null(image_prefix)) "pr_curve_combined" else image_prefix
                )
              ),
              plot = p_pr, width = fig_size[1], height = fig_size[2],
              units = "in", dpi = 300
            )
          }
          if (!is.null(image_path_svg)) {
            dir.create(image_path_svg, recursive = TRUE, showWarnings = FALSE)
            ggplot2::ggsave(
              filename = file.path(
                image_path_svg,
                sprintf(
                  "%s_pr_curve_combined.svg",
                  if (is.null(image_prefix)) "pr_curve_combined" else image_prefix
                )
              ),
              plot = p_pr, width = fig_size[1], height = fig_size[2], units = "in"
            )
          }
        }
      }
    } else {
      # separate plots per n
      for (n in as.integer(num_vars)) {
        title_suffix_n <- sprintf("%d Variable KFRE", n)

        if (plot_type %in% c("auc_roc", "all_plots")) {
          roc_df_n <- make_roc_df(n)
          p_roc <- build_roc_plot(roc_df_n, title_suffix_n)
          print(p_roc)
          if (isTRUE(save_plots)) {
            if (!is.null(image_path_png)) {
              dir.create(image_path_png, recursive = TRUE, showWarnings = FALSE)
              ggplot2::ggsave(
                filename = file.path(
                  image_path_png,
                  sprintf(
                    "%s_%dvar_auc_roc.png",
                    if (is.null(image_prefix)) as.character(n) else image_prefix, n
                  )
                ),
                plot = p_roc, width = fig_size[1], height = fig_size[2],
                units = "in", dpi = 300
              )
            }
            if (!is.null(image_path_svg)) {
              dir.create(image_path_svg, recursive = TRUE, showWarnings = FALSE)
              ggplot2::ggsave(
                filename = file.path(
                  image_path_svg,
                  sprintf(
                    "%s_%dvar_auc_roc.svg",
                    if (is.null(image_prefix)) as.character(n) else image_prefix, n
                  )
                ),
                plot = p_roc, width = fig_size[1], height = fig_size[2], units = "in"
              )
            }
          }
        }

        if (plot_type %in% c("precision_recall", "all_plots")) {
          pr_df_n <- make_pr_df(n)
          p_pr <- build_pr_plot(pr_df_n, title_suffix_n)
          print(p_pr)
          if (isTRUE(save_plots)) {
            if (!is.null(image_path_png)) {
              dir.create(image_path_png, recursive = TRUE, showWarnings = FALSE)
              ggplot2::ggsave(
                filename = file.path(
                  image_path_png,
                  sprintf(
                    "%s_%dvar_precision_recall.png",
                    if (is.null(image_prefix)) as.character(n) else image_prefix, n
                  )
                ),
                plot = p_pr, width = fig_size[1], height = fig_size[2], units = "in", dpi = 300
              )
            }
            if (!is.null(image_path_svg)) {
              dir.create(image_path_svg, recursive = TRUE, showWarnings = FALSE)
              ggplot2::ggsave(
                filename = file.path(
                  image_path_svg,
                  sprintf(
                    "%s_%dvar_precision_recall.svg",
                    if (is.null(image_prefix)) as.character(n) else image_prefix, n
                  )
                ),
                plot = p_pr, width = fig_size[1], height = fig_size[2], units = "in"
              )
            }
          }
        }
      }
    }
  }

  if (identical(mode, "both")) {
    return(result)
  }
  invisible(NULL)
}

# ===========================================================
# End of perform_eval.R
# ===========================================================
