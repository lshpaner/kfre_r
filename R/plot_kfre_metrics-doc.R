#' Plot ROC and Precision Recall curves for KFRE variants
#'
#' Draws ROC and PR curves for the selected models and horizons. When
#' `mode` is "both", returns a list of ggplot objects. Otherwise, draws
#' the plots and returns `NULL` invisibly.
#'
#' @param df Data frame containing model probabilities and outcomes.
#' @param num_vars Integer vector, KFRE model sizes to plot, any of 4, 6, 8.
#' @param fig_size Numeric length 2, plot device width and height.
#' @param mode Character, "both" to return plots, otherwise draw only.
#' @param image_path_png Optional directory to save PNGs.
#' @param image_path_svg Optional directory to save SVGs.
#' @param image_prefix Optional filename prefix.
#' @param bbox_inches Character, passed through when saving.
#' @param plot_type Character, which plots to render.
#' @param save_plots Logical, save plots to disk if TRUE.
#' @param show_years Integer vector, horizons to show, any of 2, 5.
#' @param plot_combinations Logical, if TRUE draw pairwise overlays.
#' @param show_subplots Logical, arrange subplots when TRUE.
#' @param decimal_places Integer digits for annotations.
#' @param open_new_device Logical, open device before plotting.
#'
#' @return List of ggplot objects when `mode == "both"`, else `NULL`.
#' @name plot_kfre_metrics
#' @aliases plot_kfre_metrics
#' @export
NULL
