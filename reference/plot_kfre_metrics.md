# Plot ROC and Precision Recall curves for KFRE variants

Draws ROC and PR curves for the selected models and horizons. When
`mode` is "both", returns a list of ggplot objects. Otherwise, draws the
plots and returns `NULL` invisibly.

## Arguments

- df:

  Data frame containing model probabilities and outcomes.

- num_vars:

  Integer vector, KFRE model sizes to plot, any of 4, 6, 8.

- fig_size:

  Numeric length 2, plot device width and height.

- mode:

  Character, "both" to return plots, otherwise draw only.

- image_path_png:

  Optional directory to save PNGs.

- image_path_svg:

  Optional directory to save SVGs.

- image_prefix:

  Optional filename prefix.

- bbox_inches:

  Character, passed through when saving.

- plot_type:

  Character, which plots to render.

- save_plots:

  Logical, save plots to disk if TRUE.

- show_years:

  Integer vector, horizons to show, any of 2, 5.

- plot_combinations:

  Logical, if TRUE draw pairwise overlays.

- show_subplots:

  Logical, arrange subplots when TRUE.

- decimal_places:

  Integer digits for annotations.

- open_new_device:

  Logical, open device before plotting.

## Value

List of ggplot objects when `mode == "both"`, else `NULL`.
