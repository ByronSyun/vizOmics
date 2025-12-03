# Create circos plot for multimodal correlations

Create circos plot for multimodal correlations

## Usage

``` r
plotCircos(
  feature_list,
  data_list,
  pos_threshold = 0.3,
  neg_threshold = 0.3,
  cor_method = "pearson",
  link_transparency = 0.5,
  gap_size = 2,
  feature_label_cex = 0.6,
  legend_cex = 0.8,
  link_lwd = 1,
  label_wrap_length = 15,
  brewerPalette = "Set2"
)
```

## Arguments

- feature_list:

  Named list of vectors, each containing feature names from one modality

- data_list:

  Named list of data matrices from the same modalities as feature_list,
  rows = features, columns = samples; must have the same length as
  feature_list

- pos_threshold:

  Numeric threshold for positive correlations (default 0.3)

- neg_threshold:

  Numeric threshold for negative correlations (default 0.3)

- cor_method:

  Correlation method: "pearson", "spearman", or "kendall" (default
  "pearson")

- link_transparency:

  Transparency for links (default 0.5)

- gap_size:

  Gap between sectors in degrees (default 2)

- feature_label_cex:

  Font size for feature labels (default 0.6)

- legend_cex:

  Font size for legend (default 0.8)

- link_lwd:

  Line width for correlation links (default 1)

- label_wrap_length:

  Maximum character length before wrapping labels (default 15)

- brewerPalette:

  Name of the RColorBrewer palette, representing different modalities

## Value

A list containing:

- correlations:

  Data frame of filtered correlations

- plot_recorded:

  Recorded plot object that can be replayed with replayPlot()
