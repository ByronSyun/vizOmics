# Score matrix plot

Density and pairwise scatter plots for visualising score matrices.

## Usage

``` r
matrixPlot(
  scores,
  max_ncomp = NULL,
  comp_idx = NULL,
  var_names = NULL,
  colBy = NULL,
  pointAlpha = NULL,
  pointSize = 1,
  manualCol = NULL,
  manualAlpha = NULL,
  fsize = 14,
  returnPlotList = F,
  legendTitle = "",
  compName = "comp",
  legendPosition = c("right", "left", "bottom", "top", "inside", "none")
)
```

## Arguments

- scores:

  Matrix of scores to be plotted

- max_ncomp:

  Default NULL. Number of first components to plot. If specified, will
  override comp_idx and var_names.

- comp_idx:

  Default NULL. Indices of components to plot. Will override var_names
  if specified.

- var_names:

  Default NULL. Character vector of variable names (column names) to
  plot. Will be overridden by max_ncomp or comp_idx if they are
  specified.

- colBy:

  Numeric or charactor vectors to specify colour of points.

- pointAlpha:

  Alpha value.

- pointSize:

  Point size.

- manualCol:

  Manual specification of colours.

- manualAlpha:

  Manual specification of alpha colours.

- fsize:

  Figure font size.

- returnPlotList:

  Logical. Whether to return individual plots.

- legendTitle:

  Legend title.

- compName:

  Name of the components, default is "comp", so that the 1st column is
  named comp1 etc.

- legendPosition:

  Location of the legend when plotting two scores.
