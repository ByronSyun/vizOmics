# Plot loadings of dimension reduction as bar plot.

Plot loadings of dimension reduction as bar plot.

## Usage

``` r
plotLoadings(
  Loadings,
  comp = "comp1",
  showInt = F,
  absVal = T,
  showNeg = F,
  nfeat = 30,
  fsize = 14,
  xlab = ""
)
```

## Arguments

- Loadings:

  A data.frame or an object (eg matrix) that's convertable to
  data.frame: columns are components and rows are features.

- comp:

  Character. Which component to plot.

- showInt:

  Logical. Weather to show cell type interaction (eg in PhiSpace ST cell
  type co-presence analysis).

- absVal:

  Logical. Rank loadings by absolute values or not.

- showNeg:

  Logical. Show negative loadings or not.

- nfeat:

  Number of top loadings to show.

- fsize:

  Font size.

- xlab:

  x axis title.

## Value

A ggplot2 object.
