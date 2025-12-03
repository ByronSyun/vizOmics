# Plot Sankey Diagram for Classification Results

Create an interactive Sankey diagram to visualize the flow between 2 or
3 classification results. Useful for comparing clustering or
classification outcomes across different methods or timepoints.

## Usage

``` r
plotSankey(..., add_suffix = TRUE, fontsize = 12, class_names = NULL)
```

## Arguments

- ...:

  Two or three classification vectors. Each should be a vector of class
  labels (character, factor, or numeric).

- add_suffix:

  Logical. If TRUE (default), adds suffixes to distinguish between
  classification levels when plotting 3 classifications. Only used when
  3 classifications are provided.

- fontsize:

  Numeric. Font size for node labels. Default is 12.

- class_names:

  Character vector. Optional names for the classification levels. If
  NULL, uses "Class1", "Class2", etc.

## Value

An interactive Sankey diagram (networkD3 htmlwidget object).

## Details

The function automatically detects whether 2 or 3 classification vectors
are provided and creates the appropriate Sankey diagram:

- For 2 classifications: Shows direct flow from first to second

- For 3 classifications: Shows flow from first -\> second -\> third

When `add_suffix = TRUE` with 3 classifications, suffixes ("-", "\_",
"") are added to class labels to ensure uniqueness across levels.

## Examples

``` r
if (FALSE) { # \dontrun{
# Two classifications
class1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
class2 <- sample(c("X", "Y", "Z"), 100, replace = TRUE)
plotSankey(class1, class2)

# Three classifications
class3 <- sample(c("P", "Q", "R"), 100, replace = TRUE)
plotSankey(class1, class2, class3, fontsize = 14)

# With custom names
plotSankey(class1, class2, class3, 
           class_names = c("Method1", "Method2", "Method3"))
} # }
```
