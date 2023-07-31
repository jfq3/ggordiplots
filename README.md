
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggordiplots

<!-- badges: start -->

[![R-CMD-check](https://github.com/jfq3/ggordiplots/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jfq3/ggordiplots/actions/workflows/R-CMD-check.yaml)
[![Github All
Releases](https://img.shields.io/github/downloads/jfq3/ggordiplots/total.svg)]()
<!-- badges: end -->

The `vegan` package includes several functions for adding features to
ordination plots: ordiarrows, ordibubbles, ordiellipse, ordihull,
ordispider, ordisurf. This package adds these same features to
ordination plots made with `ggplot2`.

The functions are written so that features from each can be combined in
customized ordination plots.

The funcitons `ord_labels()` and `scale_arrow()` (used to ensure vector
arrows fit within a plot) are exported to make it easier to generate
custom ordination plots.

## Installation

You can install the development version of ggordiplots from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jfq3/ggordiplots")
```

You can install the latest release from CRAN with:

``` r
install.packages("ggordiplots")
```

## Examples

Plot an ordination with ellipses around treatment groups with
`gg_ordiplot()`.

``` r
library(ggordiplots)
#> Loading required package: ggplot2
#> Loading required package: vegan
#> Loading required package: permute
#> Loading required package: lattice
#> This is vegan 2.6-4
#> Loading required package: glue
data("dune")
data("dune.env")
dune_bray <- vegdist(dune, method = "bray")
ord <- cmdscale(dune_bray, k = (nrow(dune) - 1), eig = TRUE, add = TRUE)
#> Warning in cmdscale(dune_bray, k = (nrow(dune) - 1), eig = TRUE, add = TRUE):
#> only 18 of the first 19 eigenvalues are > 0
plt1 <- gg_ordiplot(ord, groups = dune.env$Management, plot = FALSE)
```

`plt1` is list with items named:

``` r
names(plt1)
#> [1] "df_ord"      "df_mean.ord" "df_ellipse"  "df_hull"     "df_spiders" 
#> [6] "plot"
```

The first 5 items are dataframes for making plots. The last item is a
ggplot:

``` r
plt1$plot
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /> Fit
a vector of Al concentrations to the ordination with `gg_envfit()`.

``` r
Al <- as.data.frame(dune.env$A1)
colnames(Al) <- "Al"
plt2 <- gg_envfit(ord, env = Al, groups = dune.env$Management, plot = FALSE)
plt2$plot
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" /> Add
ellipses fromt he first plot toe the second plot. The plot can be
further customized using usual `ggplot2` methods. For example, change
the legend title.

``` r
plt2$plot +
  geom_path(data = plt1$df_ellipse, aes(x=x, y=y, color=Group)) +
  guides(color=guide_legend(title="Management")) # Change legend title
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
