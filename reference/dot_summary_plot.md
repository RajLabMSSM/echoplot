# Dot summary plot

Multi-fine-map summary dot plot of all lead, UCS and/or Consensus SNPs.

## Usage

``` r
dot_summary_plot(dat, credset_thresh = 0.95, show_plot = TRUE, verbose = TRUE)
```

## Arguments

- dat:

  Data to query transcripts with.

- credset_thresh:

  The minimum fine-mapped posterior probability for a SNP to be
  considered part of a Credible Set. For example, `credset_thresh=.95`
  means that all Credible Set SNPs will be 95% Credible Set SNPs.

- show_plot:

  Print plot to screen.

- verbose:

  Print messages.

## See also

Other plot:
[`add_multitrack_lines()`](https://rajlabmssm.github.io/echoplot/reference/add_multitrack_lines.md),
[`get_window_suffix()`](https://rajlabmssm.github.io/echoplot/reference/get_window_suffix.md)

## Examples

``` r
dat <- echodata::BST1
gp <- echoplot::dot_summary_plot(dat = dat)
#> ++ echoplot:: Creating dot summary plot of fine-mapping results.
#> Melting PP and CS from 5 fine-mapping methods.
```
