# Add vertical lines

Adds vertical lines indicates key SNPs

## Usage

``` r
add_multitrack_lines(
  TRKS,
  dat,
  snp_groups = c("Lead", "UCS", "Consensus"),
  line_alpha = 1,
  line_size = 0.3,
  remove_duplicated_UCS_Consensus = TRUE,
  verbose = FALSE
)
```

## See also

Other plot:
[`dot_summary_plot()`](https://rajlabmssm.github.io/echoplot/reference/dot_summary_plot.md),
[`get_window_suffix()`](https://rajlabmssm.github.io/echoplot/reference/get_window_suffix.md)
