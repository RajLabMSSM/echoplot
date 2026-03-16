# Get window suffix

Determine the plot suffix indicating its window size.

## Usage

``` r
get_window_suffix(dat, zoom, verbose = TRUE)
```

## Source

` dat <- echodata::BST1 window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom=1000) window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom=NULL) window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom="all") window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom="2x") `

## See also

Other plot:
[`add_multitrack_lines()`](https://rajlabmssm.github.io/echoplot/reference/add_multitrack_lines.md),
[`dot_summary_plot()`](https://rajlabmssm.github.io/echoplot/reference/dot_summary_plot.md)
