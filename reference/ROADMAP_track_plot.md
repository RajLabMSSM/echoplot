# Plot ROADMAP query

Plot ROADMAP query

## Usage

``` r
ROADMAP_track_plot(
  grl.roadmap,
  gr.snp = NULL,
  geom = "density",
  adjust = 0.2,
  position = c("stack", "fill", "dodge"),
  show_plot = TRUE,
  as_ggplot = TRUE,
  verbose = TRUE
)
```

## Source

` gr.snp <- echodata::dt_to_granges(echodata::BST1) grl.roadmap <- echoannot::ROADMAP_query( query_dat = gr.snp, merge_and_process = TRUE, keyword_query = "monocyte") track.roadmap <- ROADMAP_track_plot(grl.roadmap, gr.snp = gr.snp) `

## Arguments

- grl.roadmap:

  ROADMAP query results.

- gr.snp:

  Optionally, can include an extra
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to ensure the plot does not extend beyond certain coordinates.

- geom:

  The type of plot to create. Options include "density" and "histogram".

- adjust:

  The granularity of the peaks.

- show_plot:

  Whether to print the plot.
