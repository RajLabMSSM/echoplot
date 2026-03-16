# Plot annotation peaks: XGR

Import annotation from *XGR*, filter to only those within the range of
`dat`, and then plot the peaks. If the annotation has already been
downloaded previously, it will be reused.

## Usage

``` r
XGR_plot(
  dat,
  lib_name = "ENCODE_TFBS_ClusteredV3_CellTypes",
  locus_dir = tempdir(),
  palette = get_palettes(n_pals = 1, names_only = TRUE),
  fill_var = "Assay",
  facet_var = "Source",
  geom = "density",
  n_top = 5,
  adjust = 0.2,
  force_new = FALSE,
  show_plot = FALSE,
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- dat:

  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) with
  at least the following columns:

  SNP

  :   SNP RSID

  CHR

  :   chromosome

  POS

  :   position

- lib_name:

  Which XGR annotations to check overlap with. For full list of
  libraries see [here (XGR on
  CRAN).](https://cran.r-project.org/package=XGR) Passed to the
  `RData.customised` argument in `XGR::xRDataLoader`.

- locus_dir:

  Locus-specific directory.

- palette:

  Any palette available in pals. See
  [get_palettes](https://rajlabmssm.github.io/echoplot/reference/get_palettes.md)
  for a list of all palettes.

- fill_var:

  Fill variable.

- facet_var:

  Row facet variable.

- geom:

  Plot type ("density", or "histogram").

- n_top:

  Number of top annotations to be plotted (passed to
  [XGR_filter_sources](https://rdrr.io/pkg/echoannot/man/XGR_filter_sources.html)
  and then
  [XGR_filter_assays](https://rdrr.io/pkg/echoannot/man/XGR_filter_assays.html)).

- adjust:

  The granularity of the peaks.

- force_new:

  Download and prepare a new query even if the file already exists
  locally (Default: `FALSE`).

- show_plot:

  Print the plot.

- nThread:

  Number of threads to parallelise downloading annotations over.

- verbose:

  Print messages.

## Value

List with the "data" and the "plot".

## Examples

``` r
if (FALSE) { # \dontrun{
xgr_out <- echoplot::XGR_plot(dat = echodata::BST1[seq_len(1000),])
} # }
```
