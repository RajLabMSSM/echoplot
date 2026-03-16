# Plot annotation peaks: ROADMAP

Import annotation from *ROADMAP*, filter to only those within the range
of `dat`, and then plot the peaks. If the annotation has already been
downloaded previously, it will be reused.

## Usage

``` r
ROADMAP_plot(
  dat,
  roadmap_query,
  lib_name = "Roadmap.ChromatinMarks_CellTypes",
  locus_dir = tempdir(),
  limit_files = NULL,
  n_top = 5,
  adjust = 0.2,
  force_new = FALSE,
  show_plot = FALSE,
  conda_env = "echoR_mini",
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

- roadmap_query:

  Search all columns in the Roadmap annotations metadata and only query
  annotations that contain your keywords. Can provide multiple keywords
  in list form: `c("placenta","liver","monocytes")`

- lib_name:

  Name of the data library to use.

- locus_dir:

  Locus-specific directory.

- limit_files:

  Limit the number of annotation files queried (for faster testing).

- n_top:

  Number of top annotations to be plotted (passed to
  [ROADMAP_query](https://rdrr.io/pkg/echoannot/man/ROADMAP_query.html)).

- adjust:

  The granularity of the peaks.

- force_new:

  Download and prepare a new query even if the file already exists
  locally (Default: `FALSE`).

- show_plot:

  Print the plot.

- conda_env:

  Conda environment to search for tabix in.

- nThread:

  Number of threads to parallelise downloading annotations over.

- verbose:

  Print messages.

## Value

A named list containing:

- "data":

  `GRanges` object within the queried coordinates.

- "plot":

  `ggbio` plot.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1[seq_len(1000),]
roadmap_out <- echoplot::ROADMAP_plot(
    dat = dat,
    roadmap_query = "monocyte")
} # }
```
