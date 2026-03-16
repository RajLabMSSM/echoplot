# Plot transcripts

Plot a max number of transcripts per genes. See
[get_tx_biotypes](https://rajlabmssm.github.io/echoplot/reference/get_tx_biotypes.md)
for a full list of all available biotypes.

## Usage

``` r
transcript_model_track(
  dat,
  max_transcripts = 1,
  tx_biotypes = NULL,
  remove_pseudogenes = TRUE,
  show_plot = FALSE,
  show.legend = FALSE,
  fill = "skyblue",
  shape = c("arrow", "box", "ellipse", "smallArrow"),
  transcriptAnnotation = c("symbol", "transcript"),
  collapseTranscripts = c(FALSE, TRUE, "longest"),
  stacking = c("squish", "hide", "dense", "pack", "full"),
  method = "ggplot",
  xtext = TRUE,
  expand_x_mult = c(0.05, 0.1),
  verbose = TRUE
)
```

## Source

[ensembld
tutorial](https://bioconductor.org/packages/release/bioc/vignettes/ensembldb/inst/doc/ensembldb.html)
[Gvix
tutorial](https://bioconductor.org/packages/devel/bioc/vignettes/Gviz/inst/doc/Gviz.html#45_GeneRegionTrack)
[ggbio
tutorial](http://bioconductor.org/packages/devel/bioc/vignettes/ggbio/inst/doc/ggbio.pdf)

## Arguments

- dat:

  Data to query transcripts with.

- max_transcripts:

  Maximum number of transcripts per gene.

- tx_biotypes:

  Transcript biotypes to include in the gene model track. By default
  (`NULL`), all transcript biotypes will be included. See
  [get_tx_biotypes](https://rajlabmssm.github.io/echoplot/reference/get_tx_biotypes.md)
  for a full list of all available biotypes

- remove_pseudogenes:

  Whether to remove known pseudogenes.

- show_plot:

  Print plot to screen.

- show.legend:

  Show gene plot legend.

- fill:

  A variable name string, or a color to fill the plot data with.

- shape:

  Shape type to plot genes as. Passed to
  [plotTracks](https://rdrr.io/pkg/Gviz/man/plotTracks.html).

- transcriptAnnotation:

  Whether to show the gene symbol or the full transcript ID. Passed to
  [plotTracks](https://rdrr.io/pkg/Gviz/man/plotTracks.html).

- collapseTranscripts:

  Logical or character scalar. Can be one in gene, longest, shortest or
  meta. Merge all transcripts of the same gene into one single gene
  model. In the case of gene (or TRUE), this will only keep the start
  location of the first exon and the end location of the last exon from
  all transcripts of the gene. For shortest and longest, only the
  longest or shortest transcript model is retained. For meta, a
  meta-transcript containing the union of all exons is formed
  (essentially identical to the operation `reduce(geneModel)`). Passed
  to [plotTracks](https://rdrr.io/pkg/Gviz/man/plotTracks.html).

- stacking:

  Character. Stacking mode for the gene region track. One of `"squish"`,
  `"hide"`, `"dense"`, `"pack"`, or `"full"`. Passed to
  [GeneRegionTrack](https://rdrr.io/pkg/Gviz/man/GeneRegionTrack-class.html).

- method:

  Method to use: "gviz" or "ggplot".

- xtext:

  Include x-axis title and text.

- expand_x_mult:

  Expand the x-axis limits to include partially overlapping transcripts.

- verbose:

  Print messages.

## Examples

``` r
dat <- echodata::BST1 
gene_track <- echoplot::transcript_model_track(dat=dat)
#> Converting dat to GRanges object.
#> max_transcripts= 1 . 
#> 16  transcripts from  16  genes returned.
#> Fetching data...
#> OK
#> Parsing exons...
#> OK
#> Defining introns...
#> OK
#> Defining UTRs...
#> OK
#> Defining CDS...
#> OK
#> aggregating...
#> Done
#> Constructing graphics...
```
