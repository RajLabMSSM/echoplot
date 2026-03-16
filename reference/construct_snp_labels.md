# Construct SNP labels Construct SNP labels for Manhattan plot. Support function for [plot_locus](https://rajlabmssm.github.io/echoplot/reference/plot_locus.md).

Construct SNP labels

Construct SNP labels for Manhattan plot. Support function for
[plot_locus](https://rajlabmssm.github.io/echoplot/reference/plot_locus.md).

## Usage

``` r
construct_snp_labels(
  dat,
  labels_subset = c("Lead", "CS", "Consensus"),
  mean_only_text = c("Consensus", "UCS"),
  method_specific_consensus = TRUE,
  remove_duplicates = TRUE,
  grouping_vars = c("SNP"),
  merge_with_input = FALSE,
  base_size = 5,
  verbose = FALSE
)
```

## Arguments

- dat:

  Data to query transcripts with.

- labels_subset:

  Include colored shapes and RSID labels to help highlight SNPs
  belonging to one or more of the following groups: Lead, Credible Set,
  Consensus.

- mean_only_text:

  Only show the text labels with RSIDs in the "mean" Method (not all the
  other method-specific facets).

- method_specific_consensus:

  Only label Consensus SNPs that are also Credible Set SNPs for a given
  method.

- verbose:

  Print messages.
