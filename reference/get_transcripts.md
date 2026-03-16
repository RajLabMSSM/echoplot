# Get transcripts

Get transcript positions for all genes in a range.

## Usage

``` r
get_transcripts(
  gr.snp,
  max_transcripts = 1,
  remove_pseudogenes = TRUE,
  tx_biotypes = NULL,
  verbose = TRUE
)
```

## Source

` #### Alternative approaches I've tried #### dat <- echodata::LRRK2 gr.snp <- echodata::dt_to_granges(dat = dat) # Warning:: MUST load the full package bc # it loads other necessary packages into the namespace. BiocManager::install("Homo.sapiens") library(Homo.sapiens) txdb <- Homo.sapiens::Homo.sapiens GenomicFeatures::genes(txdb) BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene") library(TxDb.Hsapiens.UCSC.hg19.knownGene) txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene GenomicFeatures::genes(txdb) `

## Arguments

- max_transcripts:

  Maximum number of transcripts per gene.

- tx_biotypes:

  Transcript biotypes to include in the gene model track. By default
  (`NULL`), all transcript biotypes will be included. See
  [get_tx_biotypes](https://rajlabmssm.github.io/echoplot/reference/get_tx_biotypes.md)
  for a full list of all available biotypes

- verbose:

  Print messages.
