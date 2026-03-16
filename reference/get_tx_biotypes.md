# Get transcript biotypes

List all available transcript biotypes in the
[EnsDb.Hsapiens.v75](https://rdrr.io/pkg/EnsDb.Hsapiens.v75/man/package.html)
database.

## Usage

``` r
get_tx_biotypes(tx_biotypes = NULL, as_filter = TRUE, verbose = TRUE)
```

## Arguments

- tx_biotypes:

  Transcript biotypes to include in the gene model track. By default
  (`NULL`), all transcript biotypes will be included. See
  get_tx_biotypes for a full list of all available biotypes

- as_filter:

  Return results as an
  [TxBiotypeFilter](https://rdrr.io/pkg/AnnotationFilter/man/AnnotationFilter.html)
  (default: `TRUE`). Otherwise, a character vector will be returned.

- verbose:

  Print messages.

## Value

Character vector of valid transcript biotypes.

## Examples

``` r
tx_filter <- get_tx_biotypes()
```
