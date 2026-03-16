# List namespace

List all objects (e.g. functions, data) in the namespace one or more R
packages.

## Usage

``` r
list_namespace(packages, status_filter = NULL, verbose = TRUE)
```

## Arguments

- packages:

  A character vector of package names.

- status_filter:

  Filter by namespace status (e.g. "export","import"). If `NULL`
  (default), will return all namespace objects.

- verbose:

  Print messages.

## Value

A [data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

## Examples

``` r
nm_dt <- list_namespace(packages=c("data.table","pals"))
#> Querying 2 packages.
#> Gathering functions from: data.table
#> Gathering functions from: pals
```
