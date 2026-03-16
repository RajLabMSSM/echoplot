# Get palettes

Get a unique set of palettes from pals, ordered by preference.

## Usage

``` r
get_palettes(
  preferred = c("brewer.spectral", "brewer.brbg", "brewer.piyg", "brewer.puor"),
  n_pals = NULL,
  n = 8,
  names_only = FALSE
)
```

## Arguments

- preferred:

  Preferred palette names. Will be moved to the top of the list.

- n_pals:

  Max number of palettes to return.

- n:

  Max number of colors to return per palette. Only applicable when
  `names_only=FALSE`.

- names_only:

  Only return the names of the palettes (`TRUE`), rather than a named
  list of colors for each palette (default: `TRUE`).

## Value

Vector of palette names.

## Examples

``` r
palettes <- get_palettes()
```
