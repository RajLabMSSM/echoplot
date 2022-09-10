# echoplot 0.99.5

## New features

* `plot_locus_multi`: New function to plot multi-GWAS/multi-ancestry results.
* `get_transcripts_biotypes`: New exported function to help users
    determine available biotypes.
* `get_transcripts`: new argument `tx_biotypes=` allows user to filter by 
    biotypes. Passes up to `plot_locus`.
* `plot_locus`: changed `qtl_prefixes` --> `qtl_suffixes` to be consistent with 
    `merge` naming strategy. 


# echoplot 0.99.4

## New features

* Replace "PLOT::" with "echoplot::"
* Add Issues templates. 

# echoplot 0.99.3

## Bug fixes 

* Fix `slice_max` bug in `transcript_model_track`.

# echoplot 0.99.2

## Bug fixes 

* Removed `echotabix` as dep since these functions
    are now handled by `echoannot`.
* Updated GHA workflow.
* Nott2019 tracks working again. 


# echoplot 0.99.0

* Added a `NEWS.md` file to track changes to the package.
