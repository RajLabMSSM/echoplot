# echoplot 0.99.6

## New features

* New exports:
    - `get_palettes`
    - `list_namespace`
* Implement `rworkflows`
* `plot_locus`:
    - pass up `subset_labels` arg.

## Bug fixes

* Make `pals` an Import, and remove `RColorBrewer`.
* Fix `Can't combine ..1$Method <factor<2f980>> and ..4$Method <ordered<4e1b3>>.` error.
* Get rid of `ggrepel: 7 unlabeled data points (too many overlaps). Consider increasing max.overlaps` 
    warnings.
* Fixed potential issue with getting top SNPs per Method in `construct_snp_labels`.
* Fixed `colours encodes as numbers must be positive` errors.

# echoplot 0.99.5

## New features

* `plot_locus_multi`: New function to plot multi-GWAS/multi-ancestry results.
* `get_transcripts_biotypes`: New exported function to help users
    determine available biotypes.
* `get_transcripts`: new argument `tx_biotypes=` allows user to filter by 
    biotypes. Passes up to `plot_locus`.
* `plot_locus`: changed `qtl_prefixes` --> `qtl_suffixes` to be consistent with 
    `merge` naming strategy. 
* Export `snp_track_merged`. 
* Move functions from `echoplot`:
    - `ROADMAP_plot` and plotting subfunctions.  
    - `XGR_plot` and plotting subfunctions.  
    
## Bug fixes

* `transcript_model_track`: Handle when 0 transcripts are returned, without error.
* `get_transcripts_biotypes` ==> `get_tx_biotypes` in all places.
* Document all functions. 
* Fix GHA: @master --> @v2  

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
