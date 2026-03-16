# Get Started

``` r

library(echoplot)
```

## Plotting loci with *echoplot*

*echoplot* contains various functions that can be used separately from
the comprehensive `echolocatoR::finemap_loci()` pipeline.

Generate a multi-view plot of a given locus using
[`echoplot::plot_locus()`](https://rajlabmssm.github.io/echoplot/reference/plot_locus.md).

- You can mix and match different tracks and annotations using the
  different arguments (see
  [`?plot_locus`](https://rajlabmssm.github.io/echoplot/reference/plot_locus.md)
  for details).

The plot is centred on the lead/index SNP. If a list is supplied to
`zoom`, each zoom view will be saved individually.

- [`plot_locus()`](https://rajlabmssm.github.io/echoplot/reference/plot_locus.md)
  returns a series of `ggplot` objects bound together with
  [`patchwork`](https://patchwork.data-imaginist.com). One can further
  modify this object using `ggplot2` functions like `+ theme()`.
  - The modifications will be applied to all tracks at once.
- Save a high-resolution version of the plot by setting
  `save_plot=TRUE`.
  - Further increase resolution by adjusting the `dpi` argument
    (*default=300*).
  - Files are saved in *jpg* format by default, but users can specify
    their preferred file format (e.g. `file_format="png"`).
  - Adjust the `height` and `width` of the saved plot using these
    respective arguments.
  - The plot will be automatically saved in the locus-specific directory
    as: \*multiview\_\<locus\>\_\<zoom\>.jpg\*.

### Load example data

Load example dataset of the results from fine-mapping the BST1 locus
with `finemap_loci()`. Original data comes from the recent Nalls et
al. (2019) Parkinson’s disease GWAS (see `?BST1` for details).

``` r

library(ggplot2)
library(patchwork)

root.dir <- tempdir()
locus_dir <- file.path(root.dir, echodata::locus_dir)
dat <- echodata::BST1
LD_matrix <- echodata::BST1_LD_matrix
LD_reference <- "UKB" # Used for naming saved plots
zoom <- "10x"
show_plot <- FALSE
```

### Full window

``` r

trk_plot <- echoplot::plot_locus(dat = dat,
                                 LD_matrix = LD_matrix,
                                 LD_reference = LD_reference,
                                 locus_dir = locus_dir,
                                 save_plot = FALSE,
                                 show_plot = show_plot,
                                 zoom = zoom)
```

``` r

methods::show(trk_plot)
```

### At multiple zooms

- You can easily generate the same locus plot at multiple zoomed in
  views by supplying a list to `zoom`.
- This list can be composed of zoom multipliers (e.g. `c("1x", "2x")`),
  window widths in units of basepairs (e.g. `c(5000, 1500)`), or a
  mixture of both (e.g. `c("1x","4x", 5000, 2000)`).
- Each zoom view will be saved individually with its respective scale as
  the suffix (e.g. `multiview.BST1.UKB.4x.jpg`).
- Each zoom view is stored as a named item within the returned list.

``` r

trk_zooms <- echoplot::plot_locus(dat = dat,
                                  LD_matrix = LD_matrix,
                                  LD_reference = LD_reference,
                                  locus_dir = locus_dir,
                                  save_plot = FALSE,
                                  show_plot = show_plot,
                                  zoom = c("1x", "5x", "10x"))
names(trk_zooms) # Get zoom view names
```

``` r

methods::show(trk_zooms)
```

### Return as list

- For even further control over each track of the multi-view plot,
  specify `plot_locus(..., return_list=TRUE)` to instead return a named
  list (nested within each zoom view list item) of `ggplot` objects
  which can each be modified individually.
- Once you’ve made your modifications, you can then bind this list of
  plots back together with
  `patchwork::wrap_plots(tracks_list, ncol = 1)`.

``` r

trk_plot_list <- echoplot::plot_locus(dat = dat,
                                      LD_matrix = LD_matrix,
                                      LD_reference = LD_reference,
                                      locus_dir = locus_dir,
                                      save_plot = FALSE,
                                      show_plot = show_plot,
                                      zoom = zoom,
                                      return_list = TRUE)
```

``` r

view1_list <- trk_plot_list[[zoom]]
names(view1_list) # Get track names from a particular zoom view
```

Modify a specific track within a view.

``` r

# Modify your selected track
modified_track <- view1_list$GWAS +
                      ggplot2::labs(title = "Modified GWAS") +
                      ggplot2::theme_dark() +
                      ggplot2::theme(title = ggplot2::element_text(hjust = .5))
# Put it back into your track list
view1_list[["GWAS"]] <- modified_track
# Remove a plot you don't want
view1_list[["Genes"]] <- NULL
# Specify the relative heights of each track
track_heights <- c(.3, .1, .3, 1)

# Bind them together and plot
fused_plot <- patchwork::wrap_plots(view1_list,
                                    heights = track_heights,
                                    ncol = 1)
methods::show(fused_plot)
```

### Using XGR annotations

- Whenever you use annotation arguments (e.g. `xgr_libnames`, `Roadmap`,
  `nott_epigenome`) the annotations that overlap with your locus will
  automatically be saved as `GRanges` objects in a locus-specific
  subdirectory:
  *results/\<dataset_type\>/\<dataset_name\>/\<locus\>/annotation*
- If a selected annotation has previously been downloaded and stored for
  that locus,
  [`plot_locus()`](https://rajlabmssm.github.io/echoplot/reference/plot_locus.md)
  will automatically detect and import it to save time.

``` r

trk_plot.xgr <- echoplot::plot_locus(dat = dat,
                                     LD_matrix = LD_matrix,
                                     LD_reference = LD_reference,
                                     locus_dir = locus_dir,
                                     xgr_libnames = c("ENCODE_TFBS_ClusteredV3_CellTypes"),
                                     save_plot = FALSE,
                                     show_plot = show_plot,
                                     zoom = zoom)
```

``` r

methods::show(trk_plot.xgr)
```

### Using Roadmap annotations

- Using the `Roadmap=TRUE` and `roadmap_query="<query>"` arguments
  searches the
  [Roadmap](http://www.roadmapepigenomics.org/data/tables/brain) for
  chromatin mark data across various cell-types, cell-lines and tissues.
- Note that Roadmap queries require `tabix` to be installed on your
  machine, or within a conda environment (`conda_env = "echoR"`).
- Parallelising these queries across multiple threads speeds up this
  process (`nThread=<n_cores_available>`), as does reusing previously
  stored data which is automatically saved to the locus-specific
  subfolder.

*NOTE*: Querying remote genomic data sources (e.g. Roadmap bigwigs) is
not currently supported on Windows due to known bugs in
[`rtracklayer`](https://github.com/lawremi/rtracklayer/issues).

``` r

if (.Platform$OS.type != "windows") {
    trk_plot.roadmap <- echoplot::plot_locus(dat = dat,
                                             LD_matrix = LD_matrix,
                                             LD_reference = LD_reference,
                                             locus_dir = locus_dir,
                                             roadmap = TRUE,
                                             roadmap_query = "monocyte",
                                             save_plot = FALSE,
                                             show_plot = show_plot,
                                             zoom = "5x")
}
```

``` r

methods::show(trk_plot.roadmap)
```

### Using Nott 2019 annotations

- Query and plot brain cell type-specific epigenomic assays from [Nott
  et al. (Science,
  2019)](https://science.sciencemag.org/content/366/6469/1134.abstract)
  (see `?NOTT_2019.bigwig_metadata` for details).

``` r

trk_plot.nott_2019 <- echoplot::plot_locus(dat = dat,
                                           LD_matrix = LD_matrix,
                                           LD_reference = LD_reference,
                                           locus_dir = locus_dir,
                                           nott_epigenome = TRUE,
                                           nott_binwidth = 200,
                                           nott_regulatory_rects = TRUE,
                                           nott_show_placseq = TRUE,
                                           save_plot = FALSE,
                                           show_plot = show_plot,
                                           zoom = zoom)
```

``` r

methods::show(trk_plot.nott_2019)
```

### Using QTL datasets

- Plot multiple QTL p-value columns (or really P-value columns from any
  kind of dataset).
- Each QTL dataset will be plotted as a new track.

``` r

dat1 <- data.table::copy(dat)
dat2 <- data.table::copy(dat)
# Make fake QTL P-values for the sake of demonstration
dat1$P <- abs(jitter(dat1$P, amount = 1e-15))
dat2$P <- abs(jitter(dat2$P, amount = 1e-16))
dat_ls <- list("fake_eQTL" = dat1,
               "fake_sQTL" = dat2)

trk_plot.qtl <- echoplot::plot_locus_multi(dat_ls = dat_ls,
                                           LD_ls = list(LD_matrix, LD_matrix),
                                           locus_dir = locus_dir,
                                           show_plot = show_plot,
                                           zoom = "10x")
```

``` r

methods::show(trk_plot.qtl)
```

------------------------------------------------------------------------

## Session info

``` r

utils::sessionInfo()
```

```
## R Under development (unstable) (2026-03-12 r89607)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 24.04.4 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## time zone: UTC
## tzcode source: system (glibc)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] patchwork_1.3.2  ggplot2_4.0.2    echoplot_1.0.0   BiocStyle_2.39.0
## 
## loaded via a namespace (and not attached):
##   [1] splines_4.6.0               aws.s3_0.3.22              
##   [3] BiocIO_1.21.0               bitops_1.0-9               
##   [5] filelock_1.0.3              tibble_3.3.1               
##   [7] R.oo_1.27.1                 cellranger_1.1.0           
##   [9] basilisk.utils_1.23.1       graph_1.89.1               
##  [11] XML_3.99-0.22               rpart_4.1.24               
##  [13] lifecycle_1.0.5             pals_1.10                  
##  [15] OrganismDbi_1.53.2          ensembldb_2.35.0           
##  [17] lattice_0.22-9              MASS_7.3-65                
##  [19] backports_1.5.0             magrittr_2.0.4             
##  [21] openxlsx_4.2.8.1            Hmisc_5.2-5                
##  [23] sass_0.4.10                 rmarkdown_2.30             
##  [25] jquerylib_0.1.4             yaml_2.3.12                
##  [27] otel_0.2.0                  zip_2.3.3                  
##  [29] reticulate_1.45.0           ggbio_1.59.0               
##  [31] gld_2.6.8                   mapproj_1.2.12             
##  [33] DBI_1.3.0                   RColorBrewer_1.1-3         
##  [35] maps_3.4.3                  abind_1.4-8                
##  [37] expm_1.0-0                  GenomicRanges_1.63.1       
##  [39] purrr_1.2.1                 R.utils_2.13.0             
##  [41] biovizBase_1.59.0           AnnotationFilter_1.35.0    
##  [43] BiocGenerics_0.57.0         RCurl_1.98-1.17            
##  [45] nnet_7.3-20                 VariantAnnotation_1.57.1   
##  [47] IRanges_2.45.0              S4Vectors_0.49.0           
##  [49] echoLD_1.0.0                pkgdown_2.2.0              
##  [51] echodata_1.0.0              piggyback_0.1.5            
##  [53] codetools_0.2-20            DelayedArray_0.37.0        
##  [55] DT_0.34.0                   xml2_1.5.2                 
##  [57] tidyselect_1.2.1            UCSC.utils_1.7.1           
##  [59] farver_2.1.2                matrixStats_1.5.0          
##  [61] stats4_4.6.0                base64enc_0.1-6            
##  [63] Seqinfo_1.1.0               echotabix_1.0.1            
##  [65] GenomicAlignments_1.47.0    jsonlite_2.0.0             
##  [67] e1071_1.7-17                Formula_1.2-5              
##  [69] survival_3.8-6              systemfonts_1.3.2          
##  [71] ggnewscale_0.5.2            tools_4.6.0                
##  [73] ragg_1.5.1                  DescTools_0.99.60          
##  [75] Rcpp_1.1.1                  glue_1.8.0                 
##  [77] gridExtra_2.3               SparseArray_1.11.11        
##  [79] xfun_0.56                   MatrixGenerics_1.23.0      
##  [81] GenomeInfoDb_1.47.2         dplyr_1.2.0                
##  [83] withr_3.0.2                 BiocManager_1.30.27        
##  [85] fastmap_1.2.0               basilisk_1.23.0            
##  [87] boot_1.3-32                 digest_0.6.39              
##  [89] R6_2.6.1                    colorspace_2.1-2           
##  [91] textshaping_1.0.5           dichromat_2.0-0.1          
##  [93] RSQLite_2.4.6               cigarillo_1.1.0            
##  [95] R.methodsS3_1.8.2           tidyr_1.3.2                
##  [97] generics_0.1.4              data.table_1.18.2.1        
##  [99] rtracklayer_1.71.3          class_7.3-23               
## [101] httr_1.4.8                  htmlwidgets_1.6.4          
## [103] S4Arrays_1.11.1             pkgconfig_2.0.3            
## [105] gtable_0.3.6                Exact_3.3                  
## [107] blob_1.3.0                  S7_0.2.1                   
## [109] XVector_0.51.0              echoconda_1.0.0            
## [111] htmltools_0.5.9             bookdown_0.46              
## [113] RBGL_1.87.0                 ProtGenerics_1.43.0        
## [115] scales_1.4.0                Biobase_2.71.0             
## [117] lmom_3.2                    png_0.1-8                  
## [119] knitr_1.51                  rstudioapi_0.18.0          
## [121] tzdb_0.5.0                  reshape2_1.4.5             
## [123] rjson_0.2.23                checkmate_2.3.4            
## [125] curl_7.0.0                  proxy_0.4-29               
## [127] cachem_1.1.0                stringr_1.6.0              
## [129] rootSolve_1.8.2.4           parallel_4.6.0             
## [131] foreign_0.8-91              AnnotationDbi_1.73.0       
## [133] restfulr_0.0.16             desc_1.4.3                 
## [135] pillar_1.11.1               grid_4.6.0                 
## [137] vctrs_0.7.1                 cluster_2.1.8.2            
## [139] htmlTable_2.4.3             evaluate_1.0.5             
## [141] readr_2.2.0                 GenomicFeatures_1.63.1     
## [143] mvtnorm_1.3-5               cli_3.6.5                  
## [145] compiler_4.6.0              Rsamtools_2.27.1           
## [147] rlang_1.1.7                 crayon_1.5.3               
## [149] aws.signature_0.6.0         plyr_1.8.9                 
## [151] forcats_1.0.1               fs_1.6.7                   
## [153] stringi_1.8.7               echoannot_1.0.1            
## [155] BiocParallel_1.45.0         Biostrings_2.79.5          
## [157] lazyeval_0.2.2              Matrix_1.7-4               
## [159] downloadR_1.0.0             dir.expiry_1.19.0          
## [161] BSgenome_1.79.1             hms_1.1.4                  
## [163] bit64_4.6.0-1               KEGGREST_1.51.1            
## [165] SummarizedExperiment_1.41.1 haven_2.5.5                
## [167] memoise_2.0.1               snpStats_1.61.0            
## [169] bslib_0.10.0                bit_4.6.0                  
## [171] readxl_1.4.5
```

\
