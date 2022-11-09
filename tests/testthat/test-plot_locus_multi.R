test_that("plot_locus_multi works", {
  
    locus_dir <- file.path(tempdir(),echodata::locus_dir)
    #### Make dat_ls ####
    dat <- echodata::filter_snps(echodata::BST1, bp_distance = 10000) 
    dat_ls <- list(gwas1=dat, gwas2=dat)
    #### Make LD_ls ####
    LD_matrix <- echodata::BST1_LD_matrix
    LD_ls <- list(ancestry1=LD_matrix, ancestry2=LD_matrix)
    #### Make plot #### 
    plot_list <- plot_locus_multi(dat_ls = dat_ls,
                                  LD_ls = LD_ls,
                                  locus_dir = locus_dir, 
                                  show_plot = FALSE,
                                  return_list = TRUE)
    testthat::expect_true(names(plot_list)=="1x")
    testthat::expect_equal(names(plot_list$`1x`),
                           c("Genes","GWAS","Fine-mapping"))
    for(x in plot_list$`1x`){
        testthat::expect_true(methods::is(x,"gg"))    
    } 
})
