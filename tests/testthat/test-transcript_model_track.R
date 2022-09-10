test_that("transcript_model_track works", {
    
    #### When there's no overlap ####   
    dat <- echodata::BST1
    gene_track <- echoplot::transcript_model_track(dat=dat)
    testthat::expect_true(methods::is(gene_track,"gg"))
    
    #### When there's no overlap ####
    dat <- echodata::MEX3C[100:101,]
    gene_track2 <- echoplot::transcript_model_track(dat=dat)
    testthat::expect_null(gene_track2)
})
