test_that("ROADMAP_plot works", {
    
    dat <- echodata::BST1[seq_len(1000), ]
    roadmap_out <- ROADMAP_plot(
        dat = dat,
        roadmap_query = "monocyte")
    testthat::expect_length(roadmap_out, 2)
    testthat::expect_true(methods::is(roadmap_out$data, "GRanges"))
    testthat::expect_true(methods::is(roadmap_out$plot, "gg"))
})
