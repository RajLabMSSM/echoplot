test_that("get_palettes works", {
  
    #### Return colors ####
    palettes <- get_palettes()
    testthat::expect_length(palettes, 127)
    testthat::expect_true(
        all(
            unlist(lapply(palettes, length))==formals(echoplot::get_palettes)$n
        )
    )
    #### Return names ####
    palettes2 <- get_palettes(n_pals = 10, names_only = TRUE)
    testthat::expect_length(palettes2, 10)
    testthat::expect_true(
        all(
            unlist(lapply(palettes2, length))==1
        )
    )
})
