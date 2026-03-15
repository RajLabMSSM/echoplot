test_that("heights_dict returns full dict with no keys", {

    res <- echoplot:::heights_dict()
    testthat::expect_true(is.numeric(res))
    testthat::expect_true(length(res) > 0)
    testthat::expect_true("GWAS" %in% names(res))
    testthat::expect_true("Fine-mapping" %in% names(res))
    testthat::expect_true("Genes" %in% names(res))
})

test_that("heights_dict returns matching heights for known keys", {

    res <- echoplot:::heights_dict(keys = c("GWAS", "Fine-mapping"))
    testthat::expect_length(res, 2)
    testthat::expect_equal(res[1], 0.33)
    testthat::expect_equal(res[2], 1)
})

test_that("heights_dict uses default_height for unknown keys", {

    res <- echoplot:::heights_dict(
        keys = c("GWAS", "Unknown Track"),
        default_height = 2
    )
    testthat::expect_length(res, 2)
    testthat::expect_equal(res[1], 0.33)
    testthat::expect_equal(res[2], 2)
})

test_that("heights_dict handles empty keys vector", {

    res <- echoplot:::heights_dict(keys = character(0))
    testthat::expect_length(res, 0)
})

test_that("heights_dict returns correct height for Genes", {

    res <- echoplot:::heights_dict(keys = "Genes")
    testthat::expect_equal(res, 0.5)
})
