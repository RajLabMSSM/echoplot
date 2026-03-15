test_that("formula_terms parses simple formula", {

    res <- echoplot:::formula_terms("Method~.")
    testthat::expect_true("Method" %in% res)
    testthat::expect_false("." %in% res)
    testthat::expect_false("~" %in% res)
})

test_that("formula_terms parses complex formula with +", {

    res <- echoplot:::formula_terms("condition + Method~.")
    testthat::expect_true("condition" %in% res)
    testthat::expect_true("Method" %in% res)
    testthat::expect_length(res, 2)
})

test_that("formula_terms handles single term", {

    res <- echoplot:::formula_terms("Method")
    testthat::expect_equal(res, "Method")
})

test_that("formula_terms handles formula with *", {

    res <- echoplot:::formula_terms("A * B ~ .")
    testthat::expect_true("A" %in% res)
    testthat::expect_true("B" %in% res)
})

test_that("formula_terms removes whitespace", {

    res <- echoplot:::formula_terms("  Method  ~  .  ")
    testthat::expect_true("Method" %in% res)
    testthat::expect_true(all(nchar(res) > 0))
})
