test_that("guess_pvalue_col finds standard P column", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, P = 0.05)
    res <- echoplot:::guess_pvalue_col(dat = dat)
    testthat::expect_true("P" %in% res)
})

test_that("guess_pvalue_col finds lowercase p column", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, p = 0.05)
    res <- echoplot:::guess_pvalue_col(dat = dat)
    testthat::expect_true("p" %in% res)
})

test_that("guess_pvalue_col finds pvalue column", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, pvalue = 0.05)
    res <- echoplot:::guess_pvalue_col(dat = dat)
    testthat::expect_true("pvalue" %in% res)
})

test_that("guess_pvalue_col finds Pvalue column", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, Pvalue = 0.05)
    res <- echoplot:::guess_pvalue_col(dat = dat)
    testthat::expect_true("Pvalue" %in% res)
})

test_that("guess_pvalue_col returns empty when no match", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, zscore = 2.5)
    res <- echoplot:::guess_pvalue_col(dat = dat)
    testthat::expect_length(res, 0)
})

test_that("guess_pvalue_col handles qtl_suffix", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, P.eQTL = 0.05)
    res <- echoplot:::guess_pvalue_col(dat = dat, qtl_suffix = ".eQTL")
    testthat::expect_true("P.eQTL" %in% res)
})

test_that("guess_pvalue_col handles NULL suffix", {

    dat <- data.frame(SNP = "rs1", CHR = 1, POS = 100, P = 0.05)
    res <- echoplot:::guess_pvalue_col(dat = dat, qtl_suffix = NULL)
    testthat::expect_true("P" %in% res)
})
