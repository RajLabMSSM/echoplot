test_that("merge_datasets merges two valid datasets", {

    dat1 <- data.frame(
        CHR = c(1, 1), POS = c(100, 200),
        SNP = c("rs1", "rs2"), P = c(0.01, 0.05)
    )
    dat2 <- data.frame(
        CHR = c(1, 1), POS = c(100, 200),
        SNP = c("rs1", "rs2"), P = c(0.02, 0.06)
    )
    dat_ls <- list(gwas1 = dat1, gwas2 = dat2)
    res <- echoplot:::merge_datasets(dat_ls = dat_ls, verbose = FALSE)
    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_equal(nrow(res), 2)
    ## Merged P columns get suffixes
    testthat::expect_true("P.1" %in% names(res))
    testthat::expect_true("P.2" %in% names(res))
})

test_that("merge_datasets errors on missing required columns", {

    dat1 <- data.frame(CHR = 1, POS = 100, SNP = "rs1", P = 0.01)
    dat_bad <- data.frame(CHR = 1, POS = 100, SNP = "rs1")
    dat_ls <- list(good = dat1, bad = dat_bad)
    testthat::expect_error(
        echoplot:::merge_datasets(dat_ls = dat_ls, verbose = FALSE),
        "missing the following required"
    )
})

test_that("merge_datasets preserves extra columns", {

    dat1 <- data.frame(
        CHR = 1, POS = 100, SNP = "rs1", P = 0.01, Effect = 0.5
    )
    dat2 <- data.frame(
        CHR = 1, POS = 100, SNP = "rs1", P = 0.02, Effect = 0.3
    )
    dat_ls <- list(a = dat1, b = dat2)
    res <- echoplot:::merge_datasets(dat_ls = dat_ls, verbose = FALSE)
    testthat::expect_true("Effect.1" %in% names(res))
    testthat::expect_true("Effect.2" %in% names(res))
})

test_that("merge_datasets handles single-row datasets", {

    dat1 <- data.frame(CHR = 1, POS = 100, SNP = "rs1", P = 0.01)
    dat2 <- data.frame(CHR = 1, POS = 100, SNP = "rs1", P = 0.02)
    dat_ls <- list(a = dat1, b = dat2)
    res <- echoplot:::merge_datasets(dat_ls = dat_ls, verbose = FALSE)
    testthat::expect_equal(nrow(res), 1)
})
