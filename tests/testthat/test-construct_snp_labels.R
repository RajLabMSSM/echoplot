test_that("construct_snp_labels returns data.table with expected cols", {

    dat <- echodata::BST1[seq_len(200), ]
    res <- echoplot:::construct_snp_labels(
        dat = dat,
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    testthat::expect_true("type" %in% names(res))
    testthat::expect_true("color" %in% names(res))
    testthat::expect_true("shape" %in% names(res))
    testthat::expect_true("size" %in% names(res))
    testthat::expect_true("SNP" %in% names(res))
})

test_that("construct_snp_labels finds lead SNPs", {

    dat <- echodata::BST1
    res <- echoplot:::construct_snp_labels(
        dat = dat,
        labels_subset = c("Lead"),
        verbose = FALSE
    )
    testthat::expect_true(nrow(res) > 0)
    testthat::expect_true(all(res$type == "Lead"))
    testthat::expect_true(all(res$color == "red"))
})

test_that("construct_snp_labels handles CS labels", {

    dat <- echodata::BST1
    res <- echoplot:::construct_snp_labels(
        dat = dat,
        labels_subset = c("CS"),
        verbose = FALSE
    )
    ## CS column exists only after melt_finemapping_results
    testthat::expect_true(data.table::is.data.table(res))
})

test_that("construct_snp_labels handles Consensus labels", {

    dat <- echodata::BST1
    res <- echoplot:::construct_snp_labels(
        dat = dat,
        labels_subset = c("Consensus"),
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    if (nrow(res) > 0) {
        testthat::expect_true("Consensus" %in% levels(res$type))
    }
})

test_that("construct_snp_labels with merge_with_input returns full data", {

    dat <- echodata::BST1[seq_len(200), ]
    res <- echoplot:::construct_snp_labels(
        dat = dat,
        merge_with_input = TRUE,
        verbose = FALSE
    )
    testthat::expect_true(data.table::is.data.table(res))
    ## Should have all original rows (times methods)
    testthat::expect_true(nrow(res) >= 200)
    testthat::expect_true("text_label" %in% names(res))
})

test_that("construct_snp_labels remove_duplicates=FALSE keeps all", {

    dat <- echodata::BST1[seq_len(200), ]
    res_dedup <- echoplot:::construct_snp_labels(
        dat = dat,
        remove_duplicates = TRUE,
        verbose = FALSE
    )
    res_all <- echoplot:::construct_snp_labels(
        dat = dat,
        remove_duplicates = FALSE,
        verbose = FALSE
    )
    ## Non-deduplicated should have >= rows
    testthat::expect_true(nrow(res_all) >= nrow(res_dedup))
})
