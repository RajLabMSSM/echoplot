test_that("save_tracks saves combined list by default", {

    locus_dir <- file.path(tempdir(), "test_save_tracks_combined")
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    gg <- ggplot2::ggplot()
    TRKS <- list("GWAS" = gg, "Fine-mapping" = gg)
    res <- echoplot:::save_tracks(
        locus_dir = locus_dir,
        TRKS_zoom = TRKS,
        window_suffix = "500kb",
        LD_reference = "UKB",
        split_tracks = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(file.exists(res))
    testthat::expect_true(grepl("\\.RDS$", res))
    loaded <- readRDS(res)
    testthat::expect_true(is.list(loaded))
    testthat::expect_length(loaded, 2)
})

test_that("save_tracks saves individual tracks when split", {

    locus_dir <- file.path(tempdir(), "test_save_tracks_split")
    on.exit(unlink(locus_dir, recursive = TRUE), add = TRUE)

    gg <- ggplot2::ggplot()
    TRKS <- list("GWAS" = gg, "Genes" = gg)
    res <- echoplot:::save_tracks(
        locus_dir = locus_dir,
        TRKS_zoom = TRKS,
        window_suffix = "500kb",
        LD_reference = "UKB",
        split_tracks = TRUE,
        verbose = FALSE
    )
    testthat::expect_length(res, 2)
    ## Check that the RDS files were actually created
    tracks_folder <- file.path(locus_dir, "plot_tracks")
    rds_files <- list.files(tracks_folder, pattern = "\\.RDS$")
    testthat::expect_true(length(rds_files) >= 2)
})
