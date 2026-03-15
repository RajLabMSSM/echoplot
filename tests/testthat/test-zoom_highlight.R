test_that("zoom_highlight returns a ggplot", {

    dat <- echodata::BST1
    dat <- echoannot::add_mb(dat)
    ## Build a base plot with Mb on x-axis
    gg <- ggplot2::ggplot(dat, ggplot2::aes(x = Mb, y = -log10(P))) +
        ggplot2::geom_point()
    zh <- echoplot:::zoom_highlight(
        gg = gg,
        dat = dat,
        zoom = "5x",
        verbose = FALSE
    )
    testthat::expect_true(methods::is(zh, "gg"))
    testthat::expect_true(methods::is(zh, "ggplot"))
})

test_that("zoom_highlight adds a geom_polygon layer", {

    dat <- echodata::BST1
    dat <- echoannot::add_mb(dat)
    gg <- ggplot2::ggplot(dat, ggplot2::aes(x = Mb, y = -log10(P))) +
        ggplot2::geom_point()
    n_layers_before <- length(gg$layers)
    zh <- echoplot:::zoom_highlight(
        gg = gg,
        dat = dat,
        zoom = "5x",
        verbose = FALSE
    )
    ## Should have one more layer (the polygon)
    testthat::expect_gt(length(zh$layers), n_layers_before)
})

test_that("zoom_highlight respects alpha parameter", {

    dat <- echodata::BST1
    dat <- echoannot::add_mb(dat)
    gg <- ggplot2::ggplot(dat, ggplot2::aes(x = Mb, y = -log10(P))) +
        ggplot2::geom_point()
    zh <- echoplot:::zoom_highlight(
        gg = gg,
        dat = dat,
        zoom = "3x",
        alpha = 0.5,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(zh, "gg"))
})
