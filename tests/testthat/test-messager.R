test_that("messager works", {
    
    msg <- "Hello echoverse"
    msg_out <- utils::capture.output(messager(msg),
                                     type = "message")
    testthat::expect_equal(msg, msg_out)
})
