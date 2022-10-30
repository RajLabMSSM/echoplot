test_that("get_tx_biotypes works", {
 
    #### WithOUT supplied tx_biotypes #### 
    tx_filter <- get_tx_biotypes()
    testthat::expect_true(methods::is(tx_filter,"TxBiotypeFilter"))
    testthat::expect_length(tx_filter@value, 39)
    
    #### With supplied tx_biotypes ####
    tx_biotypes <- c("protein_coding","typoooo")
    tx_filter2 <- get_tx_biotypes(tx_biotypes = tx_biotypes, 
                                           as_filter = FALSE)
    testthat::expect_equal(tx_biotypes[[1]],tx_filter2)
})
