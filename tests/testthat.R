desc_path <- list.files("../","^DESCRIPTION$",
                        full.names = TRUE, recursive = TRUE)[1]
pkg <- read.dcf(desc_path, fields = "Package")[1]
library(testthat)
library(pkg, character.only = TRUE)

test_check(pkg)
