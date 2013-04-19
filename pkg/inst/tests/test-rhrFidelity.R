context("rhrFidelity checks")

test_that("dat is a data.frame", {
          expect_that(rhrFidelity(list(1:10)), throws_error("must be a data.frame"))
})
