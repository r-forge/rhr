context("rhrMCP")

test_that("rhr MCP fails with wrong input", {
  
  require(rhr)
  data(datSH)

  ## Tests input
  expect_that(rhrMCP(datSH), throws_error())
  expect_that(rhrMCP(datSH[, 1:3]), throws_error())
  expect_that(rhrMCP(datSH[1:2, ]), throws_error())
  expect_that(rhrMCP(datSH[, 1:2], levels=c("abc", 1, 2, 3)), throws_error())
  expect_that(rhrMCP(datSH[, 2:3], levels=c("abc", 1, 2, 3)), throws_error())
  expect_that(rhrMCP(datSH[, 2:3], levels=c(0, 102)), throws_error())

  expect_that(rhrMCP(datSH[, 2:4]), gives_warning())

  expect_that(rhrMCP(datSH[, 2:3]), is_a("RhrHREstimator"))


})
