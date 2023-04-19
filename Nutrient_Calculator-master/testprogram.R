library(testthat)

test_that("No errors in program",
          {
            expect_no_error()
          }
          )
test_that("test package",
  {
    test_package()
  }
)