source("/Users/gevork/Documents/GitHub/MacroCalc704/Nutrient_Calculator-master/app.R", chdir = FALSE)
library(testthat)

test_that("No errors in program",
          {
            expect_no_error()
          }
          )
