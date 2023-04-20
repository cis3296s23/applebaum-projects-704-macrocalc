source("/Users/gevork/Documents/GitHub/MacroCalc704/Nutrient_Calculator-master/app.R", chdir = TRUE)

library(testthat)

test_that("global variables test", { server(NULL, NULL, FALSE)
  
  # check that the values are initialized correctly
  expect_equal(g_user_email(), "tranbaoson2005@gmail.com")
  expect_equal(g_user_name(), "son tran")
  expect_equal(g_authenticated(), FALSE)
  expect_equal(g_food_id(), 0)
  expect_equal(g_measure_unit(), "")
  expect_equal(g_quantity(), 0)
})
