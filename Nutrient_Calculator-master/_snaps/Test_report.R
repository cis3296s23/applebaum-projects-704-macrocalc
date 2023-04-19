library(testthat)

# Run the tests in Test_email.R
test_results <- test_file("Test_email.R")

# Generate a test report using the testthat::summary() function
test_report <- testthat::summary(test_results)

# Write the test report to a file
writeLines(test_report, "test_report.txt")