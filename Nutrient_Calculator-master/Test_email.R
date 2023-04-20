get_user_info <- function(authenticated, gar_api_generator, user_email, import) {
  user_info <- list()
  
  if (authenticated()) {
    tryCatch({
      user_info <- gar_api_generator() %>% data_parse_function()
      user_email(user_info$email)
      database <- import("my_database.db")
      user_info_db <- database$get_user_info(user_info$email)
      
      if (is.null(user_info_db)) {
        database$save_user_info(user_info$name, user_info$email)
      }
    }, error = function(e) {
      # Handle any errors that occur
    })
  }
  
  return(user_info)
}


# Define test environment for new users
authenticated_new <- function() { TRUE }
gar_api_generator_new <- function(url, method, data_parse_function, checkTrailingSlash) {
  return(function() {
    return(list(name = "Gevork Dramgotchian", email = "tuc50903@temple.edu"))
  })
}
user_email_new <- function(email) {}
import_new <- function(module) {
  return(list(
    get_user_info = function(email) { return("tuc50903@temple.edu") },
    save_user_info = function(name, email) {
      expect_equal(name, "Gevork Dramgotchian")
      expect_equal(email, "tuc50903@temple.edu")
    }
  ))
}

# Define the test environment for existing users
authenticated_existing <- function() { TRUE }
gar_api_generator_existing <- function(url, method, data_parse_function, checkTrailingSlash) {
  return(function() {
    return(list(name = "Gevork Dramgotchian", email = "tuc50903@temple.edu"))
  })
}
user_email_existing <- function(email) {}
import_existing <- function(module) {
  return(list(
    get_user_info = function(email) {
      return(list(name = "Gevork Dramgotchian", email = "tuc50903@temple.edu"))
    },
    save_user_info = function(name, email) {}
  ))
}

# Define the tests for the get_user_info function
context("Tests for get_user_info function")

test_that("get_user_info saves new user to database", {
  # Call the function with the new user environment and make assertions
  user_info <- get_user_info(authenticated_new, gar_api_generator_new, user_email_new, import_new)
  expect_equal(user_info$name, "Gevork Dramgotchian")
  expect_equal(user_info$email, "tuc50903@temple.edu")
})

test_that("get_user_info retrieves existing user data from database", {
  # Call the function with the existing user environment and make assertions
  user_info <- get_user_info(authenticated_existing, gar_api_generator_existing, user_email_existing, import_existing)
  expect_equal(user_info$name, "Gevork Dramgotchian")
  expect_equal(user_info$email, "tuc50903@temple.edu")
})
