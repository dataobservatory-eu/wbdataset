test_that("get_csrf_token() extracts token from csrf object", {
  skip_if_not_installed("mockery")
  library(mockery)
  fake_csrf_response <- list(
    query = list(tokens = list(csrftoken = "123abcFAKETOKENxyz"))
  )

  stub(get_csrf_token, "httr::content",
       function(...) fake_csrf_response)

  csrf_token <- get_csrf_token(csrf = "fake_response_object")
  expect_type(csrf_token, "character")
  expect_true(nchar(csrf_token) >= 10)
  expect_equal(csrf_token, "123abcFAKETOKENxyz")
})

test_that("get_csrf() returns a csrf object from mocked API calls", {
  skip_if_not_installed("mockery")
  library(mockery)
  # Fake login token response
  fake_response_1 <- structure(
    list(
      status_code = 200
    ),
    class = "response"
  )

  # Fake login token content (step 1)
  fake_login_token_content <- list(
    query = list(tokens = list(logintoken = "mockLoginToken12345"))
  )

  # Fake login response (step 2)
  fake_response_2 <- structure(
    list(
      status_code = 200
    ),
    class = "response"
  )

  fake_login_result_content <- list(
    login = list(result = "Success")
  )

  # Final CSRF token response (step 3)
  fake_csrf_response <- structure(
    list(
      status_code = 200
    ),
    class = "response"
  )

  fake_csrf_content <- list(
    query = list(tokens = list(csrftoken = "mockCsrfToken1234567890"))
  )

  # Sequence tracking to return different content on each `httr::content` call
  call_counter <- 0

  stub(get_csrf, "check_api_url", function(...) TRUE)

  stub(get_csrf, "httr::handle", function(...) {
    structure(list(url = "https://example.org/api.php"), class = "handle")
  })

  stub(get_csrf, "httr::GET", function(url, query = NULL) {
    if (!is.null(query) && identical(query$type, "login")) {
      fake_response_1
    } else if (!is.null(query) && identical(query$meta, "tokens")) {
      fake_csrf_response
    } else {
      fake_response_1
    }
  })

  stub(get_csrf, "httr::POST", function(...) fake_response_2)

  stub(get_csrf, "httr::content", function(...) {
    call_counter <<- call_counter + 1
    if (call_counter == 1) return(fake_login_token_content)
    if (call_counter == 2) return(fake_login_result_content)
    return(fake_csrf_content)
  })

  csrf <- get_csrf("myUser", "myPass", "https://example.org/api.php")

  expect_s3_class(csrf, "response")
})


test_that("check_api_url() passes for valid URLs", {
  expect_silent(check_api_url("https://example.org/api.php"))
  expect_silent(check_api_url("http://localhost:8181/w/api.php"))
})

test_that("check_api_url() fails for invalid URLs", {
  expect_error(check_api_url("https://example.org/w/index.php"),
               "must end with api.php")
  expect_error(check_api_url("https://example.org/"),
               "must end with api.php")
  expect_error(check_api_url("api.json"),
               "must end with api.php")
})
