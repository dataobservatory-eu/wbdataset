test_that("get_claim() throws error for missing property", {
  expect_error(
    get_claim(qid = "Q42", pid = "P99999999"), # unlikely fake property
    "Property 'P99999999' not found for QID 'Q42'"
  )
})

test_that("get_claim() throws an error for invalid QID", {
  expect_error(
    get_claim(qid = "invalidQID", pid = "P31"),
    "Invalid QID: 'invalidQID'. QIDs must begin with 'Q' followed by digits (e.g., 'Q42').",
    fixed = TRUE
  )
})

test_that("get_claim() throws an error for invalid property ID", {
  expect_error(
    get_claim(qid = "Q42", pid = "invalidPID"),
    "Invalid property ID: 'invalidPID'. Properties must begin with 'P' followed by digits (e.g., 'P31').",
    fixed = TRUE
  )
})


test_that("get_claim(): wikibase-item value correctly returned", {
  expect_equal(
    get_claim(qid = "Q28104185", pid = "P1889")$value,
    "Q234138"
  )
  expect_equal(
    get_claim(qid = "Q28104185", pid = "P1889")$datatype,
    "wikibase-item"
  )
})

test_that("get_claim() returns multiple rows for multi-value properties", {
  df <- get_claim(qid = "Q243", pid = "P2048", first = FALSE)
  expect_true(nrow(df) > 1)
  expect_equal(unique(df$datatype), "quantity")
})


test_that("get_claim() returns correct time value for date of birth", {
  result <- get_claim(qid = "Q42", pid = "P569")
  expect_equal(result$value, "+1952-03-11T00:00:00Z")
  expect_equal(result$datatype, "time")
})

test_that("get_claim(): time value correctly returned", {
  expect_equal(
    get_claim(
      qid = "Q28104185",
      pid = "P1889",
      wikibase_api_url = "https://www.wikidata.org/w/api.php"
    )$value,
    "Q234138"
  )
  expect_equal(
    get_claim(
      qid = "Q28104185",
      pid = "P1889",
      wikibase_api_url = "https://www.wikidata.org/w/api.php"
    )$datatype,
    "wikibase-item"
  )
})

test_that("get_claim(): external-id value correctly returned", {
  expect_equal(
    get_claim(qid = "Q28104185", pid = "P1902")$value,
    "7MoIc5s9KXolCBH1fy9kkw"
  )
  expect_equal(
    get_claim(qid = "Q28104185", pid = "P1902")$datatype,
    "external-id"
  )
})


test_that("get_claim() returns correct height for the Eiffel Tower", {
  result <- get_claim(qid = "Q243", pid = "P2048", first = TRUE)
  expect_equal(result$value, "+330")
  expect_equal(result$datatype, "quantity")
  result_2 <- get_claim(qid = "Q243", pid = "P2048", first = FALSE)
  expect_true(inherits(result_2, "data.frame"))
  expect_equal(nrow(result_2), 3)
  expect_setequal(result_2$value, c("+324", "+300", "+330"))
})

test_that("get_claim() correctly retrieves coordinate location for the Eiffel Tower", {
  result <- get_claim(qid = "Q243",
                      pid = "P625",
                      first = TRUE)
  expect_equal(result$datatype, "globe-coordinate")

  # Parse coordinate string
  coord_string <- result$value
  parts <- strsplit(coord_string, "&")[[1]]
  kv_pairs <- setNames(
    sapply(parts, function(x) strsplit(x, "=")[[1]][2]),
    sapply(parts, function(x) strsplit(x, "=")[[1]][1])
  )

  # Convert lat/lon to numeric and check with tolerance
  expect_equal(as.numeric(kv_pairs["mlat"]), 48.858, tolerance = 0.001)
  expect_equal(as.numeric(kv_pairs["mlon"]), 2.294, tolerance = 0.001)

  # Check that globe is Earth (Q2)
  expect_equal(unname(kv_pairs["globe"]), "http://www.wikidata.org/entity/Q2")
})

test_that("get_claim() returns monolingualtext for 'Notre-Dame de Paris' title", {
  result <- get_claim(qid = "Q191380", pid = "P1476", first = TRUE)
  expect_equal(result$datatype, "monolingualtext")
  expect_equal(result$value, "Notre-Dame de Paris")
})

test_that("get_claim() returns commonsMedia image for 'Notre-Dame de Paris'", {
  result <- get_claim(qid = "Q191380", pid = "P18", first = TRUE)
  expect_equal(result$datatype, "commonsMedia")
  expect_match(result$value, "\\.jpg$") # Assuming the image is in JPG format
})
