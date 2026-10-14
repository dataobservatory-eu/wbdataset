# Tests for upload_local_media() that require no API access

test_that("file must be a single character string", {
  expect_error(
    upload_local_media(file = 123, csrf = NULL),
    "`file` must be a single character string"
  )

  expect_error(
    upload_local_media(file = NA_character_, csrf = NULL),
    "`file` must be a single character string"
  )

  expect_error(
    upload_local_media(file = "", csrf = NULL),
    "`file` must be a single character string"
  )

  expect_error(
    upload_local_media(file = character(0), csrf = NULL),
    "`file` must be a single character string"
  )

  expect_error(
    upload_local_media(file = c("a.jpg", "b.jpg"), csrf = NULL),
    "`file` must be a single character string"
  )
})

test_that("filename must be a single character string", {
  file <- tempfile(fileext = ".jpg")
  writeBin(as.raw(c(255, 216, 255, 217)), file)
  on.exit(unlink(file))

  expect_error(
    upload_local_media(file, filename = 123, csrf = NULL),
    "`filename` must be a single character string"
  )

  expect_error(
    upload_local_media(file, filename = "", csrf = NULL),
    "`filename` must be a single character string"
  )

  expect_error(
    upload_local_media(file, filename = NA_character_, csrf = NULL),
    "`filename` must be a single character string"
  )

  expect_error(
    upload_local_media(file,
      filename = c("a.jpg", "b.jpg"),
      csrf = NULL
    ),
    "`filename` must be a single character string"
  )
})

test_that("nonexistent files are rejected", {
  file <- tempfile(fileext = ".jpg")

  expect_false(file.exists(file))

  expect_error(
    upload_local_media(file, csrf = NULL)
  )
})

test_that("directories cannot be uploaded as files", {
  expect_error(
    upload_local_media(tempdir(), csrf = NULL),
    "not a regular file"
  )
})

test_that("local test files can be created and resolved", {
  file <- tempfile(fileext = ".jpg")
  writeBin(as.raw(c(255, 216, 255, 217)), file)
  on.exit(unlink(file))

  expect_true(file.exists(file))
  expect_false(dir.exists(file))

  path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  expect_type(path, "character")
  expect_length(path, 1L)
  expect_true(file.exists(path))
  expect_identical(basename(path), basename(file))
})

test_that("default filename corresponds to the local basename", {
  file <- tempfile(fileext = ".jpg")
  writeBin(as.raw(c(255, 216, 255, 217)), file)
  on.exit(unlink(file))

  filename <- basename(file)

  expect_type(filename, "character")
  expect_length(filename, 1L)
  expect_true(endsWith(filename, ".jpg"))
  expect_identical(filename, basename(file))
})
