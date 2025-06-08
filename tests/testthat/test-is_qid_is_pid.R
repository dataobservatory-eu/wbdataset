test_that("is_pid(), is_qid() works", {
  expect_true(is_pid("P121"))
  expect_true(is_qid("Q121"))
  expect_true(is_qid("q121"))
  expect_false(is_qid("121"))
  expect_equal(is_qid(c("121", "Q42")),
               c(FALSE, TRUE))
  expect_equal(is_pid(c("121", "p121")),
               c(FALSE, TRUE))
  expect_equal(is_pid(c("Q121", NA_character_, "p121")),
               c(FALSE, FALSE, TRUE))
  expect_equal(is_qid(c("Q121", NA_integer_, "p121")),
               c(TRUE, FALSE, FALSE))
})

test_that("get_qid() extracts valid QIDs", {
  expect_equal(get_qid("Q42"), "Q42")
  expect_equal(get_qid(list(id_on_target = "Q99")), "Q99")
  expect_equal(get_qid(structure(list(id_on_target = "Q1"), class = "dummy")), "Q1")
})

test_that("get_qid() returns NA and warns on invalid input", {
  expect_warning(res <- get_qid("P31"), "Expected QID")
  expect_true(is.na(res))

  expect_warning(res2 <- get_qid(list(id_on_target = "not_a_qid")),
                 "Expected QID")
  expect_true(is.na(res2))

  expect_equal(get_qid(NULL), NA_character_)
})

test_that("get_pid() extracts valid PIDs", {
  expect_equal(get_pid("P31"), "P31")
  expect_equal(get_pid(list(id_on_target = "P123")), "P123")
  expect_equal(get_pid(structure(list(id_on_target = "P2"),
                                 class = "dummy")), "P2")
})

test_that("get_pid() returns NA and warns on invalid input", {
  expect_warning(res <- get_pid("Q42"), "Expected PID")
  expect_true(is.na(res))

  expect_warning(res2 <- get_pid(list(id_on_target = "not_a_pid")),
                 "Expected PID")
  expect_true(is.na(res2))

  expect_equal(get_pid(NULL), NA_character_)
})
