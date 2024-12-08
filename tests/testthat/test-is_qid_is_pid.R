


test_that("is_pid(), is_qid() works", {
  expect_true(is_pid("P121"))
  expect_true(is_qid("Q121"))
  expect_true(is_qid("q121"))
  expect_false(is_qid("121"))
  expect_equal(is_qid(c("121", "Q42")), c(FALSE, TRUE))
  expect_equal(is_pid(c("121", "p121")), c(FALSE, TRUE))
  expect_equal(is_pid(c("Q121", NA_character_, "p121")), c(FALSE, FALSE, TRUE))
  expect_equal(is_qid(c("Q121", NA_integer_, "p121")), c(TRUE, FALSE, FALSE))
})


