context("test-find_synonym")

test_that("finds a regular synonym", {
  x <- find_synonym("happy")
  expect_true("cheery" %in% x$happy)
})

test_that("finds a regular antonym", {
  x <- find_synonym("happy", FALSE)
  expect_equal(x$happy, "unhappy")
})

test_that("finds syns/ants for irregular words", {
  expect_equal(find_synonym("happier"), find_synonym("happy"))
  expect_equal(find_synonym("happiest"), find_synonym("happy"))
  expect_equal(find_synonym("activities"), find_synonym("activity"))
  expect_equal(find_synonym("releasing"), find_synonym("release"))
  expect_equal(find_synonym("loved"), find_synonym("loved"))
  expect_equal(find_synonym("described"), find_synonym("describe"))
})
