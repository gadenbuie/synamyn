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
  expect_equal(find_synonym("happier")["happy"],       find_synonym("happy"))
  expect_equal(find_synonym("happiest")["happy"],      find_synonym("happy"))
  expect_equal(find_synonym("activities")["activity"], find_synonym("activity"))
  expect_equal(find_synonym("releasing")["release"],   find_synonym("release"))
  expect_equal(find_synonym("described")["describe"],  find_synonym("describe"))
  expect_equal(names(find_synonym("loved")), c("loved", "love"))
})

test_that("errors appropriately", {
  expect_error(find_synonym("qwertyasdfg"), "No synonyms found")
  expect_error(find_synonym("qwertyasdfg", FALSE), "No antonyms found")
  expect_error(find_synonym("abc def"), "single word")
})
