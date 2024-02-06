context("Balanced cv folds")


test_that("Only one set", {
  set1 <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE
  )
  result <- balanced_cv_folds(set1, nfolds = 10)

  expect_false(is.list(result$train))
  expect_false(is.list(result$test))
})

test_that("Creates nice cv folds", {
  result <- balanced_cv_folds(seq(10), 1:3, nfolds = 2)
  #
  result.c <- table(result$output[[1]])
  expect_equal(length(result.c), 2)
  expect_equal(as.vector(result.c), c(5, 5))
  #
  result.c <- table(result$output[[2]])
  expect_equal(length(result.c), 2)
  expect_equal(as.vector(result.c), c(2, 1))
  #
  #
  result <- balanced_cv_folds(seq(10), 1:3, nfolds = 3)
  #
  result.c <- table(result$output[[1]])
  expect_equal(length(result.c), 3)
  expect_equal(as.vector(result.c), c(4, 3, 3))
  #
  result.c <- table(result$output[[2]])
  expect_equal(length(result.c), 3)
  expect_equal(as.vector(result.c), c(1, 1, 1))
  #
  expect_warning(
    balanced_cv_folds(seq(10), 1:3, nfolds = 10),
    paste0(
      "Number of elements in vector [(][0-9]+[)] is",
      " less than 'nfolds' [(][0-9]+[)]"
    )
  )
})
