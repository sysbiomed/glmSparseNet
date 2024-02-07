context("Balanced cv folds")

test_that("Only one set", {
  set1 <- c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE
  )
  result <- balancedCvFolds(set1, nfolds = 10)

  result$train |> checkmate::expect_list() |> expect_failure()
  result$test |> checkmate::expect_list() |> expect_failure()
})

test_that("Creates nice cv folds", {
  result <- balancedCvFolds(seq(10), 1:3, nfolds = 2)
  #
  resultC <- table(result$output[[1]])
  expect_equal(length(resultC), 2)
  expect_equal(as.vector(resultC), c(5, 5))
  #
  resultC <- table(result$output[[2]])
  expect_equal(length(resultC), 2)
  expect_equal(as.vector(resultC), c(2, 1))
  #
  #
  result <- balancedCvFolds(seq(10), 1:3, nfolds = 3)
  #
  resultC <- table(result$output[[1]])
  expect_equal(length(resultC), 3)
  expect_equal(as.vector(resultC), c(4, 3, 3))
  #
  resultC <- table(result$output[[2]])
  expect_equal(length(resultC), 3)
  expect_equal(as.vector(resultC), c(1, 1, 1))
  #
  expect_warning(
    balancedCvFolds(seq(10), 1:3, nfolds = 10),
    paste0(
      "Number of elements in vector [(][0-9]+[)] is",
      " less than 'nfolds' [(][0-9]+[)]"
    )
  )
})
