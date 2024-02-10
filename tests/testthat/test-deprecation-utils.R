test_that("Deprecated functions", {
  balanced.cv.folds(1:10, nfolds = 10) |> lifecycle::expect_deprecated()
  my.colors() |> lifecycle::expect_deprecated()
  my.symbols() |> lifecycle::expect_deprecated()
  hallmarks() |> lifecycle::expect_defunct()
})
