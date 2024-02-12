test_that("myColors: will always return length 1", {
  myColors(1) |> expect_length(1)
  myColors(length(myColors()) + 2) |> expect_length(1)
})

test_that("mySymbols: will always return length 1", {
  mySymbols(1) |> expect_length(1)
  mySymbols(length(mySymbols()) + 2) |> expect_length(1)
})
