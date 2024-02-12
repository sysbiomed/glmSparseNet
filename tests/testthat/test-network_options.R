test_that(".calcPenalty: only supports `degree`", {
  .calcPenalty(1, "covariance", networkOptions(centrality = "unsupported")) |>
    expect_error("Centrality method not recognised")
})
