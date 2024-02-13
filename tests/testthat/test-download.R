test_that("downloadFileLocal: Detects when download is size 0", {
    local_mocked_bindings(
        download.file = function(destfile = "destination.txt", ...) {
            con <- file(destfile, open = "wt")
            close(con)
        },
        .package = "utils"
    )

    downloadFileLocal(
        "https://string-db.org/api/tsv-no-header/version"
    ) |>
        expect_output("failed to download")
})
