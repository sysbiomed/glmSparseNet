test_that(".onLoad: Unset options get initialised with defaults", {
    skip_if(getOption("testthat_interactive"))
    withr::with_options(
        list(
            "glmSparseNet.compression" = NULL,
            "glmSparseNet.show_message" = NULL
        ),
        {
            expect_no_error(.onLoad())
            expect_equal(getOption("glmSparseNet.compression"), "gzip")
        }
    )
})

test_that(".onLoad: Initialised options are retained and not overwritten", {
    skip_if(getOption("testthat_interactive"))
    withr::with_options(
        list(glmSparseNet.show_message = FALSE),
        {
            expect_no_error(.onLoad())
            expect_equal(getOption("glmSparseNet.show_message"), FALSE)
        }
    )
})
