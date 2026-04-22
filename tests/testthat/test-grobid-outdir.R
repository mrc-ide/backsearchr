test_that("extract_references forwards grobid_outdir to the grobid backend", {
  temp_in <- tempdir()
  expected_outdir <- file.path(temp_in, "grobid-out")

  testthat::local_mocked_bindings(
    extract_references_grobid = function(indir, grobid_outdir = NULL, ...) {
      expect_identical(indir, temp_in)
      expect_identical(grobid_outdir, expected_outdir)
      data.frame(
        authors = "Doe",
        title = "Mock Title",
        journal = "Mock Journal",
        year = "2020",
        doi = "10.1/mock",
        extracted_from = "mock.tei.xml",
        stringsAsFactors = FALSE
      )
    },
    deduplicate_references = function(x) x,
    .package = "backsearchr"
  )

  out <- backsearchr::extract_references(
    indir = temp_in,
    method = "grobid",
    grobid_outdir = expected_outdir
  )

  expect_equal(nrow(out), 1)
  expect_identical(out$doi[[1]], "10.1/mock")
})
