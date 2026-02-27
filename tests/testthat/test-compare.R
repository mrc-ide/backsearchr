test_that("compare_reference_lists partitions extracted refs into matched and not_matched", {
  provided <- data.frame(
    doi = c("10.1/a", NA, "10.1/z"),
    title = c("Paper One", "Interesting Study", "Never Used"),
    stringsAsFactors = FALSE
  )

  extracted <- data.frame(
    doi = c("10.1/a", "10.1/a", NA, "10.1/none"),
    title = c("paper one", "paper one", "interesting study", "No Match"),
    extracted_from = c("p1.tei.xml", "p2.tei.xml", "p3.tei.xml", "p4.tei.xml"),
    stringsAsFactors = FALSE
  )

  out <- backsearchr::compare_reference_lists(
    provided_refs = provided,
    extracted_refs = extracted
  )

  expect_equal(nrow(out$matched) + nrow(out$not_matched), nrow(extracted))
  expect_equal(nrow(out$doi_matched), 2)
  expect_equal(nrow(out$titles_matched), 1)
  expect_equal(nrow(out$not_matched), 1)
  expect_true(all(out$matched$match_type %in% c("doi", "title")))
})

test_that("compare_reference_lists keeps extracted trail and provided matches", {
  provided <- data.frame(
    doi = c("10.1000/a", NA),
    title = c("Alpha", "Beta"),
    stringsAsFactors = FALSE
  )

  extracted <- data.frame(
    doi = c("10.1000/a", NA),
    title = c("alpha", "beta"),
    extracted_from = c("f1.tei.xml", "f2.tei.xml"),
    stringsAsFactors = FALSE
  )

  out <- backsearchr::compare_reference_lists(
    provided_refs = provided,
    extracted_refs = extracted
  )

  expect_true("extracted_from" %in% names(out$matched))
  expect_true("matched_provided_row_ids" %in% names(out$matched))
  expect_true("matched_provided_titles" %in% names(out$matched))
  expect_equal(sort(unique(out$matched$match_type)), c("doi", "title"))
})

test_that("compare_reference_lists works with explicit argument names", {
  provided <- data.frame(
    doi = "10.1/a",
    title = "Paper One",
    stringsAsFactors = FALSE
  )
  extracted <- data.frame(
    doi = "10.1/a",
    title = "Paper One",
    extracted_from = "paper.tei.xml",
    stringsAsFactors = FALSE
  )

  out <- backsearchr::compare_reference_lists(
    provided_refs = provided,
    extracted_refs = extracted
  )

  expect_equal(nrow(out$matched), 1)
  expect_equal(nrow(out$not_matched), 0)
})
