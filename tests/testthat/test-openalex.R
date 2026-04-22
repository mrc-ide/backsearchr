test_that("get_study_details_openalex handles mixed DOI and title queries", {
  testthat::local_mocked_bindings(
    openalex_get_json = function(endpoint, query = list()) {
      expect_identical(endpoint, "/works")
      if (!is.null(query$filter)) {
        expect_true(grepl("doi:", query$filter, fixed = TRUE))
        return(list(
          results = list(
            list(
              doi = "https://doi.org/10.1000/test-a",
              display_name = "DOI Paper A",
              authorships = list(
                list(author = list(display_name = "Author One")),
                list(author = list(display_name = "Author Two"))
              ),
              publication_year = 2021,
              primary_location = list(source = list(display_name = "Journal A")),
              cited_by_count = 12
            )
          )
        ))
      }
      if (!is.null(query$search) && identical(query$search, "Interesting title")) {
        return(list(
          results = list(
            list(
              doi = "https://doi.org/10.2000/title-hit",
              display_name = "Interesting Title Match",
              authorships = list(
                list(author = list(display_name = "Title Author"))
              ),
              publication_year = 2019,
              primary_location = list(source = list(display_name = "Journal T")),
              cited_by_count = 7
            )
          )
        ))
      }
      list(results = list())
    },
    .package = "backsearchr"
  )

  queries <- c("https://doi.org/10.1000/test-a", "Interesting title", "10.1000/missing")
  out <- backsearchr::get_study_details_openalex(queries)

  expect_equal(nrow(out), 3)
  expect_identical(out$input_query, queries)
  expect_identical(out$query_type, c("doi", "title", "doi"))
  expect_identical(out$status, c("ok", "ok", "not_found"))
  expect_identical(out$doi[[1]], "10.1000/test-a")
  expect_identical(out$doi[[2]], "10.2000/title-hit")
  expect_true(is.na(out$doi[[3]]))
  expect_true(is.list(out$authors))
  expect_identical(out$authors[[1]], c("Author One", "Author Two"))
  expect_identical(out$authors[[2]], "Title Author")
  expect_true(all(is.na(out$oa_is_oa)))
  expect_true(all(is.na(out$oa_status)))
  expect_true(all(is.na(out$oa_url)))
  expect_true(all(is.na(out$oa_pdf_url)))
  expect_true(all(is.na(out$oa_any_repository_has_fulltext)))
})

test_that("get_study_details_openalex normalizes DOI inputs", {
  testthat::local_mocked_bindings(
    openalex_get_json = function(endpoint, query = list()) {
      expect_identical(endpoint, "/works")
      if (!is.null(query$filter)) {
        expect_true(grepl("https://doi.org/10.5555/abc", query$filter, fixed = TRUE))
        expect_true(grepl("https://doi.org/10.5555/def", query$filter, fixed = TRUE))
        return(list(
          results = list(
            list(
              doi = "https://doi.org/10.5555/abc",
              display_name = "Paper ABC",
              authorships = list(),
              publication_year = 2020,
              primary_location = list(source = list(display_name = "J1")),
              cited_by_count = 1
            ),
            list(
              doi = "https://doi.org/10.5555/def",
              display_name = "Paper DEF",
              authorships = list(),
              publication_year = 2022,
              primary_location = list(source = list(display_name = "J2")),
              cited_by_count = 2
            )
          )
        ))
      }
      stop("Unexpected call")
    },
    .package = "backsearchr"
  )

  out <- backsearchr::get_study_details_openalex(c("10.5555/ABC", "https://doi.org/10.5555/def"))
  expect_identical(out$status, c("ok", "ok"))
  expect_identical(out$doi[[1]], "10.5555/abc")
  expect_identical(out$doi[[2]], "10.5555/def")
})

test_that("get_study_details_openalex keeps top title result only", {
  testthat::local_mocked_bindings(
    openalex_get_json = function(endpoint, query = list()) {
      expect_identical(endpoint, "/works")
      expect_identical(query[["per-page"]], 1)
      list(
        results = list(
          list(
            doi = "https://doi.org/10.7777/top",
            display_name = "Top Result",
            authorships = list(),
            publication_year = 2024,
            primary_location = list(source = list(display_name = "Top Journal")),
            cited_by_count = 88
          ),
          list(
            doi = "https://doi.org/10.7777/second",
            display_name = "Second Result",
            authorships = list(),
            publication_year = 2023,
            primary_location = list(source = list(display_name = "Other Journal")),
            cited_by_count = 5
          )
        )
      )
    },
    .package = "backsearchr"
  )

  out <- backsearchr::get_study_details_openalex("Some title")
  expect_equal(nrow(out), 1)
  expect_identical(out$status[[1]], "ok")
  expect_identical(out$doi[[1]], "10.7777/top")
  expect_identical(out$title[[1]], "Top Result")
})

test_that("get_study_details_openalex handles not-found and API errors by row", {
  testthat::local_mocked_bindings(
    openalex_get_json = function(endpoint, query = list()) {
      expect_identical(endpoint, "/works")
      if (!is.null(query$search) && identical(query$search, "Broken title")) {
        stop("simulated OpenAlex outage")
      }
      if (!is.null(query$search) && identical(query$search, "No match title")) {
        return(list(results = list()))
      }
      stop("Unexpected query")
    },
    .package = "backsearchr"
  )

  out <- backsearchr::get_study_details_openalex(c("No match title", "Broken title"))
  expect_identical(out$query_type, c("title", "title"))
  expect_identical(out$status, c("not_found", "failed"))
  expect_match(out$message[[1]], "No OpenAlex match")
  expect_match(out$message[[2]], "simulated OpenAlex outage")
})

test_that("openalex DOI batching excludes invalid DOI-like inputs", {
  seen_filter <- NULL
  testthat::local_mocked_bindings(
    openalex_get_json = function(endpoint, query = list()) {
      expect_identical(endpoint, "/works")
      if (!is.null(query$filter)) {
        seen_filter <<- query$filter
      }
      list(
        results = list(
          list(
            doi = "https://doi.org/10.1371/journal.pntd.0006627",
            display_name = "Valid DOI hit",
            authorships = list(),
            publication_year = 2018,
            primary_location = list(source = list(display_name = "Journal")),
            cited_by_count = 10
          )
        )
      )
    },
    .package = "backsearchr"
  )

  out <- backsearchr::get_missing_article_info(
    c("10.1371/journal.pntd.0006627", "https://doi.org/10.1234/"),
    method = "openalex"
  )

  expect_true(grepl("10.1371/journal.pntd.0006627", seen_filter, fixed = TRUE))
  expect_false(grepl("10.1234/", seen_filter, fixed = TRUE))
  expect_identical(out$status, c("ok", "failed"))
  expect_match(out$message[[2]], "missing or invalid")
})
