test_that("get_missing_article_info requires explicit method", {
  expect_error(
    backsearchr::get_missing_article_info("10.1000/test"),
    "`method` must be explicitly supplied"
  )
  expect_error(
    backsearchr::get_missing_article_info_(data.frame(doi = NA_character_, title = "x"), method = ""),
    "`method` must be explicitly supplied"
  )
})

test_that("get_missing_article_info returns canonical schema for openalex", {
  testthat::local_mocked_bindings(
    openalex_get_json = function(endpoint, query = list()) {
      expect_identical(endpoint, "/works")
      if (!is.null(query$filter)) {
        return(list(
          results = list(
            list(
              doi = "https://doi.org/10.1000/abc",
              display_name = "DOI hit",
              authorships = list(
                list(author = list(display_name = "A One"))
              ),
              publication_year = 2020,
              primary_location = list(source = list(display_name = "J One")),
              cited_by_count = 11,
              open_access = list(
                is_oa = TRUE,
                oa_status = "gold",
                oa_url = "https://example.org/oa-a",
                any_repository_has_fulltext = FALSE
              ),
              best_oa_location = list(
                pdf_url = "https://example.org/oa-a.pdf"
              )
            )
          )
        ))
      }
      if (!is.null(query$search)) {
        return(list(
          results = list(
            list(
              doi = "https://doi.org/10.2000/title",
              display_name = "Title hit",
              authorships = list(),
              publication_year = 2018,
              primary_location = list(source = list(display_name = "J Two")),
              cited_by_count = 4,
              open_access = list(
                is_oa = TRUE,
                oa_status = "green",
                oa_url = "https://example.org/oa-b",
                any_repository_has_fulltext = TRUE
              ),
              best_oa_location = list(
                pdf_url = "https://example.org/oa-b.pdf"
              )
            )
          )
        ))
      }
      list(results = list())
    },
    .package = "backsearchr"
  )

  out <- backsearchr::get_missing_article_info(
    c("10.1000/ABC", "Some title"),
    method = "openalex"
  )

  expect_identical(
    colnames(out),
    c(
      "input_query", "query_type", "doi", "title", "authors", "year", "journal",
      "citation_count", "oa_is_oa", "oa_status", "oa_url", "oa_pdf_url",
      "oa_any_repository_has_fulltext", "status", "message"
    )
  )
  expect_equal(nrow(out), 2)
  expect_identical(out$query_type, c("doi", "title"))
  expect_identical(out$status, c("ok", "ok"))
  expect_identical(out$oa_status, c("gold", "green"))
  expect_identical(out$oa_is_oa, c(TRUE, TRUE))
  expect_identical(out$oa_url, c("https://example.org/oa-a", "https://example.org/oa-b"))
  expect_identical(out$oa_pdf_url, c("https://example.org/oa-a.pdf", "https://example.org/oa-b.pdf"))
  expect_identical(out$oa_any_repository_has_fulltext, c(FALSE, TRUE))
})

test_that("get_missing_article_info returns canonical schema for crossref", {
  testthat::local_mocked_bindings(
    cr_works = function(dois = NULL, flq = NULL, limit = NULL, select = NULL, ...) {
      if (!is.null(dois)) {
        return(list(
          data = data.frame(
            doi = "10.3000/doi",
            title = I(list("Crossref DOI title")),
            container.title = I(list("Crossref Journal")),
            is.referenced.by.count = 9,
            stringsAsFactors = FALSE
          )
        ))
      }
      if (!is.null(flq)) {
        return(list(
          data = data.frame(
            DOI = "10.3000/title",
            title = I(list("Crossref Title hit")),
            container.title = I(list("Crossref Journal 2")),
            is.referenced.by.count = 2,
            author = I(list(data.frame(given = "Jane", family = "Doe", stringsAsFactors = FALSE))),
            stringsAsFactors = FALSE
          )
        ))
      }
      stop("Unexpected call")
    },
    .package = "backsearchr"
  )

  out <- backsearchr::get_missing_article_info(
    c("10.3000/doi", "A title query"),
    method = "crossref"
  )

  expect_equal(nrow(out), 2)
  expect_identical(out$query_type, c("doi", "title"))
  expect_identical(out$status, c("ok", "ok"))
  expect_identical(out$doi[[1]], "10.3000/doi")
  expect_identical(out$doi[[2]], "10.3000/title")
  expect_true(all(is.na(out$oa_is_oa)))
  expect_true(all(is.na(out$oa_status)))
  expect_true(all(is.na(out$oa_url)))
  expect_true(all(is.na(out$oa_pdf_url)))
  expect_true(all(is.na(out$oa_any_repository_has_fulltext)))
})

test_that("get_missing_article_info_ propagates method to get_missing_article_info", {
  calls <- character(0)
  testthat::local_mocked_bindings(
    get_missing_article_info = function(queries, method) {
      calls <<- c(calls, method)
      data.frame(
        input_query = queries,
        query_type = "title",
        doi = rep("10.9999/found", length(queries)),
        title = rep("Resolved", length(queries)),
        authors = I(replicate(length(queries), character(0), simplify = FALSE)),
        year = as.integer(rep(NA_integer_, length(queries))),
        journal = rep(NA_character_, length(queries)),
        citation_count = as.integer(rep(NA_integer_, length(queries))),
        oa_is_oa = as.logical(rep(NA, length(queries))),
        oa_status = rep(NA_character_, length(queries)),
        oa_url = rep(NA_character_, length(queries)),
        oa_pdf_url = rep(NA_character_, length(queries)),
        oa_any_repository_has_fulltext = as.logical(rep(NA, length(queries))),
        status = rep("ok", length(queries)),
        message = rep("OK", length(queries)),
        stringsAsFactors = FALSE
      )
    },
    .package = "backsearchr"
  )

  refs <- data.frame(
    doi = c(NA_character_, "10.1/exists"),
    title = c("Needs lookup", "Already has doi"),
    stringsAsFactors = FALSE
  )
  out <- backsearchr::get_missing_article_info_(refs, method = "openalex")

  expect_identical(calls, "openalex")
  expect_identical(out$doi[[1]], "10.9999/found")
  expect_true("retrieved_title" %in% names(out))
})
