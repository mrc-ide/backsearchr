test_that("process_grobid_tei extracts expected fields", {
  tei <- c(
    '<TEI xmlns="http://www.tei-c.org/ns/1.0">',
    "  <text><back><div type='references'><listBibl>",
    "    <biblStruct>",
    "      <analytic>",
    "        <title level='a'>Sample Article</title>",
    "        <author><persName><surname>Smith</surname></persName></author>",
    "      </analytic>",
    "      <monogr>",
    "        <title level='j'>Journal of Tests</title>",
    "        <imprint><date when='2021'/></imprint>",
    "      </monogr>",
    "      <idno type='DOI'>10.1234/example</idno>",
    "    </biblStruct>",
    "  </listBibl></div></back></text>",
    "</TEI>"
  )

  infile <- tempfile(fileext = ".tei.xml")
  writeLines(tei, infile)

  out <- backsearchr:::process_grobid_tei(infile)
  expect_equal(nrow(out), 1)
  expect_identical(out$authors[[1]], "Smith")
  expect_identical(out$title[[1]], "Sample Article")
  expect_identical(out$journal[[1]], "Journal of Tests")
  expect_identical(out$year[[1]], "2021")
  expect_identical(out$doi[[1]], "10.1234/example")
})

test_that("process_cermxml extracts expected fields", {
  cerm <- c(
    "<article>",
    "  <ref-list>",
    "    <ref>",
    "      <element-citation>",
    "        <person-group><name><surname>Patel</surname></name></person-group>",
    "        <article-title>Another Study</article-title>",
    "        <source>Test Journal</source>",
    "        <year>2018</year>",
    "        <article-id pub-id-type='doi'>10.9999/abc</article-id>",
    "      </element-citation>",
    "    </ref>",
    "  </ref-list>",
    "</article>"
  )

  infile <- tempfile(fileext = ".cermxml")
  writeLines(cerm, infile)

  out <- backsearchr:::process_cermxml(infile)
  expect_equal(nrow(out), 1)
  expect_identical(out$authors[[1]], "Patel")
  expect_identical(out$title[[1]], "Another Study")
  expect_identical(out$journal[[1]], "Test Journal")
  expect_identical(out$year[[1]], "2018")
  expect_identical(out$doi[[1]], "10.9999/abc")
})

test_that("extract_references dispatches to requested backend", {
  testthat::local_mocked_bindings(
    extract_references_grobid = function(indir, ...) {
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

  out <- backsearchr::extract_references(tempdir(), method = "grobid")
  expect_equal(nrow(out), 1)
  expect_identical(out$doi[[1]], "10.1/mock")
})

test_that("resolve_grobid_native_lib_dir prefers platform-matching directory", {
  grobid_home <- tempfile("grobid-home-")
  dir.create(file.path(grobid_home, "lib", "mac_arm-64"), recursive = TRUE)
  dir.create(file.path(grobid_home, "lib", "lin-64"), recursive = TRUE)

  testthat::local_mocked_bindings(
    Sys.info = function() {
      c(sysname = "Linux", machine = "x86_64")
    },
    .package = "base"
  )

  out <- backsearchr:::resolve_grobid_native_lib_dir(grobid_home)
  expect_identical(out, file.path(grobid_home, "lib", "lin-64"))
})

test_that("download_doi_pdfs uses provided oa_pdf_url before fallbacks", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  testthat::local_mocked_bindings(
    download_remote_file = function(url, destfile) {
      expect_identical(url, "https://example.org/provided.pdf")
      writeBin(charToRaw("pdf-bytes"), destfile)
      0L
    },
    .package = "backsearchr"
  )

  out <- backsearchr::download_doi_pdfs(
    "10.1000/test",
    outdir,
    oa_pdf_urls = "https://example.org/provided.pdf",
    method = "doi_only"
  )

  expect_equal(nrow(out), 1)
  expect_identical(out$status[[1]], "downloaded")
  expect_identical(out$resolver_used[[1]], "oa_candidate")
  expect_identical(out$resolved_url[[1]], "https://example.org/provided.pdf")
  expect_true(file.exists(out$path[[1]]))
  expect_match(basename(out$path[[1]]), "^10_1000_test\\.pdf$")
})

test_that("download_doi_pdfs uses OpenAlex OA URLs in auto mode", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  testthat::local_mocked_bindings(
    get_openalex_oa_metadata_by_doi = function(dois) {
      data.frame(
        doi = as.character(dois),
        oa_is_oa = TRUE,
        oa_status = "gold",
        oa_url = "https://example.org/openalex-landing",
        oa_pdf_url = "https://example.org/openalex.pdf",
        stringsAsFactors = FALSE
      )
    },
    download_remote_file = function(url, destfile) {
      expect_identical(url, "https://example.org/openalex.pdf")
      writeBin(charToRaw("pdf-bytes"), destfile)
      0L
    },
    .package = "backsearchr"
  )

  out <- backsearchr::download_doi_pdfs("10.1000/no-pdf", outdir, method = "auto")

  expect_equal(nrow(out), 1)
  expect_identical(out$status[[1]], "downloaded")
  expect_identical(out$oa_status[[1]], "gold")
  expect_identical(out$oa_pdf_url[[1]], "https://example.org/openalex.pdf")
})

test_that("download_doi_pdfs flags invalid DOI input", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  out <- backsearchr::download_doi_pdfs("https://doi.org/10.1234/", outdir)

  expect_equal(nrow(out), 1)
  expect_identical(out$status[[1]], "failed")
  expect_match(out$message[[1]], "missing or invalid")
})

test_that("download_doi_pdfs falls back to DOI resolution when OA candidates fail", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  testthat::local_mocked_bindings(
    get_openalex_oa_metadata_by_doi = function(dois) {
      data.frame(
        doi = as.character(dois),
        oa_is_oa = FALSE,
        oa_status = "closed",
        oa_url = "https://example.org/not-pdf",
        oa_pdf_url = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    resolve_pdf_url_from_doi = function(doi) {
      list(
        resolver = "doi",
        source_url = paste0("https://doi.org/", doi),
        resolved_url = "https://example.org/fallback.pdf",
        content_type = "application/pdf",
        is_pdf = TRUE
      )
    },
    download_remote_file = function(url, destfile) {
      expect_identical(url, "https://example.org/fallback.pdf")
      writeBin(charToRaw("pdf-bytes"), destfile)
      0L
    },
    .package = "backsearchr"
  )

  out <- backsearchr::download_doi_pdfs("10.1000/test-a", outdir, overwrite = TRUE, method = "openalex_then_doi")

  expect_identical(out$status[[1]], "downloaded")
  expect_identical(out$resolver_used[[1]], "doi")
})

test_that("download_doi_pdfs doi_only bypasses OpenAlex metadata lookup", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)
  called_openalex <- FALSE

  testthat::local_mocked_bindings(
    get_openalex_oa_metadata_by_doi = function(dois) {
      called_openalex <<- TRUE
      data.frame(
        doi = as.character(dois),
        oa_is_oa = NA,
        oa_status = NA_character_,
        oa_url = NA_character_,
        oa_pdf_url = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    resolve_pdf_url_from_doi = function(doi) {
      list(
        resolver = "doi",
        source_url = paste0("https://doi.org/", doi),
        resolved_url = "https://example.org/direct.pdf",
        content_type = "application/pdf",
        is_pdf = TRUE
      )
    },
    download_remote_file = function(url, destfile) {
      writeBin(charToRaw("pdf-bytes"), destfile)
      0L
    },
    .package = "backsearchr"
  )

  out <- backsearchr::download_doi_pdfs("10.1000/test-b", outdir, overwrite = TRUE, method = "doi_only")

  expect_false(called_openalex)
  expect_identical(out$status[[1]], "downloaded")
  expect_true(is.na(out$oa_status[[1]]))
})

test_that("download_doi_pdfs validates oa vector lengths", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  expect_error(
    backsearchr::download_doi_pdfs(c("10.1/a", "10.1/b"), outdir, oa_pdf_urls = "https://x"),
    "oa_pdf_urls"
  )
  expect_error(
    backsearchr::download_doi_pdfs(c("10.1/a", "10.1/b"), outdir, oa_urls = "https://y"),
    "oa_urls"
  )
})
