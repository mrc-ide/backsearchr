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

test_that("download_doi_pdfs downloads files resolved from Crossref links", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  testthat::local_mocked_bindings(
    cr_works = function(dois, ...) {
      data <- data.frame(doi = dois, stringsAsFactors = FALSE)
      data$link <- I(list(data.frame(
        URL = "https://example.org/paper.pdf",
        content.type = "application/pdf",
        stringsAsFactors = FALSE
      )))
      list(data = data)
    },
    download_remote_file = function(url, destfile) {
      writeBin(charToRaw("pdf-bytes"), destfile)
      0L
    },
    .package = "backsearchr"
  )

  out <- backsearchr::download_doi_pdfs("10.1000/test", outdir)

  expect_equal(nrow(out), 1)
  expect_identical(out$status[[1]], "downloaded")
  expect_true(file.exists(out$path[[1]]))
  expect_match(basename(out$path[[1]]), "^10_1000_test\\.pdf$")
})

test_that("download_doi_pdfs reports missing Crossref PDF links", {
  outdir <- tempfile("doi-downloads-")
  dir.create(outdir)

  testthat::local_mocked_bindings(
    cr_works = function(dois, ...) {
      data <- data.frame(doi = dois, stringsAsFactors = FALSE)
      data$link <- I(list(data.frame(
        URL = "https://example.org/landing-page",
        content.type = "text/html",
        stringsAsFactors = FALSE
      )))
      list(data = data)
    },
    .package = "backsearchr"
  )

  out <- backsearchr::download_doi_pdfs("10.1000/no-pdf", outdir)

  expect_equal(nrow(out), 1)
  expect_identical(out$status[[1]], "failed")
  expect_match(out$message[[1]], "No downloadable PDF link found")
})
