##' Extract references from papers
##'
##' This function extracts references from a list of papers. Currently, it supports extraction using CERMINE
##' and GROBID.
##' To use cermine or grobid, ensure you have Java installed on your system.
##' Place the pdfs of all the papers which you want to extract references from in a directory anywhere on your machine.
##'
##' @param indir the path to the directory containing the pdfs of the papers
##' @param method the method to use for extracting references. Supported values are "cermine" and "grobid".
##' @importFrom cli cli_alert_danger cli_alert_info
##' @return a list of references extracted from the papers
##' @author Sangeeta Bhatia
##' @export
extract_references <- function(indir, method = c("cermine", "grobid"), ...) {
  method <- match.arg(method)
  if (method == "cermine") {
    ref_list <- extract_references_cermine(indir, ...)
  } else if (method == "grobid") {
    ref_list <- extract_references_grobid(indir, ...)
  }

  # Now deduplicate the references dataframe.
  ref_list <- deduplicate_references(ref_list)
  ref_list
}

empty_reference_df <- function() {
  data.frame(
    authors = character(0),
    title = character(0),
    journal = character(0),
    year = character(0),
    doi = character(0)
  )
}

extract_references_cermine <- function(indir, ...) {
  indir <- normalizePath(indir)
  if (!dir.exists(indir)) {
    cli_alert_danger("The specified directory does not exist")
  }

  extract_from <- list.files(
    indir, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE,
    ignore.case = TRUE
  )
  if (length(extract_from) == 0) {
    cli_alert_info("I did not find any PDFs in the specified directory.
                     I am exiting without doing anything.")
    stop("No PDFs found in input directory.", call. = FALSE)
  }

  # Check if the cermine jar file exists.
  cermine_jar <- system.file(
    "extdata", "cermine-impl-1.13-jar-with-dependencies.jar",
    package = "backsearchr"
  )
  if (cermine_jar == "") {
    cli_alert_danger("I could not find the bundled CERMINE jar in the package.")
  }

  # Check that user can run Java.
  check_java_available("CERMINE")

  # Extract references using cermine.
  args <- c(
    "-cp", cermine_jar, "pl.edu.icm.cermine.ContentExtractor",
    "-path", indir,
    "-outputs", "jats"
  )
  system2("java", args, stdout = TRUE, stderr = TRUE)

  # Check whether extraction produced cermxml files.
  cermxml_files <- list.files(
    indir, pattern = "\\.cermxml$", full.names = TRUE, recursive = TRUE
  )
  if (length(cermxml_files) == 0) {
    cli_alert_danger(
      "I cannot see any files with the extension cermxml in {.path {indir}}.
       This means extraction of references has failed. I am exiting with error."
    )
  }

  # Extract references from the cermxml files.
  ref_list <- lapply(cermxml_files, function(infile) {
    out <- process_cermxml(infile)
    out$extracted_from <- basename(infile)
    out
  })

  if (length(ref_list) == 0) {
    return(cbind(empty_reference_df(), extracted_from = character(0)))
  }

  do.call(rbind, ref_list)
}

extract_references_grobid <- function(
  indir, grobid_jar = NULL, grobid_home = NULL,
  grobid_exe = "processReferences", grobid_java_opts = c("-Xmx2G"), ...
) {
  indir <- normalizePath(indir)
  if (!dir.exists(indir)) {
    cli_alert_danger("The specified directory does not exist")
  }

  extract_from <- list.files(
    indir, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE,
    ignore.case = TRUE
  )
  if (length(extract_from) == 0) {
    cli_alert_info("I did not find any PDFs in the specified directory.
                     I am exiting without doing anything.")
    stop("No PDFs found in input directory.", call. = FALSE)
  }

  # Resolve bundled GROBID jar by default, allow explicit override.
  if (is.null(grobid_jar)) {
    grobid_jar <- system.file(
      "extdata", "grobid-core-0.8.2-onejar.jar",
      package = "backsearchr"
    )
  }
  if (grobid_jar == "" || !file.exists(grobid_jar)) {
    cli_alert_danger(
      "I could not find a GROBID jar. Provide `grobid_jar=` or bundle it under inst/extdata."
    )
  }

  if (is.null(grobid_home)) {
    grobid_home <- system.file("extdata", "grobid-home", package = "backsearchr")
  }
  if (grobid_home == "" || !dir.exists(grobid_home)) {
    cli_alert_danger(
      "I could not find `grobid-home`. Provide `grobid_home=` or bundle it under inst/extdata."
    )
  }

  check_java_available("GROBID")

  # Run GROBID batch parser over all PDFs in input directory.
  native_lib_candidates <- c(
    file.path(grobid_home, "lib", "mac_arm-64"),
    file.path(grobid_home, "lib", "mac-64"),
    file.path(grobid_home, "lib", "lin-64"),
    file.path(grobid_home, "lib", "win-64")
  )
  native_lib_dir <- native_lib_candidates[dir.exists(native_lib_candidates)][1]
  java_lib_opt <- character(0)
  if (!is.na(native_lib_dir) && nzchar(native_lib_dir)) {
    java_lib_opt <- paste0("-Djava.library.path=", native_lib_dir)
  }

  args <- c(
    grobid_java_opts,
    java_lib_opt,
    "-jar", grobid_jar,
    "-gH", grobid_home,
    "-dIn", indir,
    "-dOut", indir,
    "-exe", grobid_exe
  )
  grobid_out <- system2("java", args, stdout = TRUE, stderr = TRUE)
  grobid_status <- attr(grobid_out, "status")
  if (!is.null(grobid_status) && grobid_status != 0) {
    cli_alert_danger("GROBID exited with status {grobid_status}.")
    if (length(grobid_out) > 0) {
      cli_alert_info("GROBID output:\n{paste(grobid_out, collapse = '\n')}")
    }
    stop("GROBID extraction failed.", call. = FALSE)
  }

  # Parse generated TEI XML files.
  tei_files <- list.files(
    indir, pattern = "\\.tei\\.xml$", full.names = TRUE, recursive = TRUE,
    ignore.case = TRUE
  )
  if (length(tei_files) == 0) {
    # Some builds output generic .xml; keep only files likely to be TEI outputs.
    all_xml <- list.files(
      indir, pattern = "\\.xml$", full.names = TRUE, recursive = TRUE,
      ignore.case = TRUE
    )
    tei_files <- all_xml[!grepl("\\.cermxml$", all_xml, ignore.case = TRUE)]
  }
  if (length(tei_files) == 0) {
    cli_alert_danger(
      "I cannot see any files with extension .tei.xml in {.path {indir}}.
       This means extraction of references has failed. I am exiting with error."
    )
  }

  ref_list <- lapply(tei_files, function(infile) {
    out <- process_grobid_tei(infile)
    out$extracted_from <- basename(infile)
    out
  })

  if (length(ref_list) == 0) {
    return(cbind(empty_reference_df(), extracted_from = character(0)))
  }

  do.call(rbind, ref_list)
}

check_java_available <- function(tool_name) {
  can_run_java <- suppressWarnings(
    system2("java", "-version", stdout = TRUE, stderr = TRUE)
  )
  status <- attr(can_run_java, "status")
  if (!is.null(status) && status != 0) {
    cli_alert_danger(
      "I cannot find java on your system, which is needed for {tool_name}.\nI am exiting with error."
    )
    stop("Java is required for ", tool_name, call. = FALSE)
  }
}

##' Process cermxml files
##' @importFrom xml2 read_xml xml_find_all xml_text xml_find_first xml_attr
##' @noRd
process_cermxml <- function(infile) {
  # Read the XML file.
  cli_alert_info("Processing    {infile}")
  doc <- read_xml(infile)

  # Extract all references using JATS reference tags (typically <ref> elements).
  references <- xml_find_all(doc, "//ref")

  # Process each reference and extract citation details.
  if (length(references) == 0) {
    return(empty_reference_df())
  }

  ref_list <- lapply(references, function(ref) {
    surname <- xml_text(xml_find_first(ref, ".//surname"))
    title <- xml_text(xml_find_first(ref, ".//article-title"))

    # Find everything that is listed as a source; one should be the journal.
    journals <- xml_text(xml_find_all(ref, ".//source"))
    journals <- ifelse(length(journals) == 0, NA, journals)
    journal <- paste(journals, collapse = "|")

    # If a year appears in title, it might get slotted in here. Keep all.
    year <- xml_text(xml_find_all(ref, ".//year"))
    year <- ifelse(length(year) == 0, NA, year)

    doi <- xml_text(xml_find_first(ref, ".//article-id[@pub-id-type='doi']"))
    doi <- ifelse(length(doi) == 0, NA, doi)

    data.frame(
      authors = surname,
      title = title,
      journal = journal,
      year = year,
      doi = doi
    )
  })

  do.call(rbind, ref_list)
}

process_grobid_tei <- function(infile) {
  cli_alert_info("Processing    {infile}")
  doc <- read_xml(infile)

  # TEI references usually live under //listBibl/biblStruct.
  references <- xml_find_all(doc, "//*[local-name()='listBibl']/*[local-name()='biblStruct']")

  if (length(references) == 0) {
    return(empty_reference_df())
  }

  ref_list <- lapply(references, function(ref) {
    surname <- xml_text(xml_find_first(
      ref,
      ".//*[local-name()='analytic']//*[local-name()='author'][1]//*[local-name()='surname'][1]"
    ))

    title <- xml_text(xml_find_first(
      ref,
      ".//*[local-name()='analytic']//*[local-name()='title'][1]"
    ))
    if (identical(title, "")) {
      title <- xml_text(xml_find_first(
        ref,
        ".//*[local-name()='monogr']//*[local-name()='title'][1]"
      ))
    }

    journal <- xml_text(xml_find_first(
      ref,
      ".//*[local-name()='monogr']//*[local-name()='title'][@level='j'][1]"
    ))
    if (identical(journal, "")) {
      journal <- xml_text(xml_find_first(
        ref,
        ".//*[local-name()='monogr']//*[local-name()='title'][1]"
      ))
    }

    year_node <- xml_find_first(
      ref,
      ".//*[local-name()='imprint']//*[local-name()='date'][1]"
    )
    year <- xml_attr(year_node, "when")
    if (length(year) == 0 || is.na(year) || identical(year, "")) {
      year <- xml_text(year_node)
    }

    doi <- xml_text(xml_find_first(
      ref,
      ".//*[local-name()='idno'][translate(@type, 'DOI', 'doi')='doi'][1]"
    ))

    data.frame(
      authors = ifelse(identical(surname, ""), NA, surname),
      title = ifelse(identical(title, ""), NA, title),
      journal = ifelse(identical(journal, ""), NA, journal),
      year = ifelse(identical(year, ""), NA, year),
      doi = ifelse(identical(doi, ""), NA, doi)
    )
  })

  do.call(rbind, ref_list)
}
