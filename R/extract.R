##' Extract references from papers
##'
##' This function extracts references from a list of papers. Currently, it supports extraction using CERMINE only. We might
## add support for other methods in the future.
##' To use cermine, ensure you have Java installed on your system.
##' Place the pdfs of all the papers which you want to extract references from in a directory anywhere on your machine.
##' 
##' @param indir the path to the directory containing the pdfs of the papers 
##' @param method the method to use for extracting references. Currently, only "cermine" is supported.
##' @return a list of references extracted from the papers
##' @author Sangeeta Bhatia
##' @export
extract_references <- function(indir, method = "cermine", ...) {
   match.arg(method)
   if (method == "cermine") ref_list <- extract_references_cermine(indir, ...)
   ## Now dedeuplicate the references dataframe
   ref_list <- deduplicate_references(ref_list)
   ref_list
}

extract_references_cermine <- function(indir, ...) {
                                        # Check if the directory exists
  indir <- normalizePath(indir)
  if (!dir.exists(indir)) {
    cli_alert_danger("The specified directory does not exist")
  }

  extract_from <- list.files(indir, pattern = ".pdf", full.names = TRUE)
  if (length(extract_from) == 0) {
    cli_alert_info("I did not find any PDFs in the specified directory.
                     I am exiting without doing anything.")
  }


  # Check if the cermine jar file exists
  cermine_jar <- system.file(
    "extdata", "cermine-impl-1.13-jar-with-dependencies.jar",
    package = "backsearchr"
  )
  ## Check that user can run Java
  can_run_java <- suppressWarnings(
    (system2("java", stdout = TRUE, stderr = TRUE))
  )
  if (attr(can_run_java,"status") != 1) {
    cli_alert_danger("I cannot find java on your system, which is needed for CERMINE.
                      I am exiting with error.")
  }

  # Extract references using cermine
  args <- c(
    "-cp", cermine_jar, "pl.edu.icm.cermine.ContentExtractor -path", indir,
    "-outputs jats"
  )
  out <- system2("java", args, stdout = TRUE, stderr = TRUE)
  ## To check if the references were extracted successfully, we check for the
  ## presence of the cermxml files in the directory where the pdfs are
  cermxml_files <- list.files(indir, pattern = ".cermxml", full.names = TRUE)
  if (length(cermxml_files) == 0) {
    cli_alert_danger(
      "I cannot see any files with the extension cermxml in {.path {indir}}.
       This means extraction of references has failed. I am exiting with error.")
  }

  ## Extract references from the cermxml files
  ref_list <- lapply(cermxml_files, function(infile) {

    out <- process_cermxml(infile)
    out$extracted_from <- basename(infile)
    out
  })

  ref_list <- do.call(rbind, ref_list)

  
  ref_list
  
}

##' Process cermxml files
##' @importFrom xml2 read_xml xml_find_all xml_text xml_find_first
##' @noRd
process_cermxml <- function(infile) {
                                        # Read the XML file
    cli_alert_info("Processing    {infile}")
    doc <- read_xml(infile)

    # Extract all references using JATS reference tags (typically <ref> elements)
    references <- xml_find_all(doc, "//ref")

    # Process each reference and extract citation details
  ref_list <- lapply(
    references, function(ref) {
      surname <- xml_text(xml_find_first(ref, ".//surname"))
      title <- xml_text(xml_find_first(ref, ".//article-title"))
      cli_alert_info("Processing {title}")
      ## Find everything that is listed as a source, one of them would be the
      ## journal
      journals <- xml_text(xml_find_all(ref, ".//source"))
      journals <- ifelse(length(journals) == 0, NA, journals)
      journal <- paste(journals, collapse = "|")
      ## If an year appears in the title, it might get slotted in here.
      ## Extract all.
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
  
  ref_list <- do.call(rbind, ref_list)

  ref_list
  
}
