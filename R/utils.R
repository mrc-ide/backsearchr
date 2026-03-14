##' Clean up a string for comparison
##'
##' This function removes punctuation, spaces, and articles (a, an, the) from a
##' string. It also converts the string to lowercase. These steps make
##' string comparison easier, as automatically extract text is likely to have
##' minor differences in punctuation, capitalization, and articles, which for our
##' purposes are not important. Removal of articles is less important for 
##' comparison of title strings, but more important for comparison of journal
##' names.
##' @title 
##' @param text 
##' @return 
##' @author Sangeeta Bhatia
##' @export
cleanup_strings <- function(text) {
  text <- tolower(text) # Convert to lowercase
  text <- gsub("\\b(a|an|the)\\b", "", text, ignore.case = TRUE)
  text <- gsub("[[:punct:]]", "", text) # Remove punctuation
  text <- gsub("\\s+", "", text) # Remove all spaces
  text
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}

##' Deduplicate a list of references
##'
##' Identify duplicates based on DOI and title. Remove duplicates and return a
##' deduplicated list of references. 
##' @param x data.frame of references. Must have columns "doi" and "title".
##' @return a deduplicated data.frame of references with an additional column
##' called "cleanup_title" that contains the cleaned up title string. See
##' \code{\link{cleanup_strings}} for details.
##' @author Sangeeta Bhatia
##' @export
deduplicate_references <- function(x) {
  
  x$cleanup_title <- cleanup_strings(x$title)
  ## First check for duplicates based on DOI
  ## I think duplicated is treating NA as a value, so we need to compare non-NA
  ## values only
  doi_not_na <- x[!is.na(x$doi), ]
  doi_not_na <- doi_not_na[!duplicated(doi_not_na$doi), ]
  
  doi_na <- x[is.na(x$doi), ]
  doi_na <- doi_na[!duplicated(doi_na$cleanup_title), ]

  x <- rbind(doi_not_na, doi_na)
  x
}

##' Retrieve additional information for a reference using crossref
##'
##' Automated extraction leaves a lot wanting. The only thing we can reliably
##' extract is the title. We can use this title to query crossref and get
##' additional information. This function takes a single reference and author
##' name and 
##' queries crossref for additional information. It returns a data.frame with
##' the title and DOI information for the input query parameters. If you wish to
##' query multiple references, use \code{\link{get_missing_article_info_}}.
##' @param title (character) title of the article
##' @param authors (character) authors of the article
##' @return a data.frame with title and DOI information for the input query
##' parameters.
##' @importFrom rcrossref cr_works
##' @importFrom cli cli_alert_danger cli_alert_info
##' @author Sangeeta Bhatia
##' @export
get_missing_article_info <- function(title, authors) {
  ## Only get the first result
  if (is.na(title) & is.na(authors)) {
    cli_alert_info("Both title and author is NA. I am returning empty-handed.")
    return(NULL)
  }
  inputs <- c(title, authors)
  query <- paste(inputs[!is.na(inputs)], collapse = " ")
  cr <- tryCatch({cr_works(
    flq = c(query.bibliographic = query), limit = 1, select = c("DOI", "title")
  )},
  error = function(cond) {
    message(conditionMessage(cond))
    return(NULL)
  })
  if (cr$meta$total_results == 0) {
    cli_alert_info("I could not find any entry in Crossref for {title}")
    return(NULL)
  }
  ## Somewhat annoyingly, the returned data.frame may not contain the columns we asked for.
  if ("title" %in% colnames(cr$data)) {
    cr$data$cleanup_title <- cleanup_strings(cr$data$title)
    idx <- grepl(cleanup_strings(title), cr$data$cleanup_title)
    cr$data <- cr$data[idx, ]    
  } else {
    ## Add a column anyway so that we don't error downstream
    cr$data$title <- NA
  }
  cr$data
}

##' Retrieve additional information for multiple references using crossref
##'
##' This function takes a data.frame of references and queries crossref for
##' additional information for each reference that is missing a DOI. It returns
##' a data.frame with the
##' title and DOI information for the input query parameters. 
##' 
##' @param ref_list A list of references, most likely output from
##' \code{\link{extract_references}}. If you have produced a list of references
##' manually, ensure that it has the following columns: doi, title, authors.
##' @param title (character) title of the article
##' @param authors (character) authors of the article
##' @return the input data.frame with DOI updated where it could be retrirved
##' from rcrossref
##' @author Sangeeta Bhatia
##' @export
get_missing_article_info_ <- function(ref_list) {
   ## Now extract additional information for each reference
   ## doi is the key to everything; so if we have doi, we won;t
   ## bother querying the crossref API.
   doi_not_na <- ref_list[!is.na(ref_list$doi), ]
   doi_na <- ref_list[is.na(ref_list$doi), ]
   cli_alert_info("DOI available for {nrow(doi_not_na)} reference{?s}")
   cli_alert_info("DOI missing for {nrow(doi_na)} reference{?s}")
   doi_na[["crossref_title"]] <- NA
   for (row in seq_len(nrow(doi_na))) {
     cli_alert_info("Query {row} of {nrow(doi_na)}")
     skip_to_next <- FALSE
     
     out <- tryCatch(
       get_missing_article_info(doi_na[["title"]][row], doi_na[["authors"]][row]),
       error = function(e) {
          cli_alert_danger("Error in query")
          skip_to_next <<- TRUE
       }
     )

     if (skip_to_next) {
       cli_alert_info("I got an error in query {row}. Skipping to next query.")
       next
     } else if (!is.null(out) && nrow(out) != 0) {
       doi_na[["doi"]][row] <- out$doi
       doi_na[["crossref_title"]][row] <- out$title
     }
   }
  ref_list <- rbind(doi_na, doi_not_na)
  ref_list
}

strip_doi_prefix <- function(doi) {
  doi <- trimws(as.character(doi))
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi[nchar(doi) == 0] <- NA_character_
  doi
}

doi_to_filename <- function(doi, ext = ".pdf") {
  doi <- strip_doi_prefix(doi)
  safe_name <- gsub("[^A-Za-z0-9]+", "_", doi)
  safe_name <- gsub("^_+|_+$", "", safe_name)
  paste0(safe_name, ext)
}

extract_crossref_pdf_url <- function(cr_data) {
  if (is.null(cr_data) || nrow(cr_data) == 0 || !"link" %in% colnames(cr_data)) {
    return(NA_character_)
  }

  link_info <- cr_data$link[[1]]
  if (is.null(link_info) || length(link_info) == 0) {
    return(NA_character_)
  }

  if (is.data.frame(link_info)) {
    if ("content.type" %in% names(link_info)) {
      idx <- which(tolower(link_info[["content.type"]]) == "application/pdf")[1]
      if (!is.na(idx) && "URL" %in% names(link_info)) {
        return(as.character(link_info[["URL"]][idx]))
      }
    }
    return(NA_character_)
  }

  if (is.list(link_info)) {
    content_types <- vapply(
      link_info,
      function(x) as.character(x[["content.type"]] %||% NA_character_),
      character(1)
    )
    pdf_idx <- which(tolower(content_types) == "application/pdf")[1]
    if (!is.na(pdf_idx)) {
      return(as.character(link_info[[pdf_idx]][["URL"]] %||% NA_character_))
    }
    return(NA_character_)
  }

  NA_character_
}

download_remote_file <- function(url, destfile) {
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
}

##' Download PDFs for a vector of DOIs
##'
##' This function attempts to resolve each DOI to a downloadable file URL using
##' Crossref metadata and saves the file in a user-specified directory.
##' It works best for DOIs with a PDF link exposed via Crossref. When no direct
##' file link is available, the DOI is reported as not downloadable.
##'
##' @param dois character vector of DOIs or doi.org URLs
##' @param outdir path to the directory where downloaded files should be saved
##' @param overwrite logical; overwrite existing files if TRUE
##' @return a data.frame with one row per DOI and columns describing the
##' download status, output path, and resolved URL
##' @importFrom cli cli_alert_danger cli_alert_info
##' @importFrom rcrossref cr_works
##' @export
download_doi_pdfs <- function(dois, outdir, overwrite = FALSE) {
  if (missing(dois) || length(dois) == 0) {
    cli_alert_danger("No DOIs supplied.")
    stop("`dois` must contain at least one DOI.", call. = FALSE)
  }

  if (missing(outdir) || !nzchar(outdir)) {
    cli_alert_danger("No output directory supplied.")
    stop("`outdir` must be provided.", call. = FALSE)
  }

  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(outdir)) {
    cli_alert_danger("Could not create output directory {.path {outdir}}.")
    stop("Output directory could not be created.", call. = FALSE)
  }

  normalized_dois <- strip_doi_prefix(dois)
  results <- vector("list", length(normalized_dois))

  for (i in seq_along(normalized_dois)) {
    doi <- normalized_dois[[i]]
    cli_alert_info("Processing DOI {i} of {length(normalized_dois)}: {doi %||% '<missing>'}")

    result <- data.frame(
      doi = doi,
      status = "failed",
      path = NA_character_,
      url = NA_character_,
      message = NA_character_,
      stringsAsFactors = FALSE
    )

    if (is.na(doi) || !nzchar(doi)) {
      result$message <- "DOI is missing or empty."
      results[[i]] <- result
      next
    }

    cr <- tryCatch(
      cr_works(dois = doi),
      error = function(e) e
    )
    if (inherits(cr, "error") || is.null(cr) || is.null(cr$data) || nrow(cr$data) == 0) {
      result$message <- "Crossref lookup failed or returned no records."
      results[[i]] <- result
      next
    }

    download_url <- extract_crossref_pdf_url(cr$data)
    result$url <- download_url
    if (is.na(download_url) || !nzchar(download_url)) {
      result$message <- "No downloadable PDF link found in Crossref metadata."
      results[[i]] <- result
      next
    }

    destfile <- file.path(outdir, doi_to_filename(doi))
    result$path <- destfile

    if (file.exists(destfile) && !isTRUE(overwrite)) {
      result$status <- "skipped"
      result$message <- "File already exists."
      results[[i]] <- result
      next
    }

    download_ok <- tryCatch(
      {
        download_remote_file(download_url, destfile)
        TRUE
      },
      error = function(e) {
        result$message <<- conditionMessage(e)
        FALSE
      }
    )

    if (!download_ok) {
      if (file.exists(destfile)) {
        unlink(destfile)
      }
      results[[i]] <- result
      next
    }

    result$status <- "downloaded"
    result$message <- "OK"
    results[[i]] <- result
  }

  do.call(rbind, results)
}
