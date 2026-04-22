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

##' Retrieve article metadata using Crossref or OpenAlex
##'
##' This function takes a character vector of DOI strings, DOI URLs, and/or
##' titles and queries the selected backend.
##' Open Access columns are sourced from OpenAlex; when `method = "crossref"`
##' these OA-specific columns are returned as `NA`.
##'
##' @param queries character vector of DOI strings, DOI URLs, and/or titles
##' @param method retrieval backend; one of "crossref" or "openalex"
##' @return a data.frame with one row per input query and columns:
##' input_query, query_type, doi, title, authors, year, journal, citation_count,
##' oa_is_oa, oa_status, oa_url, oa_pdf_url, oa_any_repository_has_fulltext,
##' status, and message
##' @importFrom rcrossref cr_works
##' @importFrom cli cli_alert_danger cli_alert_info
##' @author Sangeeta Bhatia
##' @export
get_missing_article_info <- function(queries, method) {
  if (missing(queries) || length(queries) == 0) {
    stop("`queries` must contain at least one DOI or title.", call. = FALSE)
  }
  if (missing(method) || !nzchar(method)) {
    stop("`method` must be explicitly supplied as 'crossref' or 'openalex'.", call. = FALSE)
  }

  method <- match.arg(method, c("crossref", "openalex"))
  if (method == "openalex") {
    return(get_missing_article_info_openalex(queries))
  }
  get_missing_article_info_crossref(queries)
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
##' @param method retrieval backend; one of "crossref" or "openalex"
##' @return the input data.frame with DOI updated where it could be retrirved
##' from rcrossref
##' @author Sangeeta Bhatia
##' @export
get_missing_article_info_ <- function(ref_list, method) {
   if (missing(method) || !nzchar(method)) {
     stop("`method` must be explicitly supplied as 'crossref' or 'openalex'.", call. = FALSE)
   }
   method <- match.arg(method, c("crossref", "openalex"))
   ## Now extract additional information for each reference
   ## doi is the key to everything; so if we have doi, we won;t
   ## bother querying the metadata API.
   doi_not_na <- ref_list[!is.na(ref_list$doi), ]
   doi_na <- ref_list[is.na(ref_list$doi), ]
   cli_alert_info("DOI available for {nrow(doi_not_na)} reference{?s}")
   cli_alert_info("DOI missing for {nrow(doi_na)} reference{?s}")
   doi_na[["retrieved_title"]] <- NA_character_
   if (!"retrieved_title" %in% colnames(doi_not_na)) {
     doi_not_na[["retrieved_title"]] <- NA_character_
   }

   if (nrow(doi_na) > 0) {
     out <- tryCatch(
       get_missing_article_info(doi_na[["title"]], method = method),
       error = function(e) {
         cli_alert_danger("Error in metadata query")
         cli_alert_info(conditionMessage(e))
         NULL
       }
     )

     if (!is.null(out) && nrow(out) > 0) {
       ok_idx <- which(out$status == "ok" & !is.na(out$doi) & nzchar(out$doi))
       if (length(ok_idx) > 0) {
         doi_na[["doi"]][ok_idx] <- out$doi[ok_idx]
         doi_na[["retrieved_title"]][ok_idx] <- out$title[ok_idx]
       }
     }
   }
  ref_list <- rbind(doi_na, doi_not_na)
  ref_list
}

strip_doi_prefix <- function(doi) {
  if (length(doi) == 0) {
    return(character(0))
  }
  original_na <- is.na(doi)
  doi <- trimws(as.character(doi))
  doi[original_na] <- NA_character_
  doi <- sub("^doi:\\s*", "", doi, ignore.case = TRUE)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi[!is.na(doi) & nchar(doi) == 0] <- NA_character_
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

looks_like_pdf_response <- function(final_url, content_type = NA_character_) {
  if (!is.na(final_url) && nzchar(final_url) && grepl("\\.pdf(\\?.*)?$", final_url, ignore.case = TRUE)) {
    return(TRUE)
  }
  if (!is.na(content_type) && nzchar(content_type)) {
    return(grepl("application/pdf", content_type, ignore.case = TRUE))
  }
  FALSE
}

url_looks_like_pdf <- function(x) {
  !is.na(x) && nzchar(x) && grepl("\\.pdf(\\?.*)?$", x, ignore.case = TRUE)
}

normalize_url_candidate <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  x <- trimws(as.character(x[[1]]))
  if (!nzchar(x)) {
    return(NA_character_)
  }
  x
}

candidate_list_for_row <- function(input_pdf_url, input_oa_url, oa_pdf_url, oa_url) {
  vals <- c(
    normalize_url_candidate(input_pdf_url),
    normalize_url_candidate(input_oa_url),
    normalize_url_candidate(oa_pdf_url),
    normalize_url_candidate(oa_url)
  )
  vals <- vals[!is.na(vals) & nzchar(vals)]
  unique(vals)
}

get_openalex_oa_metadata_by_doi <- function(dois) {
  out <- data.frame(
    doi = as.character(dois),
    oa_is_oa = as.logical(rep(NA, length(dois))),
    oa_status = NA_character_,
    oa_url = NA_character_,
    oa_pdf_url = NA_character_,
    stringsAsFactors = FALSE
  )

  valid <- !is.na(dois) & grepl("^10\\.[0-9]{4,9}/\\S+$", dois)
  valid_dois <- unique(tolower(dois[valid]))
  if (length(valid_dois) == 0) {
    return(out)
  }

  chunks <- split(valid_dois, ceiling(seq_along(valid_dois) / 100))
  lookup <- list()
  for (chunk in chunks) {
    filter_values <- paste0("https://doi.org/", chunk)
    filter_query <- paste(filter_values, collapse = "|")
    resp <- tryCatch(
      openalex_get_json(
        "/works",
        query = list(
          filter = paste0("doi:", filter_query),
          `per-page` = 100,
          select = "doi,open_access,best_oa_location"
        )
      ),
      error = function(e) e
    )
    if (inherits(resp, "error") || is.null(resp$results)) {
      next
    }
    for (work in resp$results) {
      norm_doi <- strip_doi_prefix(work$doi %||% NA_character_)
      key <- tolower(as.character(norm_doi))
      if (is.na(key) || !nzchar(key)) {
        next
      }
      oa <- work$open_access
      bol <- work$best_oa_location
      lookup[[key]] <- list(
        oa_is_oa = if (!is.null(oa$is_oa)) as.logical(oa$is_oa) else NA,
        oa_status = if (!is.null(oa$oa_status)) as.character(oa$oa_status) else NA_character_,
        oa_url = if (!is.null(oa$oa_url)) as.character(oa$oa_url) else NA_character_,
        oa_pdf_url = if (!is.null(bol$pdf_url)) as.character(bol$pdf_url) else NA_character_
      )
    }
  }

  for (i in seq_along(out$doi)) {
    key <- tolower(as.character(out$doi[[i]]))
    info <- lookup[[key]]
    if (is.null(info)) {
      next
    }
    out$oa_is_oa[[i]] <- info$oa_is_oa
    out$oa_status[[i]] <- info$oa_status
    out$oa_url[[i]] <- info$oa_url
    out$oa_pdf_url[[i]] <- info$oa_pdf_url
  }
  out
}

resolve_pdf_url_from_doi <- function(doi) {
  source_url <- paste0("https://doi.org/", doi)
  con <- url(source_url, open = "rb")
  on.exit(close(con), add = TRUE)

  suppressWarnings(readBin(con, what = "raw", n = 0))
  desc <- summary(con)$description
  headers <- suppressWarnings(readLines(con, n = 0, warn = FALSE))
  content_type <- NA_character_
  if (length(headers) > 0) {
    ct <- grep("^Content-Type\\s*:", headers, value = TRUE, ignore.case = TRUE)
    if (length(ct) > 0) {
      content_type <- trimws(sub("^[^:]+:\\s*", "", ct[[1]]))
    }
  }

  resolved_url <- if (is.null(desc) || !nzchar(desc)) source_url else as.character(desc)
  is_pdf <- looks_like_pdf_response(resolved_url, content_type)
  list(
    resolver = "doi",
    source_url = source_url,
    resolved_url = resolved_url,
    content_type = content_type,
    is_pdf = is_pdf
  )
}

##' Download PDFs for a vector of DOIs
##'
##' This function resolves each DOI to candidate file URLs using user-provided
##' OA URLs, OpenAlex OA metadata, and DOI resolution fallback, then attempts to
##' download PDFs to a user-specified directory.
##'
##' @param dois character vector of DOIs or doi.org URLs
##' @param outdir path to the directory where downloaded files should be saved
##' @param oa_pdf_urls optional character vector of OA PDF URLs aligned with
##' `dois`
##' @param oa_urls optional character vector of OA landing-page URLs aligned with
##' `dois`
##' @param overwrite logical; overwrite existing files if TRUE
##' @param method resolver strategy. Supported values are "auto",
##' "openalex_then_doi", and "doi_only"
##' @return a data.frame with one row per DOI and columns describing the
##' download status, output path, resolver, resolved URLs, and OA metadata
##' @importFrom cli cli_alert_danger cli_alert_info
##' @export
download_doi_pdfs <- function(
  dois,
  outdir,
  oa_pdf_urls = NULL,
  oa_urls = NULL,
  overwrite = FALSE,
  method = c("auto", "openalex_then_doi", "doi_only")
) {
  if (missing(dois) || length(dois) == 0) {
    cli_alert_danger("No DOIs supplied.")
    stop("`dois` must contain at least one DOI.", call. = FALSE)
  }
  method <- match.arg(method)
  n <- length(dois)
  if (!is.null(oa_pdf_urls) && length(oa_pdf_urls) != n) {
    stop("`oa_pdf_urls` must be NULL or have the same length as `dois`.", call. = FALSE)
  }
  if (!is.null(oa_urls) && length(oa_urls) != n) {
    stop("`oa_urls` must be NULL or have the same length as `dois`.", call. = FALSE)
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
  oa_pdf_urls <- if (is.null(oa_pdf_urls)) rep(NA_character_, n) else as.character(oa_pdf_urls)
  oa_urls <- if (is.null(oa_urls)) rep(NA_character_, n) else as.character(oa_urls)
  oa_meta <- data.frame(
    doi = normalized_dois,
    oa_is_oa = as.logical(rep(NA, n)),
    oa_status = NA_character_,
    oa_url = NA_character_,
    oa_pdf_url = NA_character_,
    stringsAsFactors = FALSE
  )
  if (method != "doi_only") {
    oa_meta <- get_openalex_oa_metadata_by_doi(normalized_dois)
  }
  results <- vector("list", length(normalized_dois))

  for (i in seq_along(normalized_dois)) {
    doi <- normalized_dois[[i]]
    cli_alert_info("Processing DOI {i} of {length(normalized_dois)}: {doi %||% '<missing>'}")

    result <- data.frame(
      doi = doi,
      status = "failed",
      path = NA_character_,
      resolver_used = NA_character_,
      resolved_url = NA_character_,
      source_url = NA_character_,
      input_oa_pdf_url = normalize_url_candidate(oa_pdf_urls[[i]]),
      input_oa_url = normalize_url_candidate(oa_urls[[i]]),
      oa_is_oa = oa_meta$oa_is_oa[[i]],
      oa_status = oa_meta$oa_status[[i]],
      oa_url = oa_meta$oa_url[[i]],
      oa_pdf_url = oa_meta$oa_pdf_url[[i]],
      message = NA_character_,
      stringsAsFactors = FALSE
    )

    if (is.na(doi) || !grepl("^10\\.[0-9]{4,9}/\\S+$", doi)) {
      result$message <- "DOI is missing or invalid."
      results[[i]] <- result
      next
    }
    candidates <- candidate_list_for_row(
      result$input_oa_pdf_url,
      result$input_oa_url,
      result$oa_pdf_url,
      result$oa_url
    )

    selected_url <- NA_character_
    selected_resolver <- NA_character_
    if (length(candidates) > 0) {
      for (cand in candidates) {
        if (url_looks_like_pdf(cand)) {
          selected_url <- cand
          selected_resolver <- "oa_candidate"
          break
        }
      }
    }

    if ((is.na(selected_url) || !nzchar(selected_url))) {
      resolved <- tryCatch(
        resolve_pdf_url_from_doi(doi),
        error = function(e) e
      )
      if (inherits(resolved, "error")) {
        result$message <- paste0("DOI resolution failed: ", conditionMessage(resolved))
        results[[i]] <- result
        next
      }
      if (isTRUE(resolved$is_pdf) && !is.null(resolved$resolved_url) && nzchar(resolved$resolved_url)) {
        selected_url <- as.character(resolved$resolved_url)
        selected_resolver <- "doi"
      }
    }

    if (is.na(selected_url) || !nzchar(selected_url)) {
      result$message <- "No downloadable PDF URL was identified from OA URLs or DOI resolution."
      results[[i]] <- result
      next
    }
    result$resolved_url <- selected_url
    result$source_url <- selected_url
    result$resolver_used <- selected_resolver

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
        download_remote_file(selected_url, destfile)
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

is_doi_query <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(FALSE)
  }
  x <- trimws(x)
  grepl("^https?://(dx\\.)?doi\\.org/.+", x, ignore.case = TRUE) ||
    grepl("^10\\.[0-9]{4,9}/\\S+$", x, ignore.case = TRUE)
}

new_metadata_result_df <- function(queries) {
  n <- length(queries)
  data.frame(
    input_query = as.character(queries),
    query_type = ifelse(vapply(queries, is_doi_query, logical(1)), "doi", "title"),
    doi = NA_character_,
    title = NA_character_,
    authors = I(replicate(n, character(0), simplify = FALSE)),
    year = as.integer(rep(NA_integer_, n)),
    journal = NA_character_,
    citation_count = as.integer(rep(NA_integer_, n)),
    oa_is_oa = as.logical(rep(NA, n)),
    oa_status = NA_character_,
    oa_url = NA_character_,
    oa_pdf_url = NA_character_,
    oa_any_repository_has_fulltext = as.logical(rep(NA, n)),
    status = "pending",
    message = NA_character_,
    stringsAsFactors = FALSE
  )
}

update_metadata_row <- function(out, idx, fields, status = "ok", message = "OK") {
  out$doi[[idx]] <- if (is.null(fields$doi) || length(fields$doi) == 0) NA_character_ else fields$doi
  out$title[[idx]] <- if (is.null(fields$title) || length(fields$title) == 0) NA_character_ else fields$title
  out$authors[[idx]] <- if (is.null(fields$authors) || length(fields$authors) == 0) character(0) else as.character(fields$authors)
  out$year[[idx]] <- if (is.null(fields$year) || length(fields$year) == 0) NA_integer_ else as.integer(fields$year)
  out$journal[[idx]] <- if (is.null(fields$journal) || length(fields$journal) == 0) NA_character_ else fields$journal
  out$citation_count[[idx]] <- if (is.null(fields$citation_count) || length(fields$citation_count) == 0) NA_integer_ else as.integer(fields$citation_count)
  out$oa_is_oa[[idx]] <- if (is.null(fields$oa_is_oa) || length(fields$oa_is_oa) == 0) NA else as.logical(fields$oa_is_oa)
  out$oa_status[[idx]] <- if (is.null(fields$oa_status) || length(fields$oa_status) == 0) NA_character_ else as.character(fields$oa_status)
  out$oa_url[[idx]] <- if (is.null(fields$oa_url) || length(fields$oa_url) == 0) NA_character_ else as.character(fields$oa_url)
  out$oa_pdf_url[[idx]] <- if (is.null(fields$oa_pdf_url) || length(fields$oa_pdf_url) == 0) NA_character_ else as.character(fields$oa_pdf_url)
  out$oa_any_repository_has_fulltext[[idx]] <- if (
    is.null(fields$oa_any_repository_has_fulltext) ||
      length(fields$oa_any_repository_has_fulltext) == 0
  ) {
    NA
  } else {
    as.logical(fields$oa_any_repository_has_fulltext)
  }
  out$status[[idx]] <- status
  out$message[[idx]] <- message
  out
}

build_url_query <- function(query) {
  if (length(query) == 0) {
    return("")
  }
  parts <- mapply(
    function(k, v) {
      paste0(
        utils::URLencode(k, reserved = TRUE),
        "=",
        utils::URLencode(as.character(v), reserved = TRUE)
      )
    },
    names(query),
    query,
    USE.NAMES = FALSE
  )
  paste(parts, collapse = "&")
}

openalex_get_json <- function(endpoint, query = list()) {
  base_url <- "https://api.openalex.org"
  query_string <- build_url_query(query)
  url <- paste0(base_url, endpoint)
  if (nzchar(query_string)) {
    url <- paste0(url, "?", query_string)
  }

  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  response_txt <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  jsonlite::fromJSON(response_txt, simplifyVector = FALSE)
}

openalex_extract_authors <- function(work) {
  authorships <- work$authorships
  if (is.null(authorships) || length(authorships) == 0) {
    return(character(0))
  }
  authors <- vapply(
    authorships,
    function(x) {
      author <- x$author
      if (is.null(author) || is.null(author$display_name) || !nzchar(author$display_name)) {
        return(NA_character_)
      }
      as.character(author$display_name)
    },
    character(1)
  )
  authors[!is.na(authors) & nzchar(authors)]
}

openalex_extract_journal <- function(work) {
  primary_location <- work$primary_location
  if (is.null(primary_location) || is.null(primary_location$source)) {
    return(NA_character_)
  }
  source_name <- primary_location$source$display_name
  if (is.null(source_name) || !nzchar(source_name)) {
    return(NA_character_)
  }
  as.character(source_name)
}

openalex_work_to_fields <- function(work) {
  doi_val <- work$doi
  if (is.null(doi_val) || length(doi_val) == 0) doi_val <- NA_character_
  title_val <- work$display_name
  if (is.null(title_val) || length(title_val) == 0) title_val <- NA_character_
  year_val <- work$publication_year
  if (is.null(year_val) || length(year_val) == 0) year_val <- NA_integer_
  citation_count_val <- work$cited_by_count
  if (is.null(citation_count_val) || length(citation_count_val) == 0) citation_count_val <- NA_integer_
  open_access <- work$open_access
  best_oa_location <- work$best_oa_location

  oa_is_oa_val <- NA
  oa_status_val <- NA_character_
  oa_url_val <- NA_character_
  oa_any_repository_has_fulltext_val <- NA
  oa_pdf_url_val <- NA_character_

  if (!is.null(open_access)) {
    oa_is_oa_val <- if (!is.null(open_access$is_oa)) as.logical(open_access$is_oa) else NA
    oa_status_val <- if (!is.null(open_access$oa_status)) as.character(open_access$oa_status) else NA_character_
    oa_url_val <- if (!is.null(open_access$oa_url)) as.character(open_access$oa_url) else NA_character_
    oa_any_repository_has_fulltext_val <- if (!is.null(open_access$any_repository_has_fulltext)) {
      as.logical(open_access$any_repository_has_fulltext)
    } else {
      NA
    }
  }
  if (!is.null(best_oa_location) && !is.null(best_oa_location$pdf_url)) {
    oa_pdf_url_val <- as.character(best_oa_location$pdf_url)
  }

  list(
    doi = strip_doi_prefix(doi_val),
    title = as.character(title_val),
    authors = openalex_extract_authors(work),
    year = as.integer(year_val),
    journal = openalex_extract_journal(work),
    citation_count = as.integer(citation_count_val),
    oa_is_oa = oa_is_oa_val,
    oa_status = oa_status_val,
    oa_url = oa_url_val,
    oa_pdf_url = oa_pdf_url_val,
    oa_any_repository_has_fulltext = oa_any_repository_has_fulltext_val
  )
}

get_missing_article_info_openalex <- function(queries) {
  queries <- as.character(queries)
  out <- new_metadata_result_df(queries)

  doi_idx <- which(out$query_type == "doi")
  title_idx <- which(out$query_type == "title")

  if (length(doi_idx) > 0) {
    out <- get_missing_article_info_openalex_by_doi(out, doi_idx)
  }
  if (length(title_idx) > 0) {
    out <- get_missing_article_info_openalex_by_title(out, title_idx)
  }

  pending_idx <- which(out$status == "pending")
  if (length(pending_idx) > 0) {
    out$status[pending_idx] <- "failed"
    out$message[pending_idx] <- "Unprocessed query."
  }
  out
}

get_missing_article_info_openalex_by_doi <- function(out, doi_idx) {
  normalized_dois <- tolower(strip_doi_prefix(trimws(out$input_query[doi_idx])))
  invalid_doi <- is.na(normalized_dois) | !grepl("^10\\.[0-9]{4,9}/\\S+$", normalized_dois)
  if (any(invalid_doi)) {
    bad_rows <- doi_idx[invalid_doi]
    out$status[bad_rows] <- "failed"
    out$message[bad_rows] <- "DOI is missing or invalid."
  }

  valid_doi_idx <- doi_idx[!invalid_doi]
  valid_dois <- normalized_dois[!invalid_doi]
  if (length(valid_doi_idx) == 0) {
    return(out)
  }

  unique_valid_dois <- unique(valid_dois)
  # Keep OR-batched DOI filters below conservative URL size limits.
  chunks <- list(character(0))
  current_idx <- 1L
  current_len <- 0L
  for (d in unique_valid_dois) {
    token <- paste0("https://doi.org/", d)
    token_len <- nchar(token, type = "bytes")
    sep_len <- if (length(chunks[[current_idx]]) > 0) 1L else 0L
    if (
      length(chunks[[current_idx]]) >= 100 ||
      (current_len + sep_len + token_len) > 1800L
    ) {
      current_idx <- current_idx + 1L
      chunks[[current_idx]] <- character(0)
      current_len <- 0L
      sep_len <- 0L
    }
    chunks[[current_idx]] <- c(chunks[[current_idx]], d)
    current_len <- current_len + sep_len + token_len
  }
  doi_lookup <- list()

  for (chunk in chunks) {
    filter_values <- paste0("https://doi.org/", chunk)
    filter_query <- paste(filter_values, collapse = "|")
    resp <- tryCatch(
      openalex_get_json(
        "/works",
        query = list(
          filter = paste0("doi:", filter_query),
          `per-page` = 100,
          select = "doi,display_name,authorships,publication_year,primary_location,cited_by_count,open_access,best_oa_location"
        )
      ),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      failed_rows <- valid_doi_idx[valid_dois %in% chunk]
      out$status[failed_rows] <- "failed"
      out$message[failed_rows] <- paste0("OpenAlex DOI lookup failed: ", conditionMessage(resp))
      next
    }

    results <- resp$results
    if (is.null(results) || length(results) == 0) {
      next
    }
    for (work in results) {
      fields <- openalex_work_to_fields(work)
      key <- tolower(as.character(fields$doi[[1]]))
      if (!is.na(key) && nzchar(key)) {
        doi_lookup[[key]] <- fields
      }
    }
  }

  for (i in seq_along(valid_doi_idx)) {
    row_idx <- valid_doi_idx[[i]]
    key <- valid_dois[[i]]
    if (identical(out$status[[row_idx]], "failed")) {
      next
    }
    fields <- doi_lookup[[key]]
    if (is.null(fields)) {
      out$status[[row_idx]] <- "not_found"
      out$message[[row_idx]] <- "No OpenAlex match found for DOI."
    } else {
      out <- update_metadata_row(out, row_idx, fields, status = "ok", message = "OK")
    }
  }
  out
}

get_missing_article_info_openalex_by_title <- function(out, title_idx) {
  for (row_idx in title_idx) {
    title_query <- trimws(out$input_query[[row_idx]])
    if (is.na(title_query) || !nzchar(title_query)) {
      out$status[[row_idx]] <- "failed"
      out$message[[row_idx]] <- "Title query is missing or empty."
      next
    }

    resp <- tryCatch(
      openalex_get_json(
        "/works",
        query = list(
          search = title_query,
          `per-page` = 1,
          select = "doi,display_name,authorships,publication_year,primary_location,cited_by_count,open_access,best_oa_location"
        )
      ),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      out$status[[row_idx]] <- "failed"
      out$message[[row_idx]] <- paste0("OpenAlex title lookup failed: ", conditionMessage(resp))
      next
    }

    results <- resp$results
    if (is.null(results) || length(results) == 0) {
      out$status[[row_idx]] <- "not_found"
      out$message[[row_idx]] <- "No OpenAlex match found for title."
      next
    }

    out <- update_metadata_row(
      out,
      row_idx,
      openalex_work_to_fields(results[[1]]),
      status = "ok",
      message = "OK"
    )
  }
  out
}

crossref_first_existing_column <- function(x, candidates) {
  col <- candidates[candidates %in% names(x)][1]
  if (is.na(col) || !nzchar(col)) {
    return(NULL)
  }
  x[[col]]
}

crossref_extract_year <- function(row) {
  year_cols <- c("published.print", "published.online", "issued", "created")
  for (col in year_cols) {
    if (!col %in% names(row)) {
      next
    }
    val <- row[[col]]
    if (is.list(val) && length(val) > 0) {
      date_parts <- val[[1]][["date-parts"]]
      if (!is.null(date_parts) && length(date_parts) > 0 && length(date_parts[[1]]) > 0) {
        return(as.integer(date_parts[[1]][1]))
      }
    }
  }
  NA_integer_
}

crossref_extract_authors <- function(row) {
  if (!"author" %in% names(row)) {
    return(character(0))
  }
  author_obj <- row[["author"]]
  if (is.null(author_obj) || length(author_obj) == 0) {
    return(character(0))
  }
  if (is.data.frame(author_obj)) {
    fam <- if ("family" %in% names(author_obj)) as.character(author_obj$family) else NA_character_
    giv <- if ("given" %in% names(author_obj)) as.character(author_obj$given) else NA_character_
    full <- trimws(paste(giv, fam))
    full <- full[!is.na(full) & nzchar(full)]
    return(full)
  }
  if (is.list(author_obj)) {
    full <- vapply(
      author_obj,
      function(a) {
        if (is.data.frame(a)) {
          family <- if ("family" %in% names(a)) as.character(a$family[[1]]) else ""
          given <- if ("given" %in% names(a)) as.character(a$given[[1]]) else ""
          return(trimws(paste(given, family)))
        }
        if (!is.null(a$family) || !is.null(a$given)) {
          return(trimws(paste(a$given %||% "", a$family %||% "")))
        }
        NA_character_
      },
      character(1)
    )
    return(full[!is.na(full) & nzchar(full)])
  }
  character(0)
}

crossref_row_to_fields <- function(row) {
  doi_val <- crossref_first_existing_column(row, c("doi", "DOI"))
  title_val <- crossref_first_existing_column(row, c("title"))
  journal_val <- crossref_first_existing_column(row, c("container.title", "container_title"))
  citation_val <- crossref_first_existing_column(
    row,
    c("is.referenced.by.count", "is_referenced_by_count", "is-referenced-by-count")
  )

  if (is.list(title_val) && length(title_val) > 0) title_val <- title_val[[1]]
  if (is.list(journal_val) && length(journal_val) > 0) journal_val <- journal_val[[1]]

  list(
    doi = strip_doi_prefix(as.character(doi_val %||% NA_character_)),
    title = as.character(title_val %||% NA_character_),
    authors = crossref_extract_authors(row),
    year = crossref_extract_year(row),
    journal = as.character(journal_val %||% NA_character_),
    citation_count = as.integer(citation_val %||% NA_integer_),
    oa_is_oa = NA,
    oa_status = NA_character_,
    oa_url = NA_character_,
    oa_pdf_url = NA_character_,
    oa_any_repository_has_fulltext = NA
  )
}

get_missing_article_info_crossref <- function(queries) {
  queries <- as.character(queries)
  out <- new_metadata_result_df(queries)

  doi_idx <- which(out$query_type == "doi")
  title_idx <- which(out$query_type == "title")

  if (length(doi_idx) > 0) {
    out <- get_missing_article_info_crossref_by_doi(out, doi_idx)
  }
  if (length(title_idx) > 0) {
    out <- get_missing_article_info_crossref_by_title(out, title_idx)
  }

  pending_idx <- which(out$status == "pending")
  if (length(pending_idx) > 0) {
    out$status[pending_idx] <- "failed"
    out$message[pending_idx] <- "Unprocessed query."
  }
  out
}

get_missing_article_info_crossref_by_doi <- function(out, doi_idx) {
  for (row_idx in doi_idx) {
    doi_query <- strip_doi_prefix(trimws(out$input_query[[row_idx]]))
    if (is.na(doi_query) || !nzchar(doi_query)) {
      out$status[[row_idx]] <- "failed"
      out$message[[row_idx]] <- "DOI is missing or invalid."
      next
    }

    cr <- tryCatch(
      cr_works(dois = doi_query),
      error = function(e) e
    )
    if (inherits(cr, "error")) {
      out$status[[row_idx]] <- "failed"
      out$message[[row_idx]] <- paste0("Crossref DOI lookup failed: ", conditionMessage(cr))
      next
    }
    if (is.null(cr$data) || nrow(cr$data) == 0) {
      out$status[[row_idx]] <- "not_found"
      out$message[[row_idx]] <- "No Crossref match found for DOI."
      next
    }

    fields <- crossref_row_to_fields(cr$data[1, , drop = FALSE])
    out <- update_metadata_row(out, row_idx, fields, status = "ok", message = "OK")
  }
  out
}

get_missing_article_info_crossref_by_title <- function(out, title_idx) {
  for (row_idx in title_idx) {
    title_query <- trimws(out$input_query[[row_idx]])
    if (is.na(title_query) || !nzchar(title_query)) {
      out$status[[row_idx]] <- "failed"
      out$message[[row_idx]] <- "Title query is missing or empty."
      next
    }

    cr <- tryCatch(
      cr_works(
        flq = c(query.bibliographic = title_query),
        limit = 1,
        select = c(
          "DOI",
          "title",
          "author",
          "container-title",
          "published-print",
          "published-online",
          "issued",
          "created",
          "is-referenced-by-count"
        )
      ),
      error = function(e) e
    )
    if (inherits(cr, "error")) {
      out$status[[row_idx]] <- "failed"
      out$message[[row_idx]] <- paste0("Crossref title lookup failed: ", conditionMessage(cr))
      next
    }
    if (is.null(cr$data) || nrow(cr$data) == 0) {
      out$status[[row_idx]] <- "not_found"
      out$message[[row_idx]] <- "No Crossref match found for title."
      next
    }

    fields <- crossref_row_to_fields(cr$data[1, , drop = FALSE])
    out <- update_metadata_row(out, row_idx, fields, status = "ok", message = "OK")
  }
  out
}

##' Retrieve study details from OpenAlex using DOI or title
##'
##' Convenience wrapper around \code{\link{get_missing_article_info}} with
##' `method = "openalex"`.
##'
##' @param queries character vector of DOIs, DOI URLs, and/or titles
##' @return data.frame with columns input_query, query_type, doi, title, authors,
##' year, journal, citation_count, oa_is_oa, oa_status, oa_url, oa_pdf_url,
##' oa_any_repository_has_fulltext, status, and message
##' @importFrom jsonlite fromJSON
##' @export
get_study_details_openalex <- function(queries) {
  get_missing_article_info(queries, method = "openalex")
}
