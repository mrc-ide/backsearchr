##' Compare two lists of references to identify references that are in one list
##' but not the other
##'
##' @details Compares two lists of references to identify references that are in
##' one list but not the other. We first check if any dois in `extracted_refs`
##' are in `provided_refs`. Then we check if any titles in `extracted_refs` are
##' in `provided_refs`. Titles are compared after pre-processing them with
##' \code{\link{cleanup_strings}}. If the list `extracted_refs` has been generated
##' by \code{\link{extract_references}}, the titles will already be pre-processed.
##'
##' @title Compare reference lists
##' @param provided_refs data frame with columns `doi`, `title`, `author`, `year`
##' and `journal`. This dataframe will typically be the user input e.g. a list of
##' references that satisfy the inclusion criteria in a systematic review.
##' @param extracted_refs a data frame with columns `doi`, `title`, `author`, `year`
##' and `journal`. This dataframe will typically be the reference list extracted
##' from a set of papers e.g. using \code{\link{extract_references}}.
##' @importFrom cli cli_alert_danger cli_alert_info
##' @return
##' @author Sangeeta Bhatia
##' @seealso \code{\link{extract_references}}, \code{\link{cleanup_strings}}
##' @export
compare_reference_lists <- function(provided_refs, extracted_refs) {
  provided_input <- provided_refs
  extracted_input <- extracted_refs
  provided_refs$.provided_row_id <- seq_len(nrow(provided_refs))
  extracted_refs$.extracted_row_id <- seq_len(nrow(extracted_refs))

  # First ensure that the two data frames have the necessary columns.
  cols_needed <- c("doi", "title")
  missing_cols <- setdiff(cols_needed, colnames(provided_refs))
  if (length(missing_cols) > 0) {
    cli_alert_danger("I did not find all the necessary columns in `provided_refs`.")
    cli_alert_info(
      "Missing column{?s} are: {paste(missing_cols, collapse = ', ')}"
    )
    return(NULL)
  }
  missing_cols <- setdiff(cols_needed, colnames(extracted_refs))
  if (length(missing_cols) > 0) {
    cli_alert_danger("I did not find all the necessary columns in `extracted_refs`.")
    cli_alert_info(
      "Missing column{?s} are: {paste(missing_cols, collapse = ', ')}"
    )
    return(NULL)
  }

  normalize_doi <- function(x) {
    x <- trimws(tolower(as.character(x)))
    x[is.na(x) | x == ""] <- NA
    x
  }

  provided_refs$doi <- normalize_doi(provided_refs$doi)
  extracted_refs$doi <- normalize_doi(extracted_refs$doi)

  provided_refs$cleanup_title <- cleanup_strings(provided_refs$title)
  extracted_refs$cleanup_title <- cleanup_strings(extracted_refs$title)

  # First compare DOIs.
  provided_doi <- provided_refs[
    !is.na(provided_refs$doi),
    c(".provided_row_id", "doi", "title")
  ]
  extracted_doi <- extracted_refs[
    !is.na(extracted_refs$doi),
    c(".extracted_row_id", "doi", "title")
  ]
  doi_pairs <- merge(provided_doi, extracted_doi, by = "doi")
  doi_matched_ids <- unique(doi_pairs$.extracted_row_id)
  cli_alert_info(
    "{length(doi_matched_ids)} extracted reference{?s} matched by DOI."
  )

  # For remaining extracted rows, compare cleaned titles.
  remaining_extracted_ids <- extracted_refs$.extracted_row_id[
    !extracted_refs$.extracted_row_id %in% doi_matched_ids
  ]
  extracted_title <- extracted_refs[
    extracted_refs$.extracted_row_id %in% remaining_extracted_ids &
      !is.na(extracted_refs$cleanup_title),
    c(".extracted_row_id", "cleanup_title", "title")
  ]
  provided_title <- provided_refs[
    !is.na(provided_refs$cleanup_title),
    c(".provided_row_id", "cleanup_title", "title")
  ]
  title_pairs <- merge(extracted_title, provided_title, by = "cleanup_title")
  title_matched_ids <- unique(title_pairs$.extracted_row_id)
  cli_alert_info(
    "{length(title_matched_ids)} extracted reference{?s} matched by title."
  )

  summarize_pairs <- function(pairs) {
    if (nrow(pairs) == 0) {
      return(data.frame(
        .extracted_row_id = integer(0),
        matched_provided_row_ids = character(0),
        matched_provided_titles = character(0),
        stringsAsFactors = FALSE
      ))
    }
    provided_title_col <- if ("title.y" %in% colnames(pairs)) "title.y" else "title.x"
    split_pairs <- split(pairs, pairs$.extracted_row_id)
    data.frame(
      .extracted_row_id = as.integer(names(split_pairs)),
      matched_provided_row_ids = vapply(
        split_pairs,
        function(x) paste(sort(unique(x$.provided_row_id)), collapse = "|"),
        character(1)
      ),
      matched_provided_titles = vapply(
        split_pairs,
        function(x) {
          paste(sort(unique(as.character(x[[provided_title_col]]))), collapse = " | ")
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  doi_summary <- summarize_pairs(doi_pairs)
  title_summary <- summarize_pairs(title_pairs)

  matched_ids <- unique(c(doi_matched_ids, title_matched_ids))
  matched_df <- extracted_input[
    extracted_refs$.extracted_row_id %in% matched_ids,
    , drop = FALSE
  ]
  matched_df$match_type <- ifelse(
    extracted_refs$.extracted_row_id[extracted_refs$.extracted_row_id %in% matched_ids] %in%
      doi_matched_ids, "doi", "title"
  )
  matched_df$matched_provided_row_ids <- NA_character_
  matched_df$matched_provided_titles <- NA_character_

  if (nrow(matched_df) > 0) {
    matched_ids_in_order <- extracted_refs$.extracted_row_id[
      extracted_refs$.extracted_row_id %in% matched_ids
    ]
    doi_idx <- match(matched_ids_in_order, doi_summary$.extracted_row_id)
    title_idx <- match(matched_ids_in_order, title_summary$.extracted_row_id)
    has_doi <- !is.na(doi_idx)
    has_title <- !is.na(title_idx)
    matched_df$matched_provided_row_ids[has_doi] <- doi_summary$matched_provided_row_ids[doi_idx[has_doi]]
    matched_df$matched_provided_titles[has_doi] <- doi_summary$matched_provided_titles[doi_idx[has_doi]]
    matched_df$matched_provided_row_ids[!has_doi & has_title] <- title_summary$matched_provided_row_ids[title_idx[!has_doi & has_title]]
    matched_df$matched_provided_titles[!has_doi & has_title] <- title_summary$matched_provided_titles[title_idx[!has_doi & has_title]]
  }

  not_matched <- extracted_input[
    !extracted_refs$.extracted_row_id %in% matched_ids,
    , drop = FALSE
  ]

  doi_matched_df <- matched_df[matched_df$match_type == "doi", , drop = FALSE]
  titles_matched_df <- matched_df[matched_df$match_type == "title", , drop = FALSE]

  list(
    doi_matched = doi_matched_df,
    titles_matched = titles_matched_df,
    matched = matched_df,
    not_matched = not_matched
  )
}
