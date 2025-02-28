##' Compare two lists of references to identify references that are in one list
##' but not the other
##'
##' @details Compares two lists of references to identify references that are in
##' one list but not the other. We first check if any dois in `these_refs` are
##' in `against_these`. Then we chec if any titles in `these_refs` are in
##' `against_these`. Titles are compared after pre-processing them with
##' \code{\link{cleanup_strings}}. If the list `against_these` has been generated
##' by \code{\link{extract_references}}, the titles will already be pre-processed.
##' 
##' @title Compare reference lists
##' @param these_refs data frame with columns `doi`, `title`, `author`, `year`
##' and `journal`. This dataframe will typically be the user input e.g. a list of
##' references that satisfy the inclusion criteria in a systematic review.
##' @param against_these a data frame with columns `doi`, `title`, `author`, `year`
##' and `journal`. This dataframe will typically be the reference list extracted
##' from a set of papers e.g. using \code{\link{extract_references}}.
##' @return 
##' @author Sangeeta Bhatia
##' @seealso \code{\link{extract_references}}, \code{\link{cleanup_strings}}
##' @export
compare_reference_lists <- function(these_refs, against_these) {

  ## First ensure that the two data frames have the necessary columns
  cols_needed <- c("doi", "title")
  missing_cols <- setdiff(cols_needed, colnames(these_refs))
  if (length(missing_cols) > 0) {
    cli_alert_danger("I did not find all the necessary columns in `these_refs`.")
    cli_alert_info(
      "Missing column{?s} are: {paste(missing_cols, collapse = ', ')}"
    )
    return(NULL)
  }
  these_refs$cleanup_title <- cleanup_strings(these_refs$title)
  ## We will only compare non-NA dois
  compare_dois <- function(x, y) {
    if (is.na(x) | is.na(y)) {
      return(FALSE)
    }
    identical(x, y)
  }
  ## Check if dois in these_refs are in against_these
  dois_in_against_these <- these_refs$doi %in% against_these$doi
  cli_alert_info(
    "{sum(dois_in_against_these)} DOI{?s} in `these_refs` are in `against_these`."
  )
  dois_matched <- these_refs[dois_in_against_these, ]
  dois_not_in_against_these <- these_refs[!dois_in_against_these, ]

  ## Compare titles only for those rows where a DOI match was not found
  ## Check if titles in these_refs are in against_these
  
  titles_in_against_these <- dois_not_in_against_these$cleanup_title %in% against_these$cleanup_title
  cli_alert_info(
    "{sum(titles_in_against_these)} title{?s} in `these_refs` are in `against_these`."
  )
  titles_matched <- these_refs[titles_in_against_these, ]
  titles_not_in_against_these <- these_refs[!titles_in_against_these, ]

  neither_matched <- rbind(dois_not_in_against_these, titles_not_in_against_these)
  list(
    dois_matched = dois_matched,
    titles_matched = titles_matched,
    neither_matched = neither_matched
  )
}
