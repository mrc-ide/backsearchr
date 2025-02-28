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

compare_dois <- funcion(x, y) {
  ## Ensure only non-NA values are compared
  ## Remove htttps://doi.org/ from the beginning of the DOI
  x$doi %in% y$doi
}
