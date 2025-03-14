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

##' Retrieve additional information for references using crossref
##'
##' Automated extraction leaves a lot wanting. The only thing we can reliably
##' extract is the title. We can use this title to query crossref and get
##' additional information. This function takes a data.frame of references and
##' queries crossref for additional information. It returns a data.frame with
##' 
##' @param title 
##' @param x data.frame of references. Must have columns "doi" and "title".
##' @return
##' @importFrom rcrossref cr_works
##' @author Sangeeta Bhatia
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
