get_fr <- function(doc_nums, fields = c('abstract', 'action', 'agencies', 'agency_names', 'body_html_url', 'cfr_references', 'citation', 'comment_url', 'comments_close_on', 'correction_of', 'corrections', 'dates', 'docket_id', 'docket_ids', 'document_number', 'effective_on', 'end_page', 'excerpts', 'executive_order_notes', 'executive_order_number', 'full_text_xml_url', 'html_url', 'images', 'json_url', 'mods_url', 'page_length', 'pdf_url', 'president', 'public_inspection_pdf_url', 'publication_date', 'raw_text_url', 'regulation_id_number_info', 'regulation_id_numbers', 'regulations_dot_gov_info', 'regulations_dot_gov_url', 'significant', 'signing_date', 'start_page', 'subtype', 'title', 'toc_doc', 'toc_subject', 'topics', 'type', 'volume'), output = "tidy") {
  base_url <- "www.federalregister.gov/api/v1/documents/"
  if (all(nchar(doc_nums) > 9,
          output %in% c("tidy", "data.frame", "list"),
          all(fields %in% c('abstract', 'action', 'agencies', 'agency_names', 'body_html_url', 'cfr_references', 'citation', 'comment_url', 'comments_close_on', 'correction_of', 'corrections', 'dates', 'docket_id', 'docket_ids', 'document_number', 'effective_on', 'end_page', 'excerpts', 'executive_order_notes', 'executive_order_number', 'full_text_xml_url', 'html_url', 'images', 'json_url', 'mods_url', 'page_length', 'pdf_url', 'president', 'public_inspection_pdf_url', 'publication_date', 'raw_text_url', 'regulation_id_number_info', 'regulation_id_numbers', 'regulations_dot_gov_info', 'regulations_dot_gov_url', 'significant', 'signing_date', 'start_page', 'subtype', 'title', 'toc_doc', 'toc_subject', 'topics', 'type', 'volume'))
          )) {
    content <- httr::GET(paste0(base_url,
                                ifelse(all(nchar(doc_nums) == 10,length(doc_nums) == 1), paste(doc_nums, doc_nums, sep = ","), paste(doc_nums, collapse = ",")),
                                ".json?fields%5B%5D=",
                                paste(fields, collapse = "&fields%5B%5D=")))
    if (content$status == 200) {
      content <- jsonlite::fromJSON(rawToChar(content$content))
      if (is.null(content$errors)) {
        if (output != "list") {
          count <- content$count
          content <- content$results
          if (output == "tidy" & !is.null(content$regulations_dot_gov_info$supporting_documents)) {
            content$regulations_dot_gov_info <- NULL #WAYY too complicated for one rarely-used field
            if (length(content[[i]]) == 0) {content[[i]] <- NA}
            for (i in names(content[, sapply(content, is.list)])) {
              for (j in seq_along(content[[i]])) {
                if (is.data.frame(content[[i]])) {
                  q <- paste(i, names(content[[i]][j]), sep = ".")
                  if (is.null(content[[q]])) {content[[q]] <- NA}
                  content[[q]] <- content[[i]][[j]]
                }
                if (is.vector(content[[i]][[j]])) {
                  content[[i]][[j]] <- content[[i]][[j]][1]
                  }
                else if (is.data.frame(content[[i]][[j]]) & length(content[[i]][[j]]) > 0) {
                  if (length(content[[i]][[j]]) == 0) {content[[i]][[j]] <- NA}
                  if (i != "regulation_id_number_info") {
                    for (k in seq_along(content[[i]][[j]])) { #all of [these] k in j are simply vectors; however, such condition may change in later versions of the API
                      q <- paste(i, names(content[[i]][[j]][k]), sep = ".") #otherwise would yield duplicated names and thus too many
                      if (is.null(content[[q]])) {content[[q]] <- NA}
                      content[[q]][j] <- content[[i]][[j]][[k]][[1]]
                    }
                  }
                  else {
                    q <- paste(i, names(content[[i]][[j]][k]), sep = ".")
                    if (is.null(content[[q]])) {content[[q]] <- NA}
                    content[[q]] <- content[[i]][[j]][[k]]
                  }
                }
              }
              if (is.data.frame(content[[i]])) {content[[i]] <- NULL}
              else {content[[i]] <- NA}
            }
            content[, sapply(content, is.list)] <- NULL
            content <- dplyr::as_tibble(content)
          }
        }
        return(content)
      }
      else {
        stop(paste("federalregister.gov says document(s) ", content[["errors"]], "could not be found."))
      }
    }
    else {
      stop(paste("Federal Register query failed.\n federalregister.gov says", status))
    }
  }
  else {
    stop(paste("get_fr() call failed.",
               ifelse(!is.character(doc_nums), "Your document numbers are not in a character vector.",
                      ifelse(output %in% c("tidy", "data.frame", "list"), "Your fields are not in a a character vector, or are not eligible. Please see the default values.", "Your output is not one of 'tidy', 'data.frame', or 'list'.")
                      )))
  }
}