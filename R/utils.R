#' Reconstruct paragraphs.
#' 
#' Reconstruct paragraphs from a character vector with dissolved paragraphs,
#' to cope with a typical scenario when processing pdf documents.
#' 
#' @param x character vector to be processed
#' @param threshold number of characters per line to distinguish whether to make
#'   / not to make a paragraph
#' @export paragraphize
paragraphize <- function(x, threshold = 40){
  # To prepare concatenation, we check whether lines end with a hyphen. If TRUE,
  # and the next line does not start with a big capital letter, the hyphen is 
  # removed.
  if (length(x) == 1) return(x)
  x2 <- sapply(
    1:length(x),
    function(i) {
      if (grepl("-$", x[i])){
        y <- if (grepl("^[A-Z]", x[i+1])) x[i] else gsub("-$", "", x[i])
      } else {
        y <- gsub("^\\s*(.*?)\\s*$", "\\1 ", x[i])
      }
      y
    }
  )
  
  # To reconstruct paragraphs, the following heuristic is applied: We
  # assume that a new paragraph begins, if a line ends with a
  # punctuation mark, and if the length of the line undercuts a
  # certain threshold (hardcoded, 40).
  shortLines <- which(nchar(x2) < threshold)
  x2[shortLines] <- sprintf("%s<linebreak/>", x2[shortLines])
  
  # Paste everything, and break it up again
  concatenated <- paste(x2, collapse = "")
  paragraphs <- unlist(strsplit(concatenated, "<linebreak/>"))
  emptyLines <- grep("^\\s*$", paragraphs)
  if (length(emptyLines) > 0) paragraphs <- paragraphs[-emptyLines]
  gsub("^\\s*(.*?)\\s*$", "\\1", paragraphs, perl = TRUE)
}
