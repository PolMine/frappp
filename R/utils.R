#' Reconstruct paragraphs.
#' 
#' Reconstruct paragraphs from a character vector with dissolved paragraphs,
#' to cope with a typical scenario when processing pdf documents.
#' 
#' @param x character vector to be processed
#' @param threshold number of characters per line to distinguish whether to make
#'   / not to make a paragraph
#' @param new_paragraph_term character vector or expression which should start a new paragraph
#' @param language for language specific heuristics
#' @export paragraphize
paragraphize <- function(x, threshold = 40, new_paragraph_term = NULL, language = NULL){
  # To prepare concatenation, we check whether lines end with a hyphen. If TRUE,
  # and the next line does not start with a big capital letter, the hyphen is 
  # removed.
  
  # For German this can be problematic as composite expressions
  # as "verhandel- und planbar" are thinkable. Hence, we introduce 
  # a heuristic for that.
  if (length(x) == 1) return(x)
  x2 <- sapply(
    1:length(x),
    function(i) {
      if (grepl("-$", x[i])){
        if (language == "de") {
          y <- if (grepl("^[A-Z]|^und|^oder", x[i+1])) x[i] else gsub("-$", "", x[i])
        } else {
        y <- if (grepl("^[A-Z]", x[i+1])) x[i] else gsub("-$", "", x[i])
        }
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
  
  breakLines <- which(nchar(x2) < threshold & grepl(".*[[:punct:]]\\s*$", x2))
  
  # In addition, we can assume a paragraph if the line ends with a colon 
  # (indenpendent from the length of the line) and the next line
  # starts with a capital letter (might be too language dependent?) or a
  # dash or hyphen.
  
  colonLines <- which(grepl(".*\\:\\s*$", x2))
  colBreakLines <- colonLines[which(grepl("^([A-Z]|\\-|\\–|\\—)+.*", x2[colonLines+1]))]
  breakLines <- unique(unname(c(breakLines, colBreakLines)))
  
  # In addition, there are document specific lines in which
  # it would be advantageous for the following processing steps
  # if a linebreak would occur.
  
  # Wir kommen nun zu 
  # Zweite Aktuelle Debatte [Insert Linebreak here]
  
  if (!is.null(new_paragraph_term)) {
    if (length(new_paragraph_term) > 1) new_paragraph_term <- paste(new_paragraph_term, collapse = "|")
    termLines <- which(grepl(paste0("^", new_paragraph_term, ".*"), x2))
    breakLines <- unique(unname(c(breakLines, termLines)))
  }

  x2[breakLines] <- sprintf("%s<linebreak/>", x2[breakLines])
  
  
  
  # Paste everything, and break it up again
  concatenated <- paste(x2, collapse = "")
  paragraphs <- unlist(strsplit(concatenated, "<linebreak/>"))
  emptyLines <- grep("^\\s*$", paragraphs)
  if (length(emptyLines) > 0) paragraphs <- paragraphs[-emptyLines]
  gsub("^\\s*(.*?)\\s*$", "\\1", paragraphs, perl = TRUE)
}

#' Paste Lines and Check for Matches.
#' 
#' Check for matches of regular expressions in any combination of up to the number
#' of successive lines specified by \code{n_lines}. Matches that are matched by
#' any element of \code{mismatch} (using \code{grepl}) will be ignored.
#' 
#' @param x input character vector
#' @param regex a named character vector of regulary expressions
#' @param n_lines integer, maximum number of lines to match
#' @param mismatch a character vector, regex matches included in this vector
#'   will be discarded (using \code{grepl})
#' @export paste_and_match
#' @examples
#' x <- c(
#'   "(The audience", "is listening)",
#'   "Welcome", "all", "of", "you", "!",
#'   "(The audience", "goes crazy)"
#'   )
#' y <- paste_and_match(x, regex = c(yeah = "^\\(.*?\\)$"), n_lines = 2L)
#' y <- paste_and_match(x, regex = c(yeah = "^\\(.*?\\)$"), n_lines = 2L, mismatch = "goes crazy")
paste_and_match <- function(x, regex, n_lines = 2L, mismatch = character()){
  
  if (!is.character(x)) stop("x needs to be a character vector")
  if (is.null(names(x))) names(x) <- rep("", times = length(x))
  if (!is.character(regex)) stop("regex needs to be a character vector")
  if (is.null(names(regex))) stop("regex character vector needs to be named")
  if (n_lines < 1L) stop("n_lines needs to be an integer value of 1 or higher")
  if (length(x) < n_lines) n_lines <- length(x)
  if (!is.character(mismatch)) stop("mismatch needs to be a character vector")
  
  for (i in 1L:n_lines){
    for (regexID in 1L:length(regex)){
      if (i == 1L){
        collapsed <- x
      } else {
        if (length(x) > 1L){
          vecs <- sapply(1L:i, function(j) x[j:(length(x) - (i - j))])
          if (!is.matrix(vecs)) vecs <- matrix(data = vecs, nrow = 1)
          collapsed <- apply(vecs, 1, function(v) paste0(v, collapse = " "))
        } else {
          collapsed <- x
        }
      }
      matches <- grep(regex[regexID], collapsed, perl = TRUE)
      for (m in mismatch) matches <- matches[!grepl(m, collapsed[matches], perl = TRUE)]
      for (m in rev(matches)){
        if (all(names(x)[m:(m + i - 1L)] == "")){
          x[m] <- collapsed[m]
          names(x)[m] <- names(regex)[regexID]
          if (i > 1L) x <- x[-((m + 1L):(m + i - 1L))] 
        }
      }
    }
  }
  return(x)
}

#' Show Histogram of Number of Characters per Line.
#' 
#' Utility function to estimate the best cutoff threshold for
#' concatening lines.
#' 
#' @param x a character vector
#' @export hist_nchar
hist_nchar <- function(x){
  hist(
    x = x, las = 2, breaks = 1:max(nchar(x)),
    main = "Number of Characters",
    xlab = "nchar"
  )
}

