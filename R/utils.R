#' Reconstruct paragraphs.
#'
#' Reconstruct paragraphs from a character vector with dissolved paragraphs, to
#' cope with a typical scenario when processing pdf documents.
#'
#' @param x character vector to be processed
#' @param threshold number of characters per line to distinguish whether to make
#'   / not to make a paragraph
#' @param start_paragraph_term character vector or expression which should start
#'   a new paragraph
#' @param end_paragraph_term character vector or expression which should end a
#'   paragraph
#' @param language for language specific heuristics
#' @param paragraph_punctuation character vector of punctuation marks which
#'   indicate the end of a paragraph if the next line starts with an upper case
#'   letter.
#' @break_punctuation character vector of punctuation marks which indicate the
#'   end of a paragraph if the next line is shorter than the threshold. Needs to
#'   be regexed already.
#' @paragraph_by_whitespace logical; if true, the paragraphs are derived from
#'   lines containing only white space. All other arguments are ignored.
#' @export paragraphize
paragraphize <- function(x,
                         threshold = 40,
                         start_paragraph_term = NULL,
                         end_paragraph_term = NULL,
                         language = "de",
                         paragraph_punctuation = ":",
                         break_punctuation = "[[:punct:]]",
                         paragraph_by_whitespace = FALSE){
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
      if (grepl("\\S+-$", x[i])){ # changed here. Before the hyphen, there shouldn't be a whitespace as this could be an insertion indicated by "-".
        if (!is.null(language) && language == "de") {
          y <- if (grepl("^[A-Z]|^und|^oder", x[i+1])) x[i] else gsub("-$", "", x[i]) 
          
          # if after that, there still is a hyphen and a "und" or "oder" starts
          # the following line, keep the hyphen and insert a whitespace after
          # that.
          
          if (grepl("^und|^oder", x[i+1]) && grepl("-$", x[i])) y <- gsub("-$", "- ", x[i])
          
        } else {
          y <- if (grepl("^[A-Z]", x[i+1])) x[i] else gsub("-$", "", x[i])
        }
      } else {
        y <- gsub("^\\s*(.*?)\\s*$", "\\1 ", x[i])
      }
      y
    }
  )
  
  concat_hyphenated <- function(txt) {
    for (i in length(txt):2) {
      if (!grepl("^[A-Z]|^und|^oder", txt[i])) {
        txt[i-1] <- gsub("-\\s*$", "", txt[i-1])
        txt[i-1] <- trimws(txt[i-1])
        txt[i] <- trimws(txt[i])
        txt[i-1] <- paste0(txt[i-1], txt[i])
        txt[i] <- ""
      }
    }
    empties <- grep("^\\s*$", txt)
    if(length(empties) > 0) txt <- txt[-empties]
    txt <- trimws(txt)
    return(txt)
  }
  
  # 2023: In some protocols, it is possible to get the paragraphs based on empty
  # lines alone.
  
  if (isTRUE(paragraph_by_whitespace)) {

    breakLines <- grep("^\\s*$", x2)

    # empty lines in the first and last index should be ignored

    if (any(breakLines == 1)) breakLines <- breakLines[-which(breakLines == 1)]
    if (any(breakLines == length(x2))) breakLines <- breakLines[-which(breakLines == length(x2))]

  } else {

    # To reconstruct paragraphs, the following heuristic is applied: We
    # assume that a new paragraph begins, if a line ends with a
    # punctuation mark, and if the length of the line undercuts a
    # certain threshold (hardcoded, 40).

    # breakLines <- which(nchar(x2) < threshold & grepl(".*[[:punct:]]\\s*$", x2))

    # 2023: This [[:punct:]] does include commas which usually do not introduce
    # paragraphs. This is made more explicit by the argument break_punctuation.

    breakLines <- which(nchar(x2) < threshold & grepl(sprintf(".*(%s)\\s*$", break_punctuation), x2))

    # In addition, we can assume a paragraph if the line ends with a colon
    # (independent from the length of the line) and the next line starts with a
    # capital letter (might be too language dependent?) or a dash or hyphen.

    # 2023: This is not always a good heuristic. In the PDFs of the German
    # Bundestag, colons do not necessarily indicate new paragraphs.

    if (is.character(paragraph_punctuation)) {

      paragraph_punctuation_collapsed <- paste0(paste0("\\", paragraph_punctuation), collapse = "|")
      paragraph_punctuation_collapsed_regex <- sprintf(".*(%s)\\s*$", paragraph_punctuation_collapsed)

      punctuationLines <- which(grepl(paragraph_punctuation_collapsed_regex, x2))
      punctBreakLines <- punctuationLines[which(grepl("^([A-Z]|\\-|\\–|\\—)+.*", x2[punctuationLines + 1]))]
      breakLines <- unique(unname(c(breakLines, punctBreakLines)))

    }

    # In addition, there are document specific lines in which it would be
    # advantageous for the following processing steps if a linebreak would occur
    # before the term.

    if (!is.null(start_paragraph_term)) {
      if (length(start_paragraph_term) > 1) start_paragraph_term <- paste(start_paragraph_term, collapse = "|")
      termLines <- which(grepl(paste0("^", start_paragraph_term, ".*"), x2))
      breakLines <- unique(unname(c(breakLines, termLines)))
    }

    # The same is true for terms which should indicate the end of a paragraph
    if (!is.null(end_paragraph_term)) {
      if (length(end_paragraph_term) > 1) end_paragraph_term <- paste(end_paragraph_term, collapse = "|")
      termLines <- which(grepl(paste0(".*", end_paragraph_term, "\\s*$"), x2))
      breakLines <- unique(unname(c(breakLines, termLines)))
    }
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
#' Check for matches of regular expressions in any combination of up to the
#' number of successive lines specified by \code{n_lines}. Matches that are
#' matched by any element of \code{mismatch} (using \code{grepl}) will be
#' ignored.
#'
#' @param x input character vector
#' @param regex a named character vector of regular expressions
#' @param n_lines integer, maximum number of lines to match
#' @param mismatch a character vector, regex matches included in this vector
#'   will be discarded (using \code{grepl})
#' @param match_longest character vector of regex which for which lines which
#'   were already matched should be matched again when they are pasted with
#'   additional lines. In consequence, this continuously checks whether adding a
#'   new line generates a new match and prefers this longer match over the
#'   previous match. Defaults to an empty character vector which excludes
#'   already matched lines from being matched later on. This is used selectively
#'   because for most stage regex, this might lead to a vast number of
#'   mismatches whereas in others, this might be beneficial. Terminology is
#'   based on the "longest" expression in the Corpus Workbench's CQP language.
#' @export paste_and_match
#' @examples
#' x <- c(
#'   "(The audience", "is listening)",
#'   "Welcome", "all", "of", "you", "!",
#'   "(The audience", "goes crazy)"
#'   )
#' y <- paste_and_match(x, regex = c(yeah = "^\\(.*?\\)$"), n_lines = 2L)
#' y <- paste_and_match(x, regex = c(yeah = "^\\(.*?\\)$"), n_lines = 2L, mismatch = "goes crazy")
paste_and_match <- function(x, regex, n_lines = 2L, mismatch = character(), match_longest = character()){
  
  if (length(match_longest > 0L)) stop("match_longest is currently very experimental and causes lines to disappear. Do not use.")
  if (!is.character(x)) stop("x needs to be a character vector")
  if (is.null(names(x))) names(x) <- rep("", times = length(x))
  if (!is.character(regex)) stop("regex needs to be a character vector")
  if (is.null(names(regex))) stop("regex character vector needs to be named")
  if (n_lines < 1L) stop("n_lines needs to be an integer value of 1 or higher")
  if (length(x) < n_lines) n_lines <- length(x)
  if (!is.character(mismatch)) stop("mismatch needs to be a character vector")
  
  n_orig <- length(x)
  
  for (i in 1L:n_lines){
    for (regexID in 1L:length(regex)){
      if (i == 1L){
        collapsed <- x
      } else {
        if (length(x) > 1L & length(x) > (i - 1)) {

          # Note: length(x) > (i - 1) seems to be necessary because when a
          # previously concatenated string is kept, this might result in the
          # length of x being smaller than n_lines - 1 in a given chunk. In
          # consequence, the second part of j:(length(x) - (i - j)) becomes
          # negative which results in an error. Note that this might mask
          # unexpectedly long concatenations.

          vecs <- sapply(1L:i, function(j) x[j:(length(x) - (i - j))])
          if (!is.matrix(vecs)) vecs <- matrix(data = vecs, nrow = 1)
          collapsed <- apply(vecs, 1, function(v) paste0(v, collapse = " "))
        } else {
          collapsed <- x
        }
      }

      matches <- grep(regex[regexID], collapsed, perl = TRUE)

      for (m in mismatch) matches <- matches[!grepl(m, collapsed[matches], perl = TRUE)]

      # the loop above matches the entire line (in contrast to  the functionality in
      # get_chunk_data() which only looks into the who slot)

      for (m in rev(matches)){

        if (length(match_longest) > 0L & names(regex)[regexID] %in% match_longest) {

          # Note: This does not fully work yet because of a mismatch between "x"
          # and "collapsed" which results in overwriting lines.

          x[m] <- collapsed[m]
          names(x)[m] <- names(regex)[regexID]
          if (i > 1L) x <- x[-((m + 1L):(m + i - 1L))]

        } else {

        if (all(names(x)[m:(m + i - 1L)] == "")) {
          x[m] <- collapsed[m]
          names(x)[m] <- names(regex)[regexID]
          if (i > 1L) x <- x[-((m + 1L):(m + i - 1L))]
        }
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