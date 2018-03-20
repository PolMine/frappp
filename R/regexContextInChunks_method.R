#' regex matches in chunks
#' 
#' Find the matches of one regex, or a set of regex in the 
#' chunks (of speeches) that are created when parsing parliamentary
#' protocols. The purpose of the function is to improve regex. 
#' 
#' @param .Object the object
#' @param regex regex
#' @param ncharContext nchar
#' @param what defaults to "plprbttxtProtocol"
#' @param ... further parameters that will be passed into dirApply
#' @name regexContextInChunks
#' @rdname regexContextInChunks
#' @export regexContextInChunks
#' @examples
#' \dontrun{
#' regexMatches <- regexContextInChunks(
#'   .Object="/home/blaette/Lab/repos/data.plprbttxt",
#'   regex=ctk.plpr:::agendaItemRegexPlprbttxt,
#'   ncharContext=80,
#'   what="plprbttxtProtocol",
#'   filenames=NULL,
#'   mc=10, verbose=FALSE, progress=TRUE, pattern="txt"
#'   )
#' write.table(regexMatches, file="/home/blaette/Lab/repos/data.plprbttxt/agendaItemTable.csv", quote=FALSE, sep="|||")
#' }
setGeneric(
  "regexContextInChunks",
  function(.Object,... ) standardGeneric("regexContextInChunks")
  )

# filename <- "BT_15_100.txt"
# sourceDir <- "/home/blaette/Lab/repos/data.plprbttxt"
# param <- list(regex=ctk.plpr:::agendaItemRegexPlprbttxt, ncharContext=50, what="plprbttxtProtocol")
# .regexContextInChunks(filename=filename, sourceDir=sourceDir, param=param)
.regexContextInChunks <- function(filename, sourceDir, targetDir = NULL, verbose = FALSE, param){
  ### copied from xmlify ###
  regex <- param[["regex"]]
  ncharContext <- param[["ncharContext"]]
  if (verbose == TRUE) message("... processing ", filename)
  Protocol <- do.call(param[["what"]], args = c(list(filename = filename)))
  if (is.null(sourceDir)){
    Protocol$txt <- filename
    Protocol$filename <- "NA"
  } else {
    Protocol$txt <- readLines(file.path(sourceDir, filename))  
  }
  Protocol$preprocessing() # remove leading and and trailing spaces
  Protocol$getMetadata() # returns a list with sessionNo, lp, date
  Protocol$getDebate()
  Protocol$splitBySpeakers()
  Protocol$concatenateMultilineInterjections() # in package ctk.plpr
  Protocol$concatenateParagraphs(exclude = "pres")
  ### end copy from xmlify ###
  regexMatches <- lapply(
    1:length(Protocol$chunks),
    function(i) {
      chunk <- Protocol$chunks[[i]]
      role <- gsub("^(.*?)\\d+$", "\\1", names(Protocol$chunks)[i])
      matches <- ctk:::.regexContext(
        chunk,
        param = list(asText = TRUE, regex = regex, ncharContext = ncharContext)
      )
      if (nrow(matches) > 0){
        if (i == length(Protocol$chunks)){
          return(NULL)
        } else {
          return(data.frame(role=role, chunkNo = i, matches))  
        }
      } else {
        return(NULL)
      }
    })
  regexMatchDf <- do.call(rbind, regexMatches)
  if (!is.null(regexMatchDf)){
    regexMatchDf <- subset(regexMatchDf, !is.na(regexMatches))
    regexMatchDf[,"filename"] <- filename    
  }
  regexMatchDf  
}

#' @rdname regexContextInChunks
setMethod("regexContextInChunks", "character", function(.Object, regex, ncharContext, what = "plprbttxtProtocol", ...){ 
  retval <- dirApply(
    f = .regexContextInChunks,
    sourceDir=.Object, param=list(what=what, regex=regex, ncharContext=ncharContext),
    ...
    )
  retval <- do.call(rbind, retval)
  # colnames(retval) <- c("role", "chunkNo", "regex", "left_context", "regex_match", "right_context", "filename")
  rownames(retval) <- NULL
  retval <- retval[,c("filename", "chunkNo", "pos", "role", "regex", "left_context", "regex_match", "right_context")]
  retval
})