#' PlenaryProtocol class.
#' 
#' @field filename name of the file to process (basename only)
#' @field sourceDir the directory where the file to be processed resides
#' @field txt class \code{"character"}
#' @field chunks class \code{"list"} 
#' @field xml class \code{"character"}
#' @field speakerRegex speakerRegex (list)
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{initialize method for a new object of class \code{PlenaryProtocol};
#'   does nothing for class \code{PlenaryProtocol}, and needs to be defined for classes inheriting
#'   from \code{PlenaryProtocol}}
#'   \item{\code{$preprocessing(replacements = list(c("[\025]", "\u00A7"), c("\u00A0", " "), c("\a", "\n"))}}{
#'   the method will (a) remove empty lines, (b) remove leading and trailing whitespace, (c) carry out replacements, and 
#'   (d) carry out document-specific functions}
#'   \item{\code{$getMetadata()}}{extract metadata from document; the result
#'   will be stored in the field \code{metadata}}
#'   \item{\code{$getDebate()}}{remove leading and trailing noise before the
#'   "actual debate" (such as table of contents, or annexes)}
#'   \item{\code{$splitBySpeakers(maxLengthWho = 60)}}{turn the plain text
#'   document into a list of chunks by applying regular expressions for
#'   speakers}
#'   \item{\code{$concatenateMultilineInterjections()}}{}
#'   \item{\code{$concatenateParagraphs()}}{This function concatenates
#'   paragraphs that are splitted (i.e. the text does not end with an
#'   interpuncutation) by applying a regex. It includes an exlude parameter: The
#'   formalities in the protocols explained by the president may include much
#'   more than spoken text.}
#'   \item{\code{$makeSpeakerNodes}}{}
#'   \item{\code{$makeAgendaItemNodes}}{}
#'   \item{\code{$makeXML()}}{}
#'   \item{\code{$xmlify}}{}
#'   
#'}
#' @export PlenaryProtocol
#' @importFrom stringi stri_locate_all_regex
#' @aliases agendaItemTypes teiXpath
#' @importFrom XML newXMLNode saveXML xmlRoot xmlParse xmlTreeParse xmlValue
PlenaryProtocol <- R6Class(
  
  "PlenaryProtocol",
  
  public = list(
    
    # fields
    
	  filename = NULL, # character
	  sourceDir = NULL, # character
	  outfile = NULL, # character
		txt = NULL, # character
		chunks = NULL, # list
		xml = NULL, # character
		html = NULL, # character
		speakerRegex = NULL, # list
		speakerFactory = NULL, # list
		agendaItemRegex = NULL, # list
		agendaItemTypes = NULL, # character
		agendaItemNodes = NULL, # list
		preprocessingFunctions = NULL, # list
		speakerMismatchFile = NULL, # character
		speakerMismatch = NULL, # character
		agendaItemMismatchFile = NULL, # character
		agendaItemMismatch = NULL, # list
		speakerNodes = NULL, # list
		metadata = NULL, # list
		xpath = NULL, # list
    downloadReportFile = NULL, # character
		
		# methods 
		
		initialize = function(filename, sourceDir, verbose = TRUE){
		  self$filename <- filename
		  self$sourceDir <- sourceDir
		  x <- file.path(self$sourceDir, self$filename)
		  if (verbose) message("... reading file: ", x)
		  self$txt <- readLines(x)  
		  invisible(self)
		},
		
		
		preprocessing = function(replacements = list(c("[\025]", "\u00A7"), c("\u00A0", " "), c("\a", "\n"))){
		  
		  # remove empty lines
		  emptyLines <- grep("^\\s*$", self$txt)
		  if (length(emptyLines) > 0) self$txt <- self$txt[-emptyLines]
		  
		  # remove leading and trailing whitespace
		  self$txt <- gsub("^\\s*(.*)\\s*$", "\\1", self$txt)
		  
		  # carry out replacements
		  for (replacement in replacements){
		    self$txt <- gsub(replacement[1], replacement[2], self$txt)
		  }
		  
		  # execute document-specific preprocessing functions
		  if (self$filename %in% names(self$preprocessingFunctions)){
		    txt <- self$preprocessingFunctions[[self$filename]](self$txt)
		  }
		  
		  invisible(self$txt)
		},
		
		
		getMetadata = function(){
		  warning("needs to be implemented for specific protocol types")
		},
		
		
		getDebate = function(){
		  warning("needs to be implemented for specific protocol types")
		},
		
		splitBySpeakers = function(maxLengthWho = 60){
		  
		  # The first step is to get the indices of the vector where a call to a speaker occurs.
		  # We iterate through the list provided by the 'speakerRegex' field, and get the
		  # index positions. Two checks are performed: (a) If the match for the name of the
		  # speaker exceeds 'maxLengthWho', the match is discarded. (b) Matches that are 
		  # included in the field 'speakerMismatch' are discarded, too.
		  
		  startParas <- lapply(
		    self$speakerRegex,
		    function(speakerRegex) {
		      index <- grep(speakerRegex["regex"], self$txt, perl = TRUE)
		      who <- gsub(speakerRegex["regex"], speakerRegex["who"], self$txt[index], perl = TRUE)
		      # keep only those speakers ("who") with names shorter than the maximum length
		      toKeepLengthOk <- which(nchar(who) < maxLengthWho)
		      toKeepNoMismatch <- which(!who %in% self$speakerMismatch)
		      toKeep <- intersect(toKeepLengthOk, toKeepNoMismatch)
		      index[toKeep]
		    }
		  )
		  
		  # We may see that a string is matched by regexes for two different types of
		  # speakers. We resolve those double matches by removing matches from one
		  # list.
		  overlap <- which(startParas[["mp"]] %in% startParas[["gov"]] == TRUE)
		  if (length(overlap) > 0) startParas[["gov"]] <- startParas[["gov"]][-overlap]

		  # Intermediate step: We now turn the list of starting paragraphs into a
		  # oredered, named character vector.
		  startParas <- unlist(startParas)
		  startParas <- startParas[order(startParas)]
		  if (startParas[length(startParas)] != length(self$txt)){
		    startParas <- c(startParas, length(self$txt))
		  }
		  
		  # Finally, the raw character vector is chunked, i.e. it is turned into a list
		  # with sections of speech of one speaker that has been identified.
		  parasSpeechesFactor <- cut(1:length(self$txt), breaks = startParas, include.lowest = TRUE, right = FALSE)
		  self$chunks <- split(self$txt, parasSpeechesFactor)
		  names(self$chunks) <- names(startParas)[1:length(self$chunks)]
		  
		  invisible(self$chunks)
		},
		
		
		concatenateMultilineInterjections = function(){
		  chunks <- self$chunks
		  chunksNew <- lapply(
		    1:length(chunks),
		    function(k){
		      chunk <- chunks[[k]]
		      openingBrackets <- grep("^\\s*\\((?!.*\\)).*$", chunk, perl=TRUE)
		      closingBrackets <- grep("^(?!\\s*\\().*\\)\\s*$", chunk, perl=TRUE)
		      names(openingBrackets) <- rep("(", times=length(openingBrackets))
		      names(closingBrackets) <- rep(")", times=length(closingBrackets))
		      brackets <- c(openingBrackets, closingBrackets)
		      brackets <- brackets[order(brackets, decreasing=FALSE)]
		      bracketSoup <- paste(names(brackets), sep="", collapse="")
		      bracketPairs <- stringi::stri_locate_all_regex(bracketSoup, pattern="\\(\\)")[[1]]
		      if (nrow(bracketPairs) > 0 && any(is.na(as.character(bracketPairs))) == FALSE){
		        chunkList <- as.list(chunk)
		        for(i in c(nrow(bracketPairs):1)){
		          concatIntRaw <- paste(chunk[c(brackets[bracketPairs[i,1]]:brackets[bracketPairs[i,2]])], collapse=" ")
		          concatInt <- gsub("^\\s*(.*)\\s*$", "\\1", concatIntRaw)
		          concatInt <- gsub("\\s+", " ", concatInt)
		          chunkList[[brackets[bracketPairs[i,1]]]] <- concatInt
		          for (j in c(brackets[bracketPairs[i,2]]:(brackets[bracketPairs[i,1]] + 1))) chunkList[[j]] <- NULL
		        }
		        chunk <- unlist(chunkList)
		      }
		      return(chunk)
		    })
		  names(chunksNew) <- names(chunks)
		  self$chunks <- chunksNew
		},
		
		
		concatenateParagraphs = function(exclude = "pres"){
		  # The general scheme is that we iterate through the chunks. Thus there 
		  # is a helper function '.chunkProcessor' to handle the individual chunks.
		  .chunkProcessor <- function(chunk){
		    if (length(chunk) > 1){
		      # Each chunk is turned into a list of character vectors. The assumption here is
		      # that concatenating paragraphs is not performed for interjections.
		      interjection <- grep("^\\s*\\(.*?\\)\\s*$", chunk) # not yet generic
		      beforeInterjection <- interjection - 1
		      cutPoints <- unique(c(1, interjection, beforeInterjection, length(chunk)))
		      cutPoints <- cutPoints[order(cutPoints)]
		      pieceDef <- cut(
		        x = 1:length(chunk),
		        breaks = unique(c(cutPoints, length(chunk))), include.lowest = TRUE, right = TRUE
		        )
		      paraSplit <- split(chunk, pieceDef)
		      
		      # Now, we we iterate through the list of speech and interjections.
		      retval <- lapply(
		        paraSplit,
		        function(pieces){
		          
		          # To prepare concatenation, we check whether lines end with a hyphen. If TRUE,
		          # and the next line does not start with a big capital letter, the hyphen is 
		          # removed.
		          piecesPrep <- sapply(
		            1:length(pieces),
		            function(i) {
		              if (grepl("-$", pieces[i])){
		                if (grepl("^[A-Z]", pieces[i+1])){
		                  return( pieces[i] )
		                } else {
		                  return( gsub("-$", "", pieces[i]) )
		                }
		              } else {
		                return( paste(pieces[i], " ", sep = "") )
		              }
		            }
		          )
		          
		          # To reconstruct paragraphs, the following heuristic is applied: We
		          # assume that a new paragraph begins, if a line ends with a
		          # punctuation mark, and if the length of the line undercuts a
		          # certain threshold (hardcoded, 40).
		          endOfParaCandidates <- grep("[.?!]\\s*$", piecesPrep, perl = TRUE)
		          endOfPara <- endOfParaCandidates[which(nchar(endOfParaCandidates) < 40)]
		          for (i in endOfPara) piecesPrep[i] <- gsub("([.?!])\\s*$", "\\1BREAK", piecesPrep[i])
		          
		          # Paste everything, and break it up again where we assume paragraphs to 
		          # begin.
		          para <- paste(piecesPrep, collapse = "")
		          unlist(strsplit(para, "BREAK"))
		        }
		        )
		      return( unname(unlist(retval)) )
		    } else {
		      retval <- list("[1,1]" = chunk)
		      return( unname(unlist(retval)) )
		    }
		  }
		  newChunks <- lapply(self$chunks, .chunkProcessor)
		  names(newChunks) <- names(self$chunks)
		  self$chunks <- newChunks
		},
		
		makeSpeakerNodes = function(){
		  self$speakerNodes <- lapply(
		    1:length(self$chunks),
		    function(i){
		      textParas <- self$chunks[[i]]
		      type <- gsub("^(.*?)\\d+$", "\\1", names(self$chunks)[[i]])
		      speakerAttrs <- self$speakerFactory[[type]](textParas[1])
		      speakerCall <- gsub("^\\s*(.*?):.*$", "\\1:", textParas[1])
		      textParas[1] <- speakerAttrs[["text"]]
		      speakerAttrs[["text"]] <- NULL
		      speakerAttrs <- lapply(speakerAttrs, function(x) {
		        Encoding(x) <- "UTF-8"
		        x
		      })
		    speakerNode <- newXMLNode("sp", attrs = as.vector(speakerAttrs))
		    speakerCallNode <- newXMLNode("speaker", speakerCall)
		    if (length(grep("^\\s*$", textParas)) > 0) textParas <- textParas[-grep("^\\s*$", textParas)]  
		    speakerNodeParas <- lapply(
		      textParas,
		      function(para){
		        if (grepl("^\\s*\\((?!Erste\\sBeratung)(.*?)\\)\\s*$", para, perl=T)){
		          newXMLNode("stage", attrs = c(type="interjection"), para)
		        } else {
		          newXMLNode("p", para)
		          # newXMLTextNode(para)
		        }
		      }
		    )
		    addChildren(speakerNode, kids = c(speakerCallNode, speakerNodeParas))
		    saveXML(speakerNode, prefix = NULL)
		  })
		},
		
		makeAgendaItemNodes = function(maxLengthAgendaItemMatch = NULL, maxNcharAgendaItemDesc = 80){
		  chunks <- self$chunks
		  # topicCallLogical will indicate which chunks contain a topic call
		  matchTopicCall <- lapply(
		    1:length(chunks),
		    function(i){
		      chunk <- chunks[[i]]
		      # chunkType -> whether speaker is "gov", "mp", or "pres", ...
		      chunkType <- gsub("(.*?)\\d+", "\\1", names(chunks)[i])
		      # get the lines where one of agenda item regexes matches
		      matchAgendaItemCall <- unique(unlist(lapply(self$agendaItemRegex, function(regex) grep(regex, chunk))))
		      
		      # remove the matching lines that are known to be a mismatch
		      if (!is.null(self$agendaItemMismatch[[self$filename]])){
		        agendaItemMismatchPosition <- unique(unlist(lapply(
		          self$agendaItemMismatch[[self$filename]],
		          function(mismatch) grep(mismatch, chunk)
		        )))
		        if (length(agendaItemMismatchPosition) > 0) matchAgendaItemCall <- matchAgendaItemCall[-agendaItemMismatchPosition]
		      }
		      # annotate call of a debate only if it occurs in a speech of the presidency
		      if(length(matchAgendaItemCall) > 0 && chunkType == "pres"){
		        matchPosition <- max(matchAgendaItemCall) # apply only last call of an agenda item
		        if (matchPosition <= length(chunk)){
		          # which of the regexes did the hit?
		          matchRegexLogical <- sapply(self$agendaItemRegex, function(regex) grepl(regex, chunk[matchPosition]))
		          regexName <- names(matchRegexLogical)[which(matchRegexLogical == TRUE)]
		          if (any(regexName != "any")){
		            agendaItemType <- regexName[which(regexName != "any")][1]
		            agendaItemDesc <- unname(self$agendaItemTypes[agendaItemType])
		          } else if (matchPosition < length(chunk)){
		            agendaItemTitle <- chunk[matchPosition + 1]
		            agendaItemTypeMatch <- sapply(self$agendaItemTypes, function(regex) grepl(regex, agendaItemTitle))
		            agendaItemType <- paste(names(agendaItemTypeMatch[which(agendaItemTypeMatch == TRUE)]), collapse="|")   
		            agendaItemDesc <- ifelse(
		              nchar(agendaItemTitle) > maxNcharAgendaItemDesc,
		              paste(substr(agendaItemTitle, 1, maxNcharAgendaItemDesc), "...", sep=""),
		              agendaItemTitle
		            )            
		          } else {
		            agendaItemType <- NA
		            agendaItemDesc <- NA
		          }
		          
		        } else {
		          agendaItemType <- NA
		          agendaItemDesc <- NA
		        }
		      } else {
		        matchPosition <- 0
		        agendaItemType <- NA
		        agendaItemDesc <- NA 
		      }
		      list(matchPosition, agendaItemType, agendaItemDesc)
		    })
		  topicCallDf <- do.call(rbind, matchTopicCall)
		  colnames(topicCallDf) <- c("position", "type", "desc")
		  topicCallPosition <- which(topicCallDf[,"position"] > 0)
		  # topicCallPosition <- which(topicCallLogical == TRUE)
		  if (length(topicCallPosition) == 0){
		    agendaItemParas <- list("0" = 1:length(chunks))
		    topicCallDf <- data.frame(position = NA, type = NA, desc = NA)
		  } else {
		    topicCallDf <- data.frame(topicCallDf[topicCallPosition,])
		    # to include the last para in the last topic called
		    if (topicCallPosition[length(topicCallPosition)] != length(chunks)){
		      topicCallPosition <- c(topicCallPosition, length(chunks))
		    } 
		    howToCut <- cut(x = 1:length(chunks), breaks = topicCallPosition, include.lowest = TRUE)
		    howToCutNumeric <- as.numeric(howToCut)
		    howToCutNumeric[which(is.na(howToCutNumeric))] <- 0
		    if (0 %in% howToCutNumeric){
		      # not covered by the df
		      topicCallDf <- rbind(data.frame(position = NA, type = NA, desc = NA), topicCallDf)
		    }
		    agendaItemParas <- split(1:length(chunks), howToCutNumeric)
		  }
		  self$agendaItemNodes <- lapply(
		    1:length(agendaItemParas),
		    function(i){
		      topicNode <- newXMLNode(
		        "div",
		        attrs = c(
		          type = "agenda_item",
		          n = names(agendaItemParas)[i],
		          what = unname(topicCallDf[i,"type"]),
		          desc = unname(topicCallDf[i,"desc"])
		        )
		      )
		      addChildren(
		        topicNode,
		        lapply(
		          agendaItemParas[[i]],
		          function(j) xmlRoot(xmlParse(self$speakerNodes[[j]], asText=TRUE))
		        )
		      )
		      saveXML(topicNode)
		    })
		},
		
		metadataToTei = function(){
		  teiDoc <- xmlTreeParse(
		    system.file("xml/teiTemplate.xml", package = "ctk.plpr"),
		    useInternal = TRUE)
		  lapply(
		    names(self$xpath),
		    function(x){
		      teiNode <- getNodeSet(teiDoc, path=self$xpath[[x]])[[1]] 
		      xmlValue(teiNode) <- self$metadata[[x]]
		      return(NULL)
		    })
		  self$xml <- saveXML(teiDoc)
		},

		makeXML = function(){
		  
		  teiDoc <- xmlParse(self$xml, asText = TRUE)
		  bodyElement <- getNodeSet(teiDoc, path = "/TEI/text/body")[[1]]
		  addChildren(
		    bodyElement,
		    lapply(self$agendaItemNodes, function(x) xmlRoot(xmlParse(x, asText = TRUE)))
		  )
		  self$xml <- saveXML(teiDoc, prefix = '<?xml version="1.0" encoding="UTF-8"?>', indent = TRUE, encoding = "UTF-8")
		},
		
		xmlify = function(verbose = TRUE){
		  # self$txt <- readLines(file.path(self$sourceDir, self$filename))
		  self$preprocessing() # remove leading and and trailing spaces
		  self$getMetadata() # returns a list with sessionNo, lp, date
		  self$getDebate()
		  self$splitBySpeakers()
		  self$concatenateMultilineInterjections() # in package ctk.plpr
		  self$concatenateParagraphs(exclude = "pres")
		  self$makeSpeakerNodes()
		  self$makeAgendaItemNodes()
		  self$metadataToTei()
		  self$makeXML()
		},
		
		regexContextInChunks = function(regex, ncharContext = 100, verbose = FALSE){
		  
		  self$preprocessing() # remove leading and and trailing spaces
		  self$getMetadata() # returns a list with sessionNo, lp, date
		  self$getDebate()
		  self$splitBySpeakers()
		  self$concatenateMultilineInterjections() # in package ctk.plpr
		  self$concatenateParagraphs(exclude = "pres")
		  regexMatches <- lapply(
		    1:length(self$chunks),
		    function(i) {
		      chunk <- self$chunks[[i]]
		      role <- gsub("^(.*?)\\d+$", "\\1", names(self$chunks)[i])
		      matches <- ctk:::.regexContext(
		        chunk,
		        param = list(asText = TRUE, regex = regex, ncharContext = ncharContext)
		      )
		      if (nrow(matches) > 0){
		        if (i == length(Protocol$chunks)){
		          return(NULL)
		        } else {
		          return(data.frame(role = role, chunkNo = i, matches))  
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
		
  )
)