#' PlenaryProtocol class.
#' 
#' @field filename name of the file to process (basename only)
#' @field sourceDir the directory where the file to be processed resides
#' @field txt class \code{"character"}
#' @field chunks class \code{"list"} 
#' @field xml class \code{"xml_document"}
#' @field speakerRegex speakerRegex (list)
#' @param template filename of the a TEI template document (valid XML)
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
#'   \item{\code{$metadataToTei(template = system.file("xml", "teiTemplate.xml",
#'   package = "frappp"))}}{Load and parse a template for a TEI document;
#'   defaults to file 'teiTemplate.xml' included in the 'xml' subdirectory of
#'   the 'frappp' package). The metadata defined in the field 'metadata' are
#'   written into the XML document, using the the XPATH expressions in the field
#'   'xpath'. The names of the list of metadata and the names of the character
#'   vector with XPATH expressions serve as keys to match metadata information
#'   and the XPATH expression that points to the correct location in the XML
#'   document.}
#'   \item{\code{$makeSpeakerNodes}}{Iterate through the chunks (i.e. list of 
#'   character vectors in the field \code{chunks}) and add nodes for speakers
#'   in the TEI document (in the field \code{xml})).}
#'   \item{\code{$makeAgendaItemNodes}}{}
#'   \item{\code{$makeXML()}}{}
#'   \item{\code{$xmlify}}{}
#'   
#'}
#' @export PlenaryProtocol
#' @importFrom stringi stri_locate_all_regex
#' @aliases agendaItemTypes teiXpath
#' @importFrom xml2 read_xml xml_find_first xml_text<- xml_text xml_attrs xml_attrs<- xml_add_child
#' @importFrom gsubfn strapply
#' @importFrom R6 R6Class
PlenaryProtocol <- R6Class(
  
  "PlenaryProtocol",
  
  public = list(
    
    # fields
	  id = NULL,
    
    txt = NULL, # character
		chunks = NULL, # list
		xml = NULL, # an xml document (from xml2 package)
		html = NULL, # character
		
		metadata = NULL, # list
		xpath = NULL, # list
		downloadReportFile = NULL, # character

		preprocessingFunctions = NULL, # list
		
		interjectionRegex = NULL,

		speakerRegex = NULL, # list
		speakerMismatchFile = NULL, # character
		speakerMismatch = NULL, # character
		
		agendaItemRegex = NULL, # list
		agendaItemTypes = NULL, # character
		agendaItemNodes = NULL, # list
		agendaItemMismatchFile = NULL, # character
		agendaItemMismatch = NULL, # list
		
		# methods 
		
		initialize = function(id, interjectionRegex = "^\\s*\\(.*?\\)\\s*$", xpath = teiXpath){
		  self$id <- id
		  self$interjectionRegex <- interjectionRegex
		  self$xpath = xpath
		  invisible(self)
		},
		
		
		readFile = function(filename){
		  self$txt <- readLines(filename)  
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
		  if (self$id %in% names(self$preprocessingFunctions)){
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
		      matchlist <- strapply(self$txt, pattern = speakerRegex[["regex"]], FUN = speakerRegex[["fn"]], perl = TRUE)
		      index <- which(sapply(matchlist, is.null) == FALSE) # where did the regex match?
		      indexChecked <- sapply(index, function(i) if (nchar(matchlist[[i]]["who"]) < maxLengthWho) i else NA)
		      if (any(is.na(indexChecked))) indexChecked <- indexChecked[!is.na(indexChecked)]
		      indexChecked
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
		
		
		paragraphize = function(x){
		  # To prepare concatenation, we check whether lines end with a hyphen. If TRUE,
		  # and the next line does not start with a big capital letter, the hyphen is 
		  # removed.
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
		  shortLines <- which(nchar(x2) < 40)
		  x2[shortLines] <- sprintf("%s<linebreak/>", x2[shortLines])
		  
		  # Paste everything, and break it up again
		  concatenated <- paste(x2, collapse = "")
		  paragraphs <- unlist(strsplit(concatenated, "<linebreak/>"))
		  emptyLines <- grep("^\\s*$", paragraphs)
		  if (length(emptyLines) > 0) paragraphs <- paragraphs[-emptyLines]
		  gsub("^\\s*(.*?)\\s*$", "\\1", paragraphs, perl = TRUE)
		},
		
		
		reconstructParagraphs = function(exclude = "pres"){
		  # The general scheme is that we iterate through the chunks. Thus there 
		  # is a helper function '.chunkProcessor' to handle the individual chunks.
		  .processor <- function(chunk){
		    if (length(chunk) > 1){
		      # Each chunk is turned into a list of character vectors. The assumption here is
		      # that concatenating paragraphs is not performed for interjections.
		      interjection <- grep(self$interjectionRegex, chunk)
		      beforeInterjection <- interjection - 1
		      cutPoints <- unique(c(1, beforeInterjection, interjection, length(chunk)))
		      cutPoints <- cutPoints[order(cutPoints)]
		      sections <- cut(
		        x = 1:length(chunk), breaks = cutPoints,
		        include.lowest = TRUE, right = TRUE
		        )
		      paraSplit <- split(chunk, sections)
		      
		      # iterate through the list of speech and interjections.
		      retval <- lapply(paraSplit, self$paragraphize)
		      return( unname(unlist(retval)) )
		    } else {
		      return( chunk )
		    }
		  }
		  newChunks <- lapply(self$chunks, .processor)
		  names(newChunks) <- names(self$chunks)
		  self$chunks <- newChunks
		},
		
		
		getAgendaItemCalls = function(maxNcharAgendaItemDesc = 40){
		  
		  chunkTypes <- gsub("(.*?)\\d+", "\\1", names(self$chunks))
		  chunksToConsider <- which(chunkTypes == "pres")

		  matchesForTopicCall <- lapply(
		    
		    chunksToConsider,
		    
		    function(i){
		      chunk <- self$chunks[[i]]
		      
		      # get the lines where one of agenda item regexes matches
		      matchAgendaItemCall <- unique(unlist(lapply(self$agendaItemRegex, function(regex) grep(regex, chunk))))
		      if (length(matchAgendaItemCall) == 0) return( NULL )
		      
		      # remove the matching lines that are known to be mismatches
		      mismatches <- self$agendaItemMismatch[[self$filename]]
		      if (!is.null(mismatches)){
		        agendaItemMismatchPosition <- unique(unlist(
		          lapply(mismatches, function(mismatch) grep(mismatch, chunk)
		          )))
		        if (length(agendaItemMismatchPosition) > 0) matchAgendaItemCall <- matchAgendaItemCall[-agendaItemMismatchPosition]
		      }
		      if (length(matchAgendaItemCall) == 0) return( NULL ) # stop things if nothing left
		      
		      matchPosition <- max(matchAgendaItemCall) # apply only last call of an agenda item
		      if (matchPosition > length(chunk)) return( NULL ) # no idea how it can happen, but it happened 
		      
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
		        agendaItemDesc <- if (nchar(agendaItemTitle) > maxNcharAgendaItemDesc){
		          paste(substr(agendaItemTitle, 1, maxNcharAgendaItemDesc), "...", sep="")
		        } else {
		          agendaItemTitle
		        }
		      }
		      data.frame(
		        chunk = i, position = matchPosition, type = agendaItemType, desc = agendaItemDesc,
		        stringsAsFactors = FALSE
		        )
		    }
		  )
		  topicCallDf <- do.call(rbind, matchesForTopicCall)
		  invisible(topicCallDf)
		},
		
		
		makeSpeakerNode = function(chunkNo, parent){
		  # remove empty lines, if necessary
		  textParas <- self$chunks[[chunkNo]]
		  if (length(grep("^\\s*$", textParas)) > 0) textParas <- textParas[-grep("^\\s*$", textParas)]  
		  
		  type <- gsub("^(.*?)\\d+$", "\\1", names(self$chunks)[[chunkNo]])
		  speakerAttrs <- strapply(
		    X = textParas[1],
		    pattern = self$speakerRegex[[type]][["regex"]],
		    FUN = self$speakerRegex[[type]][["fn"]]
		  )[[1]]
		  speakerCall <- gsub("^\\s*(.*?):.*$", "\\1:", textParas[1])
		  textParas[1] <- speakerAttrs[["text"]]
		  speakerAttrs <- speakerAttrs[which(names(speakerAttrs) != "text")]
		  Encoding(speakerAttrs) <- "UTF-8" # just to be sure
		  
		  newSpeakerNode <- xml_add_child(.x = parent, .value = "sp")
		  xml_attrs(newSpeakerNode) <- speakerAttrs
		  xml_add_child(newSpeakerNode, .value = "speaker", speakerCall)
		  
		  lapply(
		    textParas,
		    function(para){
		      if (grepl("^\\s*\\(.*?\\)\\s*$", para, perl = TRUE)){
		        # add element for interjection, if respective regex matches
		        xml_add_child(newSpeakerNode, .value = "stage", type = "interjection", para)
		      } else {
		        xml_add_child(newSpeakerNode, .value = "p", para)
		      }
		    }
		  )
		  invisible(type)
		},

		
		makeSpeakerNodes = function(chunkNos, parent){
		  lapply(chunkNos, function(i) self$makeSpeakerNode(chunkNo = i, parent = parent))
		  invisible(self)
		},
		
		
		makeHeader = function(template = system.file("xml", "teiTemplate.xml", package = "frappp")){
		  self$xml <- xml2::read_xml(template)
		  lapply(
		    names(self$xpath),
		    function(x){
		      teiNode <- xml2::xml_find_first(x = self$xml, xpath = self$xpath[[x]])
		      xml2::xml_text(teiNode) <- as.character(self$metadata[[x]])
		    })
		  invisible(self$xml)
		},
		
		
		makeBody = function(agendaItems = FALSE){
		  bodyNode <- xml2::xml_find_first(x = self$xml, xpath = "/TEI/text/body")
		  if (agendaItems == FALSE){
		    self$makeSpeakerNodes(chunkNos = 1:length(self$chunks), parent = bodyNode)
		  } else {
		    topicCallDf <- self$getAgendaItemCalls()
		    if (is.null(topicCallDf)){
		      agendaItemChunks <- list(1:length(self$chunks))
		      topicCallDf <- data.frame(chunk = 1, position = NA, type = NA, desc = NA)
		    } else {
		      topicCallChunks <- topicCallDf[["chunk"]]
		      # to include the last para in the last topic called
		      if (max(topicCallChunks) != length(self$chunks)){
		        topicCallChunks <- c(topicCallChunks, length(self$chunks))
		      }
		      howToCut <- cut(x = 1:length(self$chunks), breaks = topicCallChunks, include.lowest = TRUE)
		      howToCutInt <- as.integer(howToCut)
		      howToCutInt[which(is.na(howToCutInt))] <- 0
		      if (0 %in% howToCutInt){
		        topicCallDf <- rbind(data.frame(chunk = 1, position = NA, type = NA, desc = NA), topicCallDf)
		      }
		      agendaItemChunks <- split(1:length(self$chunks), howToCutInt)
		    }
		    lapply(
		      1:length(agendaItemChunks),
		      function(i){
		        newAgendaItemNode <- xml_add_child(
		          .x = bodyNode, .value = "div", type = "agenda_item", 
		          n = names(agendaItemChunks)[i], what = unname(topicCallDf[i,"type"]), desc = unname(topicCallDf[i,"desc"])
		        )
		        self$makeSpeakerNodes(chunkNos = agendaItemChunks[[i]], parent = newAgendaItemNode)
		      }
		    )
		  }
		  invisible(self)
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