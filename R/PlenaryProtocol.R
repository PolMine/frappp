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
#'   \item{\code{$getMetadata()}}{extract metadata from document; the result will be stored in the field \code{metadata}}
#'   \item{\code{$getDebate()}}{remove leading and trailing noise before the "actual debate" (such as table of contents, or annexes)}
#'   \item{\code{$splitBySpeakers(maxLengthWho = 60)}}{turn the plain text document into a list of chunks by applying regular expressions for speakers}
#'   \item{\code{$concatenateMultilineInterjections()}}{}
#'   \item{\code{$concatenateParagraphs()}}{}
#'   \item{\code{$makeSpeakerNodes}}{}
#'   \item{\code{$makeAgendaItemNodes}}{}
#'   \item{\code{$makeXML()}}
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
		  txt <- self$txt  
		  startOfDebate <- grep("Beginn:\\s+\\d+[\\.|:]\\d+\\s+Uhr", txt, perl = TRUE)
		  if (length(startOfDebate) == 0){
		    # in some protocols, the start time of the session is not provided
		    # in this case, use the first line with "Beginn:", but only if the city is indicated 
		    # in the line before
		    cityOfSession <- grep("^(Bonn|Berlin),", txt, perl=T)[1]
		    startOfDebate <- grep("^\\s*Beginn:\\s*$", txt, perl=T)
		    if (startOfDebate[1] == cityOfSession + 1){
		      startOfDebate <- startOfDebate[1]
		    } else {
		      startOfDebate <- c()
		    }
		  }
		  if (length(startOfDebate) == 0){
		    startOfDebate <- grep("(Sitzung\\sist\\ser\u00F6ffnet|er\u00F6ffne\\s+die\\s+Sit\\s*zung)", txt, perl=T) - 1
		  }
		  if (length(startOfDebate) == 0){
		    startOfDebate <- grep("rufe\\s(den|)\\s*Tagesordnungspunkt\\s1\\sauf", txt, perl=T) - 2
		  }
		  endOfDebate <- grep("^\\s*\\(\\s*Schlu(ss|.)( der Sitzung|).*?Uhr", txt, perl=T)[1]
		  if (length(endOfDebate) == 0 || is.na(endOfDebate)) {
		    endOfDebate <- grep("Die\\sSitzung\\sist\\sgeschlossen", txt, perl=T) + 1
		  }
		  if (length(endOfDebate) == 0 || is.na(endOfDebate)) {
		    endOfDebate <- length(txt)
		    message("end of debate not found in ", self$filename)
		  }
		  self$txt <- txt[c((startOfDebate[1] + 1):(endOfDebate[1]-1))]
		},
		
		splitBySpeakers = function(maxLengthWho = 60){
		  txt <- self$txt
		  speakerRegexList <- self$speakerRegex
		  speakerMismatch <- self$speakerMismatch
		  startParas <- lapply(speakerRegexList, function(speakerRegex) {
		    index <- grep(speakerRegex["regex"], txt, perl=T)
		    who <- gsub(speakerRegex["regex"], speakerRegex["who"], txt[index], perl=T)
		    # keep only those speakers ("who") with names shorter than the maximum length
		    toKeepLengthOk <- which(nchar(who) < maxLengthWho)
		    toKeepNoMismatch <- which(!who %in% speakerMismatch)
		    toKeep <- intersect(toKeepLengthOk, toKeepNoMismatch)
		    index[toKeep]
		  })
		  
		  # remove start paras for gov if present in mps
		  overlapMpGov <- which(startParas$mp %in% startParas$gov == TRUE)
		  if (length(overlapMpGov) > 0){
		    startParas$gov <- startParas$gov[-which(startParas$gov %in% startParas$mp == TRUE)]
		  }
		  startParas <- unlist(startParas)
		  startParas <- startParas[order(startParas)]
		  # for debugging purpuses: txt[as.numeric(names(which(table(startParas) > 1)))]
		  if (startParas[length(startParas)] != length(txt)){
		    startParas <- c(startParas, length(txt))
		  }
		  parasSpeechesFactor <- cut(c(1:length(txt)), breaks=startParas, include.lowest=T, right=FALSE)
		  docSplitted <- split(txt, parasSpeechesFactor)
		  names(docSplitted) <- names(startParas)[c(1:length(docSplitted))]
		  self$chunks <- docSplitted
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
		
		concatenateParagraphs = function(){
		  
		  "This function concatenates paragraphs that are splitted (i.e. the text does
      not end with an interpuncutation) by applying a regex. It includes an exlude
      parameter: The formalities in the protocols explained by the president may
      include much more than spoken text."
		  
		  message("not implemented here")
		},
		
		makeSpeakerNodes = function(){
		  chunks <- self$chunks
		  self$speakerNodes <- lapply(1:length(chunks), function(i){
		    textParas <- chunks[[i]]
		    type <- gsub("^(.*?)\\d+$", "\\1", names(chunks)[[i]])
		    speakerAttrs <- self$speakerFactory[[type]](textParas[1])
		    speakerCall <- gsub("^\\s*(.*?):.*$", "\\1:", textParas[1])
		    textParas[1] <- speakerAttrs[["text"]]
		    speakerAttrs[["text"]] <- NULL
		    speakerAttrs <- lapply(speakerAttrs, function(x) {
		      Encoding(x) <- "UTF-8"
		      x
		    })
		    speakerNode <- newXMLNode("sp", attrs=as.vector(speakerAttrs))
		    speakerCallNode <- newXMLNode("speaker", speakerCall)
		    if (length(grep("^\\s*$", textParas)) > 0) textParas <- textParas[-grep("^\\s*$", textParas)]  
		    speakerNodeParas <- lapply(
		      textParas,
		      function(para){
		        if (grepl("^\\s*\\((?!Erste\\sBeratung)(.*?)\\)\\s*$", para, perl=T)){
		          newXMLNode("stage", attrs=c(type="interjection"), para)
		        } else {
		          newXMLNode("p", para)
		          # newXMLTextNode(para)
		        }
		      }
		    )
		    addChildren(speakerNode, kids=c(speakerCallNode, speakerNodeParas))
		    saveXML(speakerNode, prefix=NULL)
		  })
		},
		
		makeAgendaItemNodes = function(maxLengthAgendaItemMatch = NULL, maxNcharAgendaItemDesc = 80){
		  chunks <- self$chunks
		  # topicCallLogical will indicate which chunks contain a topic call
		  matchTopicCall <- lapply(
		    c(1:length(chunks)),
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