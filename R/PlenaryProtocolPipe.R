#' Pipe class to process plenary protocols.
#' 
#' @field protocolclass the class to initialize
#' @section Arguments:
#' \describe{
#'   \item{sourceDir}{directory with the XML files to be processed}
#'   \item{targetDir}{if NULL (the default), files in sourceDir will be overwritten}
#'   \item{element}{defaults to "sp"}
#'   \item{attrs}{element attributes that will be used to generate a key}
#'   \item{attributesToAdd}{nested list with lps and roles, rownames used as keys}
#'   \item{pattern}{defaults to "xml"}
#'   \item{mc}{logical}
#'   \item{progress}{logical}
#'   \item{verbose}{logical}
#'   \item{lp}{the legislative period for which to perform the download}
#' }
#' @rdname PlenaryProtocolPipe-class
#' @export PlenaryProtocolPipe
#' @import ctk
#' @importFrom R6 R6Class
PlenaryProtocolPipe <- R6::R6Class(
  
  "PlenaryProtocolPipe",
  inherit = ctk::Pipe,
  
  public = list(
    
    govDataFile = "character",
    mpDataFile = "character",
    presDataFile = "character",
    federalCouncilDataFile = "character",
    supplDataFile = "character",
    aliasFile = "character",
    downloadReportFile = "character",
    protocolclass = "character",
    xslFile = "character",

    initialize = function(dir, downloadReportFile, protocolclass = "PlenaryProtocol"){
      self$dir <- dir
      self$downloadReportFile <- downloadReportFile
      self$protocolclass <- protocolclass
    },
    
    getDocs = function(targetDir, lp, first=1, makeUrl = quote(sprintf()), fmtFilename="BT_%02d_%03d.pdf"){
      .getFilesOnDisk <- function(reportFile){
        if (file.exists(reportFile)){
          downloadTable <- read.table(
            file = reportFile, stringsAsFactors = FALSE,
            quote = "", sep = "\t"
          )
          filesOnDisk <- downloadTable[,1]
        } else {
          filesOnDisk <- c()
        }
        return(filesOnDisk)
      }
      
      reportFile <- file.path(targetDir, "downloadReport.csv")
      .downloadWorker <- function(lp){
        newDocs <- c()
        filesOnDisk <- .getFilesOnDisk(reportFile)
        proceed <- TRUE
        docNo <- first - 1
        fail <- 0
        while (proceed == TRUE){
          docNo <- docNo + 1
          targetFile <- sprintf(fmtFilename, lp, docNo)
          if(!targetFile %in% filesOnDisk){
            url <- eval(makeUrl)
            if (url.exists(url)){
              message("... trying to download url: ", url)
              download.file(url, destfile=file.path(targetDir, targetFile))
              write.table(
                x=data.frame(file=targetFile, url=url, time=as.character(Sys.time())),
                file=reportFile, row.names=FALSE, col.names=FALSE,
                append=TRUE, sep="\t", quote=FALSE
              )
              newDocs <- c(newDocs, targetFile)
            } else {
              if (fail > 3) proceed <- FALSE
              fail <- fail + 1
            }
          }
        }
        return(newDocs)
      }
      newDocsList <- lapply(lp, .downloadWorker)
      return(unlist(newDocsList))
    },
    
    
    getMetadata = function(sourceDir, targetDir = NULL, what, ...){
      txtDir <- file.path(self$dir, sourceDir)
      param <- list(what = what)
      data <- lapply(
        list.files(txtDir, full.names = FALSE),
        function(filename){
          print(filename)
          Protocol <- do.call(param[["what"]], args = c(list(filename = filename), param))
          Protocol$txt <- scan(
            file = file.path(self$dir, sourceDir, filename), what = "character", sep = "\n", quiet = TRUE
            )
          Protocol$preprocessing()
          Protocol$getMetadata()
          Protocol$metadata
        }
      )
      do.call(rbind, data)
    },
    
    xmlify = function(sourceDir = "txt", targetDir = "tei", ...){
      .xmlifyFile <- function(filename, sourceDir, targetDir, verbose, param){
        Protocol <- eval(parse(text = param[["class"]]))$new(
          filename = filename, sourceDir = sourceDir,
          downloadReportFile = param[["downloadReportFile"]]
        )
        Protocol$xmlify()
        outfile <- file.path(targetDir, gsub("^(.*?)\\.txt$", "\\1.xml", filename))
        cat(Protocol$xml, file = outfile)
      }
      
      dirApply(
        f = .xmlifyFile,
        sourceDir = file.path(self$dir, sourceDir),
        targetDir = file.path(self$dir, targetDir),
        param = list(
          class = self$protocolclass,
          downloadReportFile = self$downloadReportFile
        ), ...
      )
    },
    
    
    regexContextInChunks <- function(sourceDir, regex, ncharContext, protocolclass = "PlenaryProtocol", ...){ 
      data <- lapply(
        list.files(file.path(self$dir, sourceDir), full.names = FALSE),
        function(filename){
          Protocol <- do.call(protocollclass, args = list(filename = filename))
          Protocol$txt <- readLines(filename)
          Protocol$preprocessing()
          Protocol$regexContextInChunks(regex = regex)
          Protocol$metadata
        }
      )
      do.call(rbind, data)
    },
    
    
    xslt = function(sourceDir, targetDir, xslFile = system.file("xsl/plpr.xsl", package = "ctk.plpr"), ...){
      checkDirs(.Object,sourceDir, targetDir)
      message("using xsl-file:", xslFile)
      # callNextMethod(
      #   .Object, sourceDir=sourceDir, targetDir=targetDir,
      #   xslFile=xslFile, ...
      # )
    },
    
    teiToVrt = function(sourceDir, targetDir, ...){
      .teiToVrtWorker <- function(filename, sourceDir, targetDir, verbose, param){
        startTime <- Sys.time()
        tei <- saveXML(xmlParse(file=file.path(sourceDir, filename)), indent=FALSE)
        tei %>>% xslt(xslFile=param[["xslFile"]]) %>>% tokenize(with="treetagger") %>>% treetagger -> vrt
        cat(vrt, file=file.path(targetDir, gsub("^(.*?)\\.xml$", "\\1.vrt", filename)))
        return(Sys.time() - startTime)
      }
      
      dirApply(
        f=.teiToVrtWorker, sourceDir=file.path(self$dir, sourceDir),
        targetDir = file.path(self$dir, targetDir),
        param=list(xslFile=self$xslFile), ...
      )  
    },
    
    teiToHtml = function(
      sourceDir, targetDir, xpath=xpathPlprbttxt,
      pattern="xml", mc=FALSE, progress=TRUE, verbose=FALSE,
      sample=FALSE, filenames=NULL, continue=FALSE
    ){
      .teiToHtml <- function(filename, sourceDir, targetDir, verbose, param){
        xpaths <- param[["xpath"]]
        startTime <- Sys.time()
        doc <- xmlTreeParse(file.path(sourceDir, filename), useInternalNodes=TRUE)
        data <- lapply(xpaths, function(xpath) xmlValue(getNodeSet(doc, path=xpath)[[1]]))
        protocolTitle <- sprintf(
          "# %s %s/%s (%s)",
          data$title, data$legislativePeriod, data$sessionNo, data$date
        )
        debatesList <- lapply(
          getNodeSet(doc, path='//div[@type="agenda_item"]'),
          function(topicNode){
            debateTextList <- lapply(
              xmlChildren(topicNode),
              function(topicNodeChild){
                if (xmlName(topicNodeChild) == "sp"){
                  speechMarkdownList <- lapply(
                    xmlChildren(topicNodeChild),
                    function(speakerNodeChild){
                      if (xmlName(speakerNodeChild) == "speaker"){
                        return("")
                      } else if (xmlName(speakerNodeChild) == "p"){
                        return(xmlValue(speakerNodeChild))
                      } else if (xmlName(speakerNodeChild) == "stage"){
                        return(paste("\n>", xmlValue(speakerNodeChild), "\n", collapse=" "))
                      }
                    })
                  speechMarkdown <- paste(speechMarkdownList, collapse="\n\n")
                  speakerRole <- xmlAttrs(topicNodeChild)["role"]
                  if (speakerRole == "mp"){
                    speakerCall <- sprintf("%s (%s)", xmlAttrs(topicNodeChild)["who"], xmlAttrs(topicNodeChild)["parliamentary_group"])
                  } else if (speakerRole %in% c("government", "federal_council", "parliamentary_commissioner")){
                    speakerCall <- sprintf("%s, %s", xmlAttrs(topicNodeChild)["who"], xmlAttrs(topicNodeChild)["position"])
                  } else if (speakerRole %in% c("presidency")){
                    speakerCall <- sprintf("%s %s", xmlAttrs(topicNodeChild)["position"], xmlAttrs(topicNodeChild)["who"])
                  }
                  speakerMarkdown <- paste("**", speakerCall, "**", sep="")
                  speech <- paste(speakerMarkdown, speechMarkdown, sep=": ")
                  return(speech)
                } else if (xmlName(topicNodeChild) == "stage"){
                  return(paste("*", xmlValue(topicNodeChild), "*"))
                }
              })
            debateText <- paste(debateTextList, collapse="\n\n")
            headingAgendaItem <- paste(
              "## Debatte ",
              xmlAttrs(topicNode)["n"], ': "',
              xmlAttrs(topicNode)["desc"], '" (<i>',
              xmlAttrs(topicNode)["what"], "</i>) ##",
              sep="")
            paste(headingAgendaItem, debateText, sep="\n\n")
          })
        protocolText <- paste(unlist(debatesList), sep="\n\n", collapse="\n\n")
        protocolMarkdown <- paste(protocolTitle, protocolText, sep="\n\n")
        protocolHtml <- markdown::markdownToHTML(text=protocolMarkdown)  
        protocolHtml <- gsub("<.?ul>", "", protocolHtml)
        protocolHtml <- gsub("<li>", "<p>", protocolHtml)
        protocolHtml <- gsub("</li>", "</p>", protocolHtml)
        filenameOut <- paste(strsplit(filename, "\\.")[[1]][1], ".html", sep="")
        cat(protocolHtml, file=file.path(targetDir, filenameOut))
        return(Sys.time() - startTime)
      }
      dirApply(
        f=.teiToHtml,
        sourceDir=file.path(self$dir, sourceDir),
        targetDir=file.path(self$dir, targetDir),
        pattern=pattern, mc=mc, progress=progress, verbose=verbose, sample=sample,
        filenames=filenames, continue=continue,
        param=list(xpath=teiXpath)
      )
    }
  )
)