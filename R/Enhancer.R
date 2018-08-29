#' Enrich TEI document.
#' 
#' @field sourceDir the directory (full path) with the XML files to enhance
#' @field speakerdata a \code{data.table} with the names of a speaker, and his/her role, inter alia
#' @section Methods:
#' \describe{
#'   \item{\code{$new(sourceDir, datafiles, aliasfile)}}{Initialize a new object of class Enhancer.
#'   Upon initialization, the \code{getSpeakerData}-method will be called to read in the content of
#'   the datafiles, and the aliasfile is parsed.}
#'   \item{\code{$getSpeakerData(datafiles, verbose = FALSE)}}{Read in tables defined by datafiles,
#'   thereby filling field speakerdata. Self is returned invisibly.}
#'   \item{\code{$getSpeakerAttributes(element = "sp", attrs = c("who", "role"), verbose = TRUE)}}{
#'   Iteratre through files in sourceDir, and extract information about speakers from XML files.
#'   The result is kept in the field speakerattributes.}
#'   \item{\code{$getAdditionalSpeakerInfos(.BY)}}{Helper function, to enrich the speakerattributes
#'   data.table.}
#'   \item{\code{$consolidateAndEnrichtSpeakerattributes()}}{bla}
#'   \item{\code{$stripTitlesFromName(names)}}{Remove titles from name.}
#' }
#' 
#' @section Arguments:
#' \describe{ \item{datafiles}{a named character vector with filenames (full
#' path) that provide additional information on speakers; the names of the
#' vector indicate the role of the speakers in the respective table (e.g. "mp",
#' "gov", or "pres"), if the name is "supplement", the table is expected to
#' include a column "role" that will provide the information about the role of
#' the speaker.}
#' }
#' @export Enhancer
Enhancer <- R6Class(
  
  "Enhancer",
  
  public = list(
    
    sourceDir = NULL, # character
    targetDir = NULL,
    speakerdata = NULL,
    speakerattributes = NULL,
    aliases = NULL,
    aliasfile = NULL,
    
    # methods 
    
    initialize = function(sourceDir, targetDir = NULL, datafiles = NULL, aliasfile = NULL){
      self$sourceDir <- sourceDir
      if (!is.null(targetDir)) self$targetDir <- targetDir
      if (!is.null(datafiles)) self$getSpeakerData(datafiles)
      if (!is.null(aliasfile)) self$aliases <- yaml::yaml.load_file(aliasfile)
    },
    
    getSpeakerData = function(datafiles, verbose = FALSE){
      speaker_data_list <- lapply(
        names(datafiles),
        function(x) {
          if (verbose) message("... reading: ", datafiles[x])
          colsToGet <- if (x == "supplement") c("lp", "name", "party", "role") else c("lp", "name", "party")
          DT <- data.table::fread(datafiles[x], sep = ";", header = TRUE, stringsAsFactors = FALSE, select = colsToGet)
          if (x != "supplement") DT[["role"]] <- x
          setcolorder(DT, neworder = c("lp", "name", "party", "role"))
          DT
        }
      )
      self$speakerdata <- rbindlist(speaker_data_list)
      invisible(self)
    },
    
    getSpeakerAttributes = function(element = "sp", attrs = c("who", "role"), verbose = TRUE){
      filesInSourceDir <- list.files(self$sourceDir)
      filesGroupedByLp <- split(
        filesInSourceDir,
        gsub("^.*?_(\\d+)_.*?$", "\\1", filesInSourceDir)
      )
      if (verbose) message("... getting unique speaker names")
      speakerAttrsList <- lapply(
        names(filesGroupedByLp),
        function(lp){
          attrValuesMatrix <- ctk::getAttributeValues(
            .Object = self$sourceDir, element = element, attrs = attrs,
            unique = TRUE, progress = FALSE, filenames = filesGroupedByLp[[lp]]
          )
          data.table(lp = lp, attrValuesMatrix)
        })
      self$speakerattributes <- rbindlist(speakerAttrsList)
      invisible(self$speakerattributes)
    },
    
    getAdditionalSpeakerInformation = function(.BY){
      dataSubset <- self$speakerdata[role == .BY[["role"]]][lp == .BY[["lp"]]]
      who <- self$stripTitlesFromName(.BY[["who"]])
      who <- gsub("\\s+", " ", who)
      if (grepl("\\s*\\(.*?\\)\\s*$", who)) who <- gsub("\\s*\\(.*?\\)\\s*$", "", who)
      hasAlias <- who %in% names(self$aliases)
      if (any(grepl(who, dataSubset[["name"]]))){
        index <- grep(who, dataSubset[["name"]])
      } else if (hasAlias == TRUE && any(grepl(self$aliases[[who]], dataSubset[["name"]])) == TRUE){
        index <- grep(self$aliases[[who]], dataSubset[["name"]])
      } else if (any(agrepl(who, dataSubset[["name"]])) == TRUE){
        index <- agrep(who, dataSubset[["name"]])
      } else {
        index <- NULL
      }
      if (length(index) == 1){
        party <- dataSubset[["party"]][index]
        name <- dataSubset[["name"]][index]
      } else if (length(index) > 1){
        partyUnique <- unique(dataSubset[["party"]][index])
        nameUnique <- unique(dataSubset[["name"]][index])
        if (length(nameUnique) == 1){
          name <- nameUnique
        } else {
          name <- dataSubset[["name"]][index[which.min(as.vector(adist(who, dataSubset[["name"]][index])))]]
        }
        if (length(partyUnique) == 1){
          party <- partyUnique
        } else {
          party <- dataSubset[["party"]][which(dataSubset[["name"]] == name)[1]]
        }
      } else if (length(index) == 0){
        name <- "NA"
        party <- "NA"
      }
      list(
        party = gsub("^\\s*(.*?)\\s*$", "\\1", party),
        name = gsub("^\\s*(.*?)\\s*$", "\\1", name)
      )
    },
    
    stripTitlesFromName = function(names){
      toRemove <- c(
        "Dr\\.-Ing\\.\\s*", "Dr\\.\\s*", "Prof\\.\\s*", "Professor\\s*", "Professorin\\s*",
        "h\\.\\s*c\\.", "Dipl\\.\\s*", "\\(.*?\\)", "\\(", "\\)",
        "\\|", "#"
      )
      for (del in toRemove) names <- gsub(del, "", names)
      names
    },
    
    consolidateAndEnrichSpeakerattributes = function(){
      self$speakerattributes <- self$speakerattributes[,self$getAdditionalSpeakerInformation(.BY), by = c("lp", "who", "role"), with = TRUE]
      invisible(self$speakerattributes)
    },
    
    analyseMissingInformation = function(sourceDir, element = "sp", attrs = c("who", "parliamentary_group", "role", "party", "name", "position"), mc = FALSE){
      attrDfMissing <- subset(self$speakerattributes, name %in% c("", "NA"))
      attrDfMissingByRole <- split(attrDfMissing, f = attrDfMissing$role)
      attrDfMissingByRole
    },
    
    enhanceFile = function(filename){
      startTime <- Sys.time()
      doc <- xml2::read_xml(x = file.path(self$sourceDir, filename))
      lpOfDoc <- xml2::xml_text(xml2::xml_find_all(doc, "//legislativePeriod")[[1]])
      dtSubsetLp <- self$speakerattributes[lp == lpOfDoc]
      nodes <- xml2::xml_find_all(doc, xpath = paste("//", "sp"))
      lapply(
        nodes,
        function(node){
          nodeAttrs <- xml2::xml_attrs(node)
          dtSubset <- dtSubsetLp[role == nodeAttrs["role"]][who == nodeAttrs["who"]]
          if (nrow(dtSubset) == 1){
            xml2::xml_attr(node, attr = "party") <- dtSubset[["party"]]
            xml2::xml_attr(node, attr = "name") <- dtSubset[["name"]]
          } else if (nrow(dtSubset) > 2){
            warning(nodeAttrs)
          } else {
            xml2::xml_attr(node, attr = "name") <- "NA"
            xml2::xml_attr(node, attr = "party") <- "NA"
          }
        }
      )
      if (is.null(targetDir)){
        return( as.character(doc) )
      } else {
        xml2::write_xml(doc, file = file.path(self$targetDir, filename), encoding = "UTF-8", options = "format")
        invisible( as.character(Sys.time() - startTime) )
      }
    }
  )
)