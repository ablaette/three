#' three class for preparing three.js scripts
#' 
#' to be displayed with a browser
#' 
#' @slot js character, the JavaScript code
#' @slot json a list with character strings 
#' @slot type the display mode
#' @slot threejsDir directory with js files, may be a web folder
#' @slot htmlDir directory where to put the files
#' @aliases store
#' @rdname three
#' @exportClass three
setClass(
  "three",
  slots=c(
    js="character",
    json="list",
    type="character",
    threejsDir="character",
    htmlDir="character"
  )
)

#' @exportMethod show
#' @param object a three object
#' @rdname three
#' @importFrom utils browseURL
setMethod("show", "three", function(object){
  filenames <- store(object)
  browseURL(filenames[1])
  return(c(tmpFileJs=filenames[1], tmpFileJson=filenames[2]))
})

setGeneric("store", function(object, ...) standardGeneric("store"))

#' @param directory evident
#' @param filePrefix evident, too
#' @exportMethod store
#' @rdname three
setMethod("store", "three", function(object, directory=NULL, filePrefix=NULL){
  if (is.null(directory)) directory <- tempdir()
  if (is.null(filePrefix)) {
    tmpFileJs <- tempfile(fileext=".html", tmpdir=directory)  
    tmpFileJson <- tempfile(fileext=".js", tmpdir=directory)
    httpFileJson <- tmpFileJson
  } else {
    tmpFileJs <- file.path(directory, paste(filePrefix, ".html", sep=""))
    tmpFileJson <- file.path(directory, paste(filePrefix, ".js", sep=""))
    jsFilenamePrep <- unlist(strsplit(tmpFileJson, "/"))
    httpFileJson <- file.path(directory, jsFilenamePrep[length(jsFilenamePrep)])
  }
  cat(
    paste(
      unlist(lapply(names(object@json), function(name){ paste(name, " = ", object@json[[name]], ";", sep="") })),
      collapse="\n"
    ),
    file=tmpFileJson
  )
  object@js <- gsub(
    'src="jsonDataFile.js"',
    paste('src="', httpFileJson, '"', sep=""),
    object@js
  )
  cat(object@js, file=tmpFileJs)
  return(c(tmpFileJs=tmpFileJs, tmpFileJson=tmpFileJson))
})