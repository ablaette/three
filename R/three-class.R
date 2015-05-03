#' three class for preparing three.js scripts
#' 
#' to be displayed with a browser
#' 
#' @slot js character, the JavaScript code
#' @slot json a list with character strings 
#' @slot type the display mode
#' @slot threejsDir directory with js files, may be a web folder
#' @slot htmlDir directory where to put the files
#' @rdname three-class
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
setMethod("show", "three", function(object){
  if (get('session', '.GlobalEnv')@jsDir == ""){
    tmpFileJs <- tempfile(fileext=".html")  
    tmpFileJson <- tempfile(fileext=".js")
    httpFileJson <- tmpFileJson
  } else {
    tmpFileJs <- tempfile(fileext=".html", tmpdir=get('session', '.GlobalEnv')@jsDir)  
    tmpFileJson <- tempfile(fileext=".js", tmpdir=get('session', '.GlobalEnv')@jsDir)
    jsFilenamePrep <- unlist(strsplit(tmpFileJson, "/"))
    httpFileJson <- file.path("http://134.91.37.242/js/R", jsFilenamePrep[length(jsFilenamePrep)])
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
  browseURL(tmpFileJs)
  return(c(tmpFileJs=tmpFileJs, tmpFileJson=tmpFileJson))
})
