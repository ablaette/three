#' three class for preparing three.js scripts
#' 
#' to be displayed with a browser
#' 
#' @slot js character, the JavaScript code
#' @slot json a list with character strings 
#' @slot type the display mode
#' @rdname three-class
#' @exportClass three
setClass(
  "three",
  slots=c(
    js="character",
    json="list",
    type="character"
  )
)

#' @exportMethod show
#' @param object a three object
#' @rdname three
setMethod("show", "three", function(object){
  tmpFileJson <- tempfile(fileext=".js")
  cat(
    paste(
      unlist(lapply(names(object@json), function(name){ paste(name, " = ", object@json[[name]], ";", sep="") })),
      collapse="\n"
    ),
    file=tmpFileJson
    )
  object@js <- gsub(
    'src="jsonDataFile.js"',
    paste('src="', tmpFileJson, '"', sep=""),
    object@js
    )
  tmpFileJs <- tempfile(fileext=".html")
  cat(object@js, file=tmpFileJs)
  browseURL(tmpFileJs)
  return(c(tmpFileJs=tmpFileJs, tmpFileJson=tmpFileJson))
})
