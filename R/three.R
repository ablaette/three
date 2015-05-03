#' create a three-object
#' 
#' @param type character vector providing display mode ("base", "anaglyph", "stereo")
#' @param bgColor the background color
#' @param jsUrlPrefix if not provided, the js files in the package will be linked, if provided
#' the respective web folder will be referenced
#' @rdname three
#' @aliases light,three-method light
#' @export three
#' @examples
#' \dontrun{
#' vis <- three(type="base", bgColor="0xcccccc")
#' vis <- light(vis)
#' coords <- mtcars[, c("mpg", "wt", "cyl")]
#' coords <- coords * 10
#' colnames(coords) <- c("x", "y", "z")
#' vis <- points(vis, coords=coords, color="#1111FF")
#' vis <- three:::text(vis, .data=data.frame(coords, row.names=rownames(mtcars)), color="0xff111111")
#' vis
#' }
three <- function(type="base", bgColor="0x000000", jsUrlPrefix=NULL, adjust=list()){
  htmlTemplate <- scan(
    file=system.file("templates", paste(type, ".html", sep=""), package="three"),
    what="character", sep="\n", quiet=TRUE
  )
  lookUp <- c("three.min.js", "TrackballControls.js", "AnaglyphEffect.js", "StereoEffect.js")
  if (is.null(jsUrlPrefix)){
    jsPath <- sapply(lookUp, function(x) system.file("js", x, package="three"))  
  } else {
    jsPath <- sapply(lookUp, function(x) file.path(jsUrlPrefix, x))
  }
  
  html3d <- paste(htmlTemplate, collapse="\n")
  for (x in c("three.min.js", "TrackballControls.js")) html3d <- gsub(x, jsPath[x], html3d)
  if (type == "anaglyph") html3d <- gsub("AnaglyphEffect.js", jsPath["AnaglyphEffect.js"], html3d)
  if (type == "stereo") html3d <- gsub("StereoEffect.js", jsPath["AnaglyphEffect.js"], html3d)
  for (adj in names(adjust)) html3d <- gsub(adj, adjust[[adj]], html3d)
  html3d <- gsub(
    "renderer.setClearColor\\(.*?\\);",
    sprintf("renderer.setClearColor(%s, 1);", bgColor),
    html3d
  )
  new("three", js=html3d, type=type)
}

