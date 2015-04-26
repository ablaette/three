#' @include utils.R three-class.R
NULL

#' @param from data frame with coordinates of starting points of lines
#' @param to data frame with coordinates of end points of lines
#' @param color color of line / point etc.
#' @param lwd line width, defaults to 5
#' @rdname three
#' @exportMethod lines
setMethod("lines", "three", function(x, from, to, color="0x000000", lwd=5){
  color <- tolower(gsub('^#', '0x', color))
  x@json[["linesFrom"]] <- toJSON(from)
  x@json[["linesTo"]] <- toJSON(to)
  x@json[["linesColor"]] <- toJSON(color)
  x@json[["linesWidth"]] <- toJSON(lwd)
  newJs <- c(
    startLoop="for (var i = 0; i < linesFrom.x.length; i++){",
    material="\tvar material = new THREE.LineBasicMaterial({ color: eval(linesColor), linewidth: eval(linesWidth) });",
    geometry="\tvar geometry = new THREE.Geometry();",
    defineLine1="\tgeometry.vertices.push(",
    defineLine2="\tnew THREE.Vector3( linesFrom.x[i], linesFrom.y[i], linesFrom.z[i] ),",
    defineLine3="\tnew THREE.Vector3( linesTo.x[i], linesTo.y[i], linesTo.z[i] )",
    defineLine4="\t);",
    generateLine="\tvar line = new THREE.Line( geometry, material );",
    addScene="\tscene.add(line);",
    endLoop="}",
    replaceTag="// addHere\n"
  )
  if (length(color) == nrow(from)) newJs['material'] <- gsub('eval\\(linesColor\\)', 'eval(linesColor[i])', newJs['material'])
  if (length(lwd) == nrow(from)) newJs['material'] <- gsub('eval\\(linesWidth\\)', 'eval(linesWidth[i])', newJs['material'])
  newJs <- paste("\t\t\t", newJs, sep="")
  newJs <- .list2code(newJs)
  x@js <- gsub("//\\saddHere\n", newJs, x@js)
  x
})

