#' @include utils.R three-class.R
NULL


#' @param coords coordinates of points
#' @param size point size
#' @exportMethod points
#' @rdname three
setMethod("points", "three", function(x, coords, size=5, color="0xcccccc"){
  color <- tolower(gsub("^#", "0x", color))
  x@json[["sphereCoords"]] <- toJSON(coords)
  x@json[["sphereSize"]] <- toJSON(size)
  x@json[["sphereColor"]] <- toJSON(color)
  jsRawPrep <- list()
  jsRawLoop <- c(
    startLoop="for (var i = 0; i < sphereCoords.x.length; i++) {",
    geometry="\tvar geometry = new THREE.SphereGeometry( sphereSize[i], 16, 16 );",
    material="\tvar material = new THREE.MeshLambertMaterial( {color: eval(sphereColor[i]), shading: THREE.FlatShading });",
    mesh="\tvar mesh = new THREE.Mesh(geometry, material);",
    meshPosition="\tmesh.position.set(sphereCoords.x[i], sphereCoords.y[i], sphereCoords.z[i]);",
    id="\tmesh.name = ''+i;",
    addScene="\tscene.add(mesh);",
    endLoop="}",
    toReplace="// addHere\n"
  )
  if (length(size) == 1) {
    jsRawLoop <- jsRawLoop[-which(names(jsRawLoop) == "geometry")]
    jsRawPrep[['geometry']] <- "\tvar geometry = new THREE.SphereGeometry( sphereSize, 16, 16 );\n"
  } 
  if (length(color) == 1) {
    jsRawPrep[['material']] <- gsub("sphereColor\\[i\\]", "sphereColor", jsRawLoop['material'])
    jsRawLoop <- jsRawLoop[-which(names(jsRawLoop) == "material")]
  }
  jsRawLoop <- paste("\t\t\t", jsRawLoop, sep="")
  newJs <- paste(
    .list2code(jsRawPrep),
    .list2code(jsRawLoop),
    sep=""
    )
  x@js <- gsub("//\\saddHere\n", newJs, x@js)
  x
})



