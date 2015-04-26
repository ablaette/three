#' @include utils.R three-class.R
NULL

setGeneric("light", function(x, ...) standardGeneric("light"))

#' @param pointLight a list with the x,y,z-coordinates of a point light
#' @rdname three
#' @exportMethod light
setMethod("light", "three", function(x, pointLight=list(x=0, y=0, z=0)){
  newJsRaw <- c(
    "scene.add( new THREE.AmbientLight( 0x444444 ) );",
    "var particleLight;",
    "particleLight = new THREE.Mesh( new THREE.SphereGeometry( 4, 8, 8 ), new THREE.MeshBasicMaterial( { color: 0xffffff } ) );",
    "scene.add( particleLight );",
    "var pointLight = new THREE.PointLight( 0xaaaaaa, 1);",
    "particleLight.add( pointLight );",
    sprintf("particleLight.position.x = %d;", pointLight[["x"]]),
    sprintf("particleLight.position.y = %d;", pointLight[["y"]]),
    sprintf("particleLight.position.z = %d;", pointLight[["z"]]),
    "// addHere\n"
  )
  newJs <- .list2code(newJsRaw)
  x@js <- gsub("//\\saddHere\n", newJs, x@js)
  x
})


