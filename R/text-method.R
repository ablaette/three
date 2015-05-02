#' @include utils.R three-class.R
NULL


#' @param x a three object
#' @param .data a data.frame with three columns providing x/y/z coordinates, and rownames providing text
#' @param offset offset of the text
#' @exportMethod text
#' @rdname three
setMethod("text", "three", function(x, .data, color="0x000000", size=15, offset=c(x=5, y=5, z=5)){
  color <- toupper(gsub('^0x', '#', color))
  x@json[["text"]] <- toJSON(rownames(.data))
  x@json[["textCoords"]] <- toJSON(.data)
  x@json[["textOffset"]] <- toJSON(offset)
  x@json[["textSize"]] <- toJSON(size)
  x@json[["textColor"]] <- toJSON(color)
  newJs <- c(
    canvasSize="var size = 256;",
    startLoop="for (var i = 0; i < text.length; i++){",
    createCanvas="\tvar canvas = document.createElement('canvas');",
    canvasWidth="\tcanvas.width = size;",
    canvasHeight="\tcanvas.height = size;",
    getCanvas="\tvar context = canvas.getContext('2d');",
    textColor="\tcontext.fillStyle = textColor;",
    textAlign="\tcontext.textAlign = 'center';",
    textSize="\tcontext.font = textSize + 'px Arial';",
    textStyle="\tcontext.fillText(text[i], size / 2, size / 2);",
    asTexture="\tvar amap = new THREE.Texture(canvas);",
    update="\tamap.needsUpdate = true;",
    defineSprite="\tvar mat = new THREE.SpriteMaterial({map: amap, transparent: false, useScreenCoordinates: false, color: 0xffffff});",
    createSprite="\tvar sp = new THREE.Sprite(mat);",
    scaleSprite="\tsp.scale.set( 100, 100, 10 );",
    positionSprite="\tsp.position.set(textCoords.x[i] - textOffset.x, textCoords.y[i] - textOffset.y, textCoords.z[i] - textOffset.y);",
    addScene="\tscene.add(sp);",
    endLoop="}",
    toReplace="// addHere\n"
  )
  if (length(color) == nrow(.data)) newJs["textColor"] <- c("\tcontext.fillStyle = textColor[i];")
  if (length(size) == nrow(.data)) newJs["textSize"] <- c("\tcontext.font = textSize[i] + 'px Arial';")
  newJs <- paste("\t\t\t", newJs, sep="")
  newJs <- .list2code(newJs)
  x@js <- gsub("//\\saddHere\n", newJs, x@js)
  x
})

