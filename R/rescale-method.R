setGeneric("rescale", function(object, ...) standardGeneric("rescale"))

.rescale <- function(values, min, max){
  values <- values - min(values)
  values <- values / max(values)
  values <- values * (max - min)
  values <- values + min
  values
}

#' rescale coordinates of an object
#' 
#' bla
#' 
#' @param object object
#' @param min min
#' @param max max
#' @name rescale
#' @aliases rescale,igraph-method
#' @rdname rescale
#' @exportMethod rescale
setMethod("rescale", "igraph", function(object, min, max){
  V(object)$x <- .rescale(V(object)$x, min, max)
  V(object)$y <- .rescale(V(object)$y, min, max)
  V(object)$z <- .rescale(V(object)$z, min, max)
  object
})