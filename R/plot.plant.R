#' Plot method for plant objects.
#' 
#' @param x The \code{plant} object to be plotted.
#' @param trunk.width The plotting line width for the trunk. Successive child
#'   branches are plotted with increasingly finer \code{lwd}, to a minimum of
#'   \code{1}.
#' @param add If \code{TRUE}, the plant will be added to the current plot.
#' @param ... Additional arguments passed to \code{plot} and \code{segments}.
#' @return \code{NULL}
#' @seealso \code{\link{germinate}}
#' @export
plot.plant <- function(x, trunk.width=20, add=FALSE,
    branch.width=10, branch.text.size=0.5,
    node.size=2, node.label.size=0.75, node.text.size=0.5, ...) {
  if(isTRUE(add)) {
    with(x, segments(x0, y0, x1, y1, col=colors, lwd=pmax(trunk.width/nchar(x$branches), 1), ...))
  } else {
    plot(c(x$x0, x$x1), c(x$y0, x$y1+2), type='n', asp=1, axes=FALSE, xlab='', ylab='', ...)  
    #with(x, segments(x0, y0, x1, y1, col=colors, lwd=pmax(trunk.width/nchar(x$branches), 1), ...)) 
    with(x, segments(x0, y0, x1, y1, col=branch.colors, lwd=branch.width, ...)) 
    with(x, points(x1, y1, pch=21, col='black', bg=node.colors, cex=node.size, ...)) 
    with(x, text(x1, y1, labels=node.labels, col='black', cex=node.label.size, ...)) 
    with(x, text(x1, y1+1, labels=node.texts, col='black', cex=node.text.size, ...)) 
    with(x, text((x0+x1)/2, (y0+y1)/2, labels=branch.texts, col='black', cex=branch.text.size, ...)) 
  }
}
