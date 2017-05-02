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
#' @param tree.rotation: tree rotation 
#' values = c(0, 90, 180) ~ (bottom-up, left-right, top-down)
#' @param text.angle: text angle, if NULL then auto determine
plot.plant <- function(x, trunk.width=20, add=FALSE,
    tree.rotation=180, text.angle=NULL,
    branch.width=1, branch.text.size=0.5,
    node.size=2, node.label.size=0.75,
    node.text.size=0.5, tree.label=NULL, ...) {

  # distance from node to its label
  l = max(abs(c(x$y0, x$y1)))/30
  r = 1
  #print(tree.rotation)
  if (tree.rotation == 90){
    if (is.null(text.angle)){text.angle=90}
    tmp = x$x0; x$x0=x$y0; x$y0=-tmp
    tmp = x$x1; x$x1=x$y1; x$y1=-tmp
  }else if (tree.rotation == 180){
     r = -1
     x$y0 = r*x$y0; x$y1 = r*x$y1
  }
  if (is.null(text.angle)){text.angle = 0}
  #cat('text.angle = ', text.angle, '\n')

  if(isTRUE(add)) {
    with(x, segments(x0, y0, x1, y1, col=colors, lwd=pmax(trunk.width/nchar(x$branches), 1), ...))
  } else {
    if (tree.rotation == 90){
        plot(c(x$x0-2*r, x$x1+1*r), c(x$y0, x$y1), type='n', asp=1, axes=FALSE, xlab='', ylab='', ...)  
    }else{
        plot(c(x$x0, x$x1), c(x$y0, x$y1+2*r), type='n', asp=1, axes=FALSE, xlab='', ylab='', ...)  
    }
    #with(x, segments(x0, y0, x1, y1, col=colors, lwd=pmax(trunk.width/nchar(x$branches), 1), ...)) 
    #with(x, segments(x0, y0, x1, y1, col=branch.colors, lwd=branch.width, ...)) 
    with(x, draw.branch(x0, y0, x1, y1, fill.color=branch.colors,
        border.color=branch.border.colors, border.linetype=branch.border.linetypes,
        border.width=branch.border.widths,
        w=branch.width, ...)) 
    with(x, points(x1, y1, pch=21, col=node.border.colors,
        lwd=node.border.widths,
        bg=node.colors, cex=node.size, ...)) 
    with(x, text(x1, y1, labels=node.labels, col='black', cex=node.label.size, srt=text.angle, ...))
    if (tree.rotation == 90){
        with(x, text(x1+l*r, y1, labels=node.texts, col='black', cex=node.text.size, 
            srt=text.angle, ...))
        if(!is.null(tree.label)){
            text(x$x0[1]-2*l, x$y0[1], label=tree.label, cex=node.label.size, srt=text.angle)
        }
    }else{
        with(x, text(x1, y1+l*r, labels=node.texts, col='black', cex=node.text.size,
            srt=text.angle,...)) 
    }
    with(x, text((x0+x1)/2, (y0+y1)/2, labels=branch.texts, col='black',
        cex=branch.text.size, srt=text.angle, ...)) 
  }
}

# Draw tree branch using polygon that allows for
# choosing border style and fill
#' @example:
#' plot(x=c(0,30), y=c(0, 30)); draw.branch(c(5, 20, 12, 5, 5, 20), c(5, 20, 10, 20, 25, 25), c(10, 12, 20, 12, 10, 20), c(12, 15, 5, 12, 25, 29), border.linetype=c(1, 2, 3, 4,1,2), border.width=c(1,2,1,2,2,2))


draw.branch <- function(x0, y0, x1, y1, w=1, border.color='black', border.linetype='solid', border.width=0.5, fill.color=NULL, ...){
    X0 = x0; Y0=y0; X1=x1; Y1=y1
    #print(w)
    w = w/2
    #cat('w===', w, '\n')
    #arrows(x0, y0, x1, y1, col='red')
    n = length(x0)
    if (length(border.color) == 1){border.color = rep(border.color, n)}
    if (length(fill.color) == 1){fill.color = rep(fill.color, n)}
    if (length(border.linetype) == 1){border.linetype = rep(border.linetype, n)}
    if (length(border.width) == 1){border.width = rep(border.width, n)}
    for (i in 1:length(X0)){
        x0 = X0[i]; y0 = Y0[i]; x1 = X1[i]; y1 = Y1[i]
        # don't need to catch for atan(Inf), still works
        #if(x0 == x1){
        #    a = pi/2
        #}else{
            a = atan((y1-y0)/(x1-x0))
        #}
        x01 = x0 - sin(a)*w
        x02 = x0 + sin(a)*w
        y01 = y0 + cos(a)*w
        y02 = y0 - cos(a)*w
        x11 = x1 + sin(a)*w
        x12 = x1 - sin(a)*w
        y11 = y1 - cos(a)*w
        y12 = y1 + cos(a)*w
        xx = c(x01, x02, x11, x12, x01)
        yy =  c(y01, y02, y11, y12, y01)
        #print(xx)
        #print(yy)
        polygon(xx, yy, col=fill.color[i], border=border.color[i], lty=border.linetype[i], lwd=border.width[i])

        if (x0 > x1){
            #rect

            # nice
            #polygon(c(x0, x0-b, x1, x1, x1+b, x0, x0), c(y0, y0, y1-b, y1, y1, y0+b, y0), ...)
        }else{
            # nice
            #polygon(c(x0, x0+b, x1, x1, x1-b, x0, x0), c(y0, y0, y1-b, y1, y1, y0+b, y0), ...)
        }
    }

}



