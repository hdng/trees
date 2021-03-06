% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/squirrels.R
\name{squirrels}
\alias{squirrels}
\title{Release squirrels.}
\usage{
squirrels(x, branches, pos, left = "0", right = "1", plot = TRUE, ...)
}
\arguments{
\item{x}{A \code{plant} object generated by \code{\link{germinate}}.}

\item{branches}{A binary coded vector of addresses of branches upon which 
squirrels (for example) will be placed. The trunk should be coded as 
\code{'Y'}, while all other elements of \code{branches} may only contain the
characters given in args \code{left} and \code{right}. E.g. if \code{left}
and \code{right} are \code{'0'} and \code{'1'}, respectively, then
\code{'0'} is the first branch to the left stemming from the top of the
trunk, while \code{'1'} is the first branch to the right, stemming from the
top of the trunk; \code{'01'} would be a branch forking to the right from
the end of the first branch that forked left off the trunk.}

\item{pos}{A vector whose elements describe the position of squirrels (for 
example), expressed as the distance from the beginning of the branch. This 
vector must be the same length as \code{branches}, and its elements 
correspond to the elements of \code{branches}.}

\item{left}{The character used to represent left-turning branches in the 
\code{branches} vector (default is \code{'0'}). Must not be \code{'Y'}.}

\item{right}{The character used to represent right-turning branches in the 
\code{branches} vector (default is \code{'1'}). Must not be \code{'Y'}.}

\item{plot}{Should the points be added to the current plot?}

\item{...}{Additional arguments controlling the appearance of squirrels (for 
example), passed to \code{points}.}
}
\value{
A \code{data.frame} with branch addresses (\code{branches}), squirrel 
  positions (\code{pos}), and the corresponding \code{x} and \code{y} 
  coordinates. If \code{plot} is \code{TRUE}, the points will be added to the
  current plot.
}
\description{
Add squirrels (for example) at specific locations defined by branch address 
and distance along branch.
}
\examples{
# From http://stackoverflow.com/q/28163979/489704
g <- germinate(list(trunk.height=32,
                    branches=c('0', '1', '00', '01', '010', '011'),
                    lengths=c(21, 19, 5, 12, 6, 2)),
               left='0', right='1', angle=40)

xy <- squirrels(g, c('Y', '0', '010', '0', '00'), pos=c(23, 12, 4, 2, 1), 
                pch=21, bg='white', cex=3, lwd=2)
text(xy$x, xy$y, labels=seq_len(nrow(xy)), font=2)
legend('bottomleft', bty='n',
       legend=paste(seq_len(nrow(xy)), 
                    c('FluffyTail', 'Ginger', 'NutCracker', 'SuperSquirrel', 
                      'ChipnDale'), sep='. '))
}
\seealso{
\code{\link{germinate}} \code{\link{foliate}}
}

