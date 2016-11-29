\name{plot}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
X-Y Plotting Function
}
\description{
Generic function for plotting R objects
}
\usage{
tamahagane.plot(x, y, title="", xLabel="", yLabel="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x coordinates of points in the plot.
}
  \item{y}{
y coordinates of points in the plot.
}
  \item{title}{
x coordinates of points in the plot.
}
  \item{xLabel}{
A title for x-axis.
}
  \item{yLabel}{
A title for y-axis.
}
}
\details{
This function takes five parameters.Atleast one of (x, y) them is required to run this function. Other parameters are optional you can also assign them values for better understanding of plotting.
}
\value{
This function plots the graph on the basis of given attributes.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Farhan Mubasher (xululabs)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
tamahagane.plot(cars$sapeed, cars$distance, title="My Graph", xLabel="Speed", yLabel="Distance")

tamahagane.plot(cars$speed)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
