\name{joins}
\alias{joins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Join Two Datasets Together
}
\description{
This function will perform all join operations for two datasets
}
\usage{
tamahagane.apply.join(x,y,joinType="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
Two datasets which are required to join.
}
  \item{joinType}{
Type of join e.g inner, outer, left, right
}
}
\details{
This function will take two datasets and type of join operation and will return result after joininng them on the basis of type of join. Type of join can be ("inner", "outer", "left", "right") different and if you will not specify the type of join then this function will perform "inner" join by default.
}
\value{
Return data after performing joining operations.
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
tamahagane.apply.join(data1,data2, joinType= "outer")
tamahagane.apply.join(data1,data2, joinType= "left")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ join}% use one of  RShowDoc("KEYWORDS")
\keyword{left join, right join, outer join, inner join }% __ONLY ONE__ keyword per line
