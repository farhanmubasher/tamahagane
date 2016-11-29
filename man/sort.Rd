\name{sort}
\alias{sort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sorting Function
}
\description{
This function sort a vector into ascending or descending order.
}
\usage{
tamahagane.sort(attribute, sortType="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{attribute}{
A vector which is given to sort.
}
\item{sortType}{
Type of sort order, Ascending (ASC) or Descending (DESC)

}
}
\details{
This is a gneric function which takes the input as given attribute and type of sort and then sort that attribute.If you will not specify the type of sort then it will sort the vector in ascending order by default.
}
\value{
This function sort the given vector in required order.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Farhan Mubasher (xululabs)
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

tamahagane.sort(myfile$Age, sortType = "DESC")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sorting }% use one of  RShowDoc("KEYWORDS")
\keyword{ sort }% __ONLY ONE__ keyword per line
