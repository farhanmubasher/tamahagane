\name{filters}
\alias{filters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filtering Function for Dataset
}
\description{
This function is used to apply different filters on datasets.
}
\usage{
tamahagane.apply.filter(dataset, attribute, filterType="", object="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
A paramter which will tkae your dataset.
}
  \item{attribute}{
Specific attribute of given dataset, which will be used for filtering data on the ase of some given object.
}
  \item{filterType}{
Type of filter you want to apply to given dataset. for Example ("EQUAL", "NOTEQUAL", "ISIN", "ISNOTIN", "STARTWITH", "MATCHES", "NOTMATCHES")
}
  \item{object}{
A value of given attribute, which is used to filter that attribute
}
}
\details{
This function will take four parameters and will return the filtered data on the basis of given attribute , its object,  and filter type. Each row will be compare according to applied filter and ata the end filtered data will be shown.Do not leave any parameter blank or empty otherwise it will produce error message.Give all the parameters of this function correctly.
}
\value{
This function will return Filtered Data.
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
female <- tamahagane.apply.filter(myData, myData$Sex, filterType = "EQUAL", object = "Female" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
