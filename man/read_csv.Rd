\name{read_csv}
\alias{read_csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read CSV Function
}
\description{
This function takes the file path as parameter and opens CSV file from given path.
}
\usage{
read_csv(file.path= " your file path")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.path}{
This parameter will take path of CSV file and will give it to the function as input. So that function can read csv file from this given path.
}
}
\details{
Your file path should be in this format, for example ,
With back slashes (file.path= "D:\\folder\\folder\\file.csv")
and With forward slashes (file.path = "D:/folder/folder/file.csv)
}
\value{
This function will read the csv file from given path.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

tamahagane.read.csv(file.path= "C:/farhan/Desktop/Titanic.csv")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ readcsv }% use one of  RShowDoc("KEYWORDS")
\keyword{ readfile }% __ONLY ONE__ keyword per line
