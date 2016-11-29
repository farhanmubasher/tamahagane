\name{decision_tree}
\alias{decision_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Decision Tree Function
}
\description{
This function takes two parameters. 1) "formula" will take your model for decision tree. 2) "dataset" will take your data
}
\usage{
decision_tree(formula, dataset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
 This argument will take your model as input. Model includes a label attribute and some other dependent attributes of same dataset. This function will make decision tree on the base of corelation of Label attribute and other dependent attributes.
 labelatt ~ att1+att2+att3
}
\item{dataset}{
 This parameter will take your dataset as input. Data on which you want to perform decision tree.
}
}
\details{
Your formula/model should be include one label attribute and some other attributes (at least one) and all these (label and other) attributes should be from same dataset. So that this function can take decisions on the base of this given model/formula. You can also use dot (".") if you want to perform this function with one label attribute and for all remaining attributes. So you don't have to write all attributes of dataset in the formula
}
\value{
This function will return some data nodes on the basis of some decisions and then you can plot this data nodes by calling tamahagane.plot() function.
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
tamahagane.plot()
}
\examples{
dt <- tamahagane.decision.tree(Titanic$Survived ~ Age + Gender , Titanic)
dt <- tamahagane.decision.tree(Titanic$Survived ~ . , Titanic)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ decision tree }% use one of  RShowDoc("KEYWORDS")
\keyword{tree}% __ONLY ONE__ keyword per line
