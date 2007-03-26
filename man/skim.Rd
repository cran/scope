\name{skim}
\alias{skim}
\title{Aggregate Across Data Frame Subsets Defined by a Scope Object.}
\description{
Apply a function to values in the specified data frame column, across each group
of rows specified in the scope object.
}
\usage{
skim(x, this, FUN = "max", scope = NULL, ...)
}
\arguments{
  \item{x}{A data frame.}
  \item{this}{A column name in \code{x}, the main argument to \code{FUN}.}
  \item{FUN}{An aggregate function.}
  \item{scope}{A scope object, e.g. created by \code{scope()}.}
  \item{\dots}{Extra arguments to pass to \code{FUN}.}
}
\details{
 The default scope is all rows.  The default function is \code{max(...)}.  
 Remember that \code{skim()} returns a data vector, not a scope object.  For 
 functions that return one of their arguments (perhaps \code{max} and \code{min} but perhaps 
 not \code{mean}) the result of \code{skim()} can be passed back to \code{scope()} to \sQuote{probe} for 
 the row from which the value originated.  There is a danger that more or fewer 
 than one row will be detected, so check with \code{score()}.

}
\value{
 A vector of same length as \code{this} giving aggregated values.
}

\seealso{
\code{\link{scope}},
\code{\link{score}},
\code{\link{scoop}},
\code{\link{probe}}
}
\examples{
data(Theoph)
#What is the maximum concentration for each subject?
S <- scope(Theoph,'Subject')
m <- skim(Theoph,'conc',scope=S)
}
\keyword{manip}
