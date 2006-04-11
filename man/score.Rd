\name{score}
\alias{score}
\title{Calculate Lengths of Elements for a Scope Object}
\description{
 For each row represented by a scope object, give the number of row names that 
 are \sQuote{in scope}: i.e. the lengths of the per-row vectors. 
}
\usage{
score(scope)
}
\arguments{
  \item{scope}{A scope object, e.g. created by \code{scope()}.}
}
\details{
  This is a useful analytical tool, e.g., for checking whether previous calls 
  to \code{scope()} have limited per-row scopes to exactly one row name (a 
  condition expected by \code{scoop()}).
}
\value{
  A vector of integers, representing numbers of row names per element in a scope object.
}

\seealso{
\code{\link{scope}},
\code{\link{skim}},
\code{\link{scoop}},
\code{\link{probe}},

}
\examples{
data(Theoph)
#Consider rows within Subject.
S <- scope(Theoph,'Subject')
#How many row names in scope, per row?
min(score(S))
max(score(S))
}
\keyword{manip}

