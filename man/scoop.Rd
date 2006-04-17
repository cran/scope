\name{scoop}
\alias{scoop}
\title{Look Up Column Values for Rows In Scope}
\description{
 For each vector of row.names in \code{scope}, \code{scoop()} will try to retrieve a value 
 from the specified column.  If the vector contains exactly one row.name, the 
 corresponding value will be returned.  Otherwise, NA is returned. If scope is 
 not specified, the specified column is returned.
}
\usage{
scoop(x, this, scope = NULL)
}
\arguments{
  \item{x}{A data frame.}
  \item{this}{A column name in \code{x}, from which values are taken.}
  \item{scope}{A scope object, ideally all elements atomic.}
}
\value{
 A vector of same length as the data frame columns, and same mode as \code{this}.
}
\seealso{
\code{\link{scope}},
\code{\link{score}},
\code{\link{skim}},
\code{\link{probe}},
}
\examples{
data(Theoph)
#Consider earliest row for each Subject, and return 'conc'.
S <- scope(Theoph,'Subject')
S2 <-scope(Theoph,'Time',that=0,scope=S)
any(score(S2)!=1)
scoop(Theoph,'conc',scope=S2)
}
\keyword{manip}

