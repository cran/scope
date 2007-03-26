\name{probe}
\alias{probe}
\alias{select}
\title{
 Probe a Data Frame for Values Corresponding to a Scoped Aggregate
}
\description{
 For each row of a data frame, find the value in an arbitrary column and in the 
 row where some other column has the scoped aggregated value.
}
\usage{
probe(x, this, FUN = "max", that = this, scope = NULL, ...)
select(x, from, among = NULL, where, is,  ...)
}
\arguments{
  \item{x}{A data frame (probe) or a column name (select)}
  \item{this}{A column name in \code{x}, to which \code{FUN} is applied, within scope.}
  \item{FUN}{An aggregate function, preferably returning one of its arguments.}
  \item{that}{The column from which to select return values, possibly the same
  as \code{this}.}
  \item{scope}{A scope object, identifying rows across which \code{FUN} is applied, or 
   the name of an element in x of class scope.}
  \item{\dots}{Extra arguments to FUN.}
  \item{from}{A data frame.}
  \item{among}{A scope object, or name of one in x.}
  \item{where}{A column in x.}
  \item{is}{A function.}
 
}
\details{
 \code{probe()} is a short-cut for a combination of \code{skim()}, \code{scope()}, and \code{scoop()}.  That 
 is, given a data frame and a scope object, aggregate on one column and use 
 each aggregate value to find the row (if any) where the column value matches; 
 return the value in an arbitrary column for that row. NA is returned if there 
 is not exactly one match for the aggregate.
 
 \code{select()} is a SQL-like reargumentation of \code{probe()}. See example.

}
\value{
 A vector of values of same length and mode as \code{that}.
}

\seealso{
\code{\link{scope}},
\code{\link{score}},
\code{\link{skim}},
\code{\link{scoop}}
}
\examples{
data(Theoph)
#What is the time of the maximum concentration within subject (per row)?
S <- scope(Theoph,'Subject')
T <- probe(Theoph,'conc',that='Time',scope=S)
U <- select('Time',from=Theoph,among=S,where='conc',is='max')
# The T and U vectors above are not the same. You would need to switch 'conc' and 'Time' in
# the select argument above to do the same thing. 
}
\keyword{manip}

