\name{scope}
\alias{scope}
\title{Specify Arbitrary Row Name Subsets}
\description{
 Create or refine a list of vectors of row names.  Each vector corresponds to 
 a row in a data frame, and its elements identify rows relevant to that row.
 Relevance is determined by the passed arguments.  Refinement consists of 
 passing the output back to the function with further criteria, effectively 
 nesting serial restrictions (scope can shrink but not grow).
}
\usage{
scope(x, this, FUN = "==", that = x[[this]], scope = NULL)
}
\arguments{
  \item{x}{A data frame.}
  \item{this}{A column name in \code{x}, representing the first argument to \code{FUN}.}
  \item{FUN}{A function taking two arguments, typically a comparison operator.}
  \item{that}{A column name in \code{x}, representing the second argument to \code{FUN};
   or a vector of same length as columns in \code{x}; or an atomic value to be recycled.}
  \item{scope}{
  A scope object, e.g. created by previous use of \code{scope}.
  Specifically, a list (same length as columns in \code{x}) of vectors of row 
  names in \code{x}.}
}
\details{
 A data frame and one of its column names must be specified (\code{x}, \code{this}).  Defaults 
 are chosen so that if nothing else is specified, per-row scope will be all 
 rows with the same value of \code{this} as the row in question.  \code{FUN} will operate on
 \code{this} and \code{that}, within \code{scope}, which is all rows, by default.  \emph{All} values 
 of \code{this} within \code{scope} will be compared to \emph{each} value of \code{that}. If comparison 
 evaluates to \code{TRUE}, the corresponding row name is retained in \code{scope}. 
}
\value{
  A list of vectors, one per row in \code{x}, of relevant row names in \code{x}.
}
\seealso{ 
\code{\link{score}},
\code{\link{skim}},
\code{\link{scoop}},
\code{\link{probe}}
}
\examples{
data(Theoph)
#For each row, consider only those rows with the same Subject
S <- scope(x=Theoph, this='Subject', FUN='==', that='Subject')
#Same effect as...
S <- scope(Theoph,'Subject')
#For each row within Subject, consider only those rows having non-zero times.
S2 <- scope(Theoph,'Time','>',0,scope=S)
}
\keyword{manip}

