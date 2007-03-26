\name{scope}
\alias{scope}
\alias{print.scope}
\alias{format.scope}
\alias{[<-.scope}
\alias{[.scope}
\alias{[[.scope}
\alias{c.scope}

\title{Specify Arbitrary Row Name Subsets}
\description{
 Create or refine a list of vectors of row names.  Each vector corresponds to 
 a row in a data frame, and its elements identify rows relevant to that row.
 Relevance is determined by the passed arguments.  Refinement consists of 
 passing the output back to the function with further criteria, effectively 
 nesting serial restrictions (scope can shrink but not grow).
}
\usage{
scope(x, this, FUN = NULL, that = NULL, scope = NULL, ...)
"[<-.scope"(x,...,value)
"[.scope"(x, ..., drop = TRUE)
"[[.scope"(x, ..., drop = TRUE)
c.scope(..., recursive = FALSE)
}
\synopsis{
scope(x, this, FUN = NULL, that = NULL, scope = NULL, ...)
}
\arguments{
  \item{x}{A data frame.}
  \item{this}{A column name in \code{x}, representing the first argument to \code{FUN}.}
  \item{FUN}{A function taking two arguments, typically a comparison operator.}
  \item{that}{A column name in \code{x}, representing the second argument to \code{FUN};
   or a vector of same length as columns in \code{x}; or an atomic value to be recycled.}
  \item{scope}{
  A scope object, e.g. created by previous use of \code{scope()}.
  Specifically, a list (same length as columns in \code{x}) of vectors of row 
  names in \code{x}. An atomic character string will be understood as an element of x.}
  \item{...}{Extra arguments passed to FUN.}
  \item{value}{Used internally by the subset assignment method for scope.}
  \item{drop}{Used internally by the subset and element-selection methods for scope; defaults to TRUE.}
  \item{recursive}{Used internally by the c() method for scope; defaults to FALSE.}
  

}
\details{
 A data frame and one of its column names must be specified (\code{x}, \code{this}).  Defaults 
 are chosen so that if nothing else is specified, per-row scope will be all 
 rows with the same value of \code{this} as the row in question.  \code{FUN} will operate on
 \code{this} and \code{that}, within \code{scope}, which is all rows, by default.  All  row-specific values 
 of \code{this} within \code{scope} will be compared to the row-specific value of \code{that}. If comparison 
 evaluates to \code{TRUE}, the corresponding row name is retained in the return value.
 
 \code{this} is usually atomic.  If it contains more than one (valid) column name, 
 and if no other arguments are supplied, the scope object will be constructed by 
 using the columns as factors in a multi-way classification of rows (using \code{tapply}).
 Such usage is a shortcut for serial scoping (each iteration passing the scope object 
 from the previous iteration) where \code{FUN} is always "==" and \code{that} is 
 always the same as \code{this} (the defaults).
 
 \code{FUN} is typically a binary operator, but must accept extra arguments.  \code{scope()} 
 will pass any extra arguments supplied.  Also, \code{scope()} will pass the extra argument
 "row", which is a character vector of row names (taken from \code{x}) for the current comparison.

 For each element, \code{format.scope()} prints the name, the first and last member, and the length.
 \code{print.scope()} gives all the details (vector values, i.e. corresponding row names). 
}
\value{
  A list of vectors, one per row in \code{x}, of relevant row names in \code{x}. 
  \code{print.scope()} has been defined.
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

