\name{scope-package}
\alias{scope-package}
\docType{package}
\title{
 Data Manipulation Using Arbitrary Row and Column Criteria
}
\description{
 Calculate, per data frame row, a value that depends on information in a 
 relevant subset of rows and columns.  These functions create and refine
 scope objects, which identify relevant rows on a per-row basis. Columns 
 can be aggregated within relevant scopes to aid identification of a row 
 of interest, from which the value in an arbitrary column can be selected.
}
\details{
\tabular{ll}{
Package: \tab scope\cr
Type: \tab Package\cr
Version: \tab 2.0-2\cr
Date: \tab 2006-10-25\cr
License: \tab GPL version 2 or newer\cr
}
 Given a data frame of length n, \code{scope()} creates a list of length n, each element of 
 which is a vector of length n or smaller (a scope object).  The vectors hold data frame row names 
 that are selected by some criterion specified as an argument to \code{scope()}.  Scope 
 objects may be sequentially refined by passing to \code{scope()} with additional 
 criteria.  For a given scope object, \code{score()} returns a vector of the lengths of 
 each element.  \code{scoop()} effectively dereferences the scope object, returning a 
 vector of values of a specified column for scope elements of length one (1).  
 \code{skim()} is like \code{tapply()}, applying an aggregate function to cells specified 
 by scope elements.  Some aggregate functions are likely to return values that 
 occur once among their arguments (often \code{min()} and \code{max()}, but probably not \code{mean()}); 
 passing such functions to \code{skim()} creates a vector that can be used with \code{scope()} 
 to identify the matching row.  \code{probe()} is a short-cut that uses \code{skim()}, 
 \code{scope()}, and \code{scoop()} to find the value in an arbitrary column where the value in 
 a related column matches the aggregated value.
 
 Similar results can be achieved with code blocks that \code{merge()}, \code{tapply()}, 
 \code{aggregate.data.frame()}, etc.  The \code{scope()} package is an alternative that 
 has three advantages: a given manipulation can be expressed in fewer lines of code; 
 the row and column orders of the source data frame are untouched (cs. 
 \code{merge()}); and the source-sink mapping is explicit (via the \code{scope} object).
 
 Changes for version 2.  \code{scope()}, \code{skim()}, and \code{score} have been vectorized 
 internally for greater speed.  All main functions now accept extra (...) arguments. 
 The \code{scope} object itself now inherits class "scope", and its elements have 
 the same names as the row names of the source data frame.  A \code{print} function has 
 been added for scope objects.  \code{scope()} may now be invoked with \code{this} 
 having length greater than 1 (if no other arguments are supplied) as a shortcut 
 for serial "scoping" using the function "==" (similar to \code{tapply()} with 
 multiple indices).
}
\author{
Tim Bergsma

Maintainer: Tim Bergsma <timb@metrumrg.com>
}
\keyword{manip}
\seealso{
 \code{\link{tapply}}
}
\examples{
data(Theoph)
#What is the time since the maximum concentration within subject?
Scope <- scope(Theoph,'Subject')
Tmax  <- probe(Theoph,'conc',that='Time',scope=Scope)
Theoph$SinceMax <- Theoph$Time - Tmax
}
