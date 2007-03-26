"scoop" <-
function(x,this,scope=NULL, ...){
#check input
if (!is.data.frame(x)) stop("'x' is not a dataframe")
len <- dim(x)[[1]]
if (len < 1) stop("'x' has no rows")
if (!this %in% names(x)) stop("'this' is not a column in 'x'")
this <- as.vector(this)
if (is.null(scope)) return(x[[this]])
if (length(scope)==1 && is.character(scope) && scope %in% names(x)) scope <- x[[scope]]
if (length(scope)!= len) stop("'scope' and 'x' not same length")
#prepare
out <- vector("character",length=len)
#assign
for (i in 1:len){
rows <- length(scope[[i]])
if(rows != 1) out[[i]] <- NA
if(rows == 1) out[[i]] <- scope[[i]][[1]]
}
ref <- array(x[[this]],dimnames=list(row.names(x)))
ret<- (as.vector(ref[dimnames=out]))
class(ret) <- class(x[[this]])
return(ret)
}

