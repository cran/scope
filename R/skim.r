"skim" <-
function(x,this,FUN='max',scope=NULL,...){
#check input
if (!is.data.frame(x)) stop("'x' is not a dataframe")
len <- dim(x)[[1]]
if (len < 1) stop("'x' has no rows")
if (!this %in% names(x)) stop("'this' is not a column in 'x'")
if (is.null(scope)) scope <- vector("list",len)
if (length(scope)!= len) stop("'scope' and 'x' not same length")
#prepare
FUN <- match.fun(FUN)
out <- vector("numeric",length=len)
#assign
for (i in 1:len){
if (is.null(scope[[i]])) scope[[i]] <- row.names(x)
out[[i]] <- FUN(x[[this]][row.names(x) %in% scope[[i]] ],...)
}
return(out)
}

