"scope" <-
function(x,this,FUN='==',that=x[[this]],scope=NULL){
#check input
if (!is.data.frame(x)) stop("'x' is not a dataframe")
len <- dim(x)[[1]]
if (len < 1) stop("'x' has no rows")
if (!this %in% names(x)) stop("'this' is not a column in 'x'")
if (length(that)==1 && that %in% names(x)) that <- x[[that]]
if (length(that)==1) that <- rep(that, len)
if (length(that) != len) stop("'that' is not a column in 'x' and wrong length")
if (is.null(scope)) scope <- vector("list",len)
if (length(scope)!= len) stop("'scope' and 'x' not same length")
if (!is.element('scope',class(scope))) class(scope) <- c('scope',class(scope))
#prepare
FUN <- match.fun(FUN)
#assign
for (i in 1:len){
if (is.null(scope[[i]])) scope[[i]] <- row.names(x)
rows <- row.names(x)[FUN(x[[this]],that[i])]#range of 'this', but only one of 'that'
scope[[i]] <- intersect(scope[[i]], rows)
}
return(scope)
}

