"probe" <-
function(x,this,FUN='max',that=this,scope=NULL, ...){
#check input
if (!is.data.frame(x)) stop("'x' is not a dataframe")
len <- dim(x)[[1]]
if (len < 1) stop("'x' has no rows")
if (!this %in% names(x)) stop("'this' is not a column in 'x'")
if (!that %in% names(x)) stop("'that' is not a column in 'x'")
if (is.null(scope)) scope <- vector("list",len)
if (length(scope)!= len) stop("'scope' and 'x' not same length")
#prepare aggregate
skim <- skim(x,this,FUN,scope,...)
#try to probe cells for row with aggregate value 
scope <- scope(x,this,that=skim,scope=scope)
#test for selectivity of probe
if(any(score(scope) != 1)) warning("Some scopes not atomic")
#assign
return(scoop(x,that,scope))
}

