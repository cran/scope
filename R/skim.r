"skim" <- function(x,this,FUN='max',scope=NULL,...){
	#check input
	null.scope <- is.null(scope)
	ok.scope <- !is.null(scope)
	if (ok.scope && length(scope)==1 && is.character(scope) && scope %in% names(x))scope <- x[[scope]]
	if (!is.data.frame(x)) stop("'x' is not a dataframe")
	if (nrow(x) < 1) stop("'x' has no rows")
	if (length(this) > 1) stop("'this' must be atomic")
	if (!this %in% names(x)) stop("'this' must be a column in 'x'")
	if (ok.scope && length(scope)!= nrow(x)) stop("'scope', if supplied, must have same length as nrow(x); or scope must name a column in x")
	if (ok.scope && row.names(x) != names(scope)) stop("row.names(x) not same as names(scope)")

	#prepare
	LEN <- nrow(x)
	NAMES <- as.character(row.names(x))
	FUN <- match.fun(FUN)
	LEFT <- as.vector(x[[this]])
	names(LEFT) <- NAMES

	#assign
	if(null.scope){
		names(LEFT) <- rep("all",length(LEFT))
		val <- FUN(LEFT,...)
		if (length(val) != 1) stop("FUN should return exactly one value")
		out <- rep(val,LEN)
		names(out) <- NAMES
	}
	if(!null.scope){
		scope.data <- unlist(scope)
		scope.form <- score(scope)
		left <- LEFT[scope.data]
		index<- rep(NAMES,scope.form)
		names(left) <- index
		out <- tapply(left, index, FUN,...)
		empty.groups <- setdiff(NAMES,names(out))
		empty.data <- rep(NA,length(empty.groups))
		names(empty.data) <- empty.groups
		out <- c(out,empty.data)
		out <- out[NAMES]		
	}
	class(out) <- class(x[[this]])
	return(out)
}