"scope" <- function(x,this,FUN=NULL,that=NULL,scope=NULL,...){
	#check input
	ok.frame 	<- is.data.frame(x)
	ok.this 	<- all(this %in% names(x))
	ok.fun 		<- !is.null(FUN)
	ok.that 	<- !is.null(that)
	ok.scope 	<- !is.null(scope)
	one.this	<- length(this)==1
	long.frame 	<- ok.frame && nrow(x) > 1
	one.that	<- ok.that && length(that)==1
	nrow.that	<- ok.that && ok.frame && length(that)==nrow(x) 
	known.that	<- one.that && that %in% names(x)
	null.fun	<- is.null(FUN)
	null.that	<- is.null(that)
	null.scope	<- is.null(scope)
	nrow.scope	<- ok.scope && length(scope)==nrow(x)
	local.scope	<- ok.scope && length(scope)==1 && is.character(scope) && scope %in% names(x)

	#issue errors
	if (!ok.frame) 	stop("'x' is not a dataframe")
	if (!long.frame) 	stop("'x' has less than 2 rows")
	if (!ok.this) 	stop("'this' must be column(s) in 'x'")
	if (!one.this && (ok.fun||ok.that||ok.scope) ) 	stop("'this' must be atomic if 'FUN','that',or 'scope' is supplied")
	if (ok.that && !(one.that||nrow.that) ) stop("length of 'that', if supplied, must equal 1 or nrow(x)")
	if (ok.scope && !nrow.scope && !local.scope) stop("length of 'scope', if supplied, must equal nrow(x); or 'scope' must name a column in x")

	#set values
	LEN <-nrow(x)
	NAMES <- as.character(row.names(x))
	if (one.this) 			LEFT <- as.vector(x[[this]])
	if (!one.this)			LEFT <- as.vector(x[[this[[1]]]])#not used
	if (known.that) 			RIGHT <- as.vector(x[[that]])
	if (!known.that && one.that) 	RIGHT <- rep(as.vector(that), LEN)
	if (!known.that && nrow.that) RIGHT <- as.vector(that)
	if (null.that) 			RIGHT <- LEFT 
	names(LEFT) <- NAMES
	names(RIGHT)<- NAMES
	if(null.fun) fun <- match.fun("==")
	if(ok.fun) fun <- match.fun(FUN)

	#cases
	if(null.scope && null.that && null.fun){
		selector <- list()
		for (i in this) selector[[i]] <- as.factor(x[[i]])
		selector.matrix <- unlist(selector)
		dim(selector.matrix) <- dim(as.data.frame(selector))
		new.scope <- tapply(NAMES,selector,as.vector)[selector.matrix]
		names(new.scope) <- NAMES
	}
	if(null.scope && (ok.that||ok.fun)){
		new.scope <- list()
		for (i in 1:length(RIGHT)){
			group <- names(RIGHT)[[i]]
			comparison <- fun(LEFT,RIGHT[[i]],row=NAMES[i],...)
			surviving.members <-names(LEFT)[comparison]
			new.scope[[group]] <- surviving.members
		}
	}
	if(ok.scope){
		if(local.scope) scope <- x[[scope]]
		scope.data <- unlist(scope)
		scope.form <- score(scope)
		left <- LEFT[scope.data]
		right <- rep(RIGHT,scope.form)
		row <- rep(NAMES,scope.form)
		#calculate
		comparison <- fun(left,right,row=row,...)
		surviving.groups <- names(right)[comparison]
		surviving.members <-names(left)[comparison]
		empty.groups <- setdiff(NAMES,surviving.groups)
		#reconstitute scope
		new.scope <- tapply(surviving.members,surviving.groups,as.vector)
		#append empties
		empty.scope <- rep(list(vector("character")),length(empty.groups))
		names(empty.scope) <- empty.groups
		new.scope <- c(new.scope, empty.scope)
		#sort as original
		new.scope <- as.list(new.scope[NAMES])
	}
	class(new.scope) <- c("scope","list")
	return(new.scope)
}
"print.scope" <- function(x,...)print.default(x,quote=FALSE)
"format.scope" <- function(x,...){
    nms <- names(x)
    num <- score(x)
    first <- function(vec)ifelse(length(vec)>0,vec[1],"")
    last  <- function(vec)ifelse(length(vec)>1,vec[length(vec)],"")
    firsts <- sapply(x,"first")
    lasts <- sapply(x,"last")
    width <- function(x)max(nchar(as.character(x)),na.rm=TRUE)
    sprintf("%*s: %*s..%*s (%*i)",width(nms),nms,width(firsts),firsts,width(lasts),lasts,width(num),num)
}
#This function exists solely so that scope's class will not be lost when assigning to a subset.
#Common problem with tinkering with dataframes.
"[<-.scope" <- function(x,...,value){
	if (!as.logical(length(value))) 
        return(x)
    value <- as.scope(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}

#This function exists so that scopes look like scopes (class not lost) when accessed by subsetting.
"[.scope" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

#This function exists so that scopes look like scopes (class not lost) when accessed by element selection.
"[[.scope" <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

#This function exists so that when scopes are combined in a vector, their class is not lost.
c.scope <- function (..., recursive = FALSE) 
structure(c(unlist(lapply(list(...), unclass))), class = "scope")


#This function is a SQL-like wrapper for probe().
select <- function(x,from,among=NULL,where,is,...)
 probe(x=from, that=x, this=where, FUN=is, scope=among,...) 






