"score" <-
function(scope){
#check input
len <- length(scope)
if (len < 1) stop("'scope' has zero length")
#prepare
out <- vector('numeric', length=len)
#assign
for(i in 1:len) out[[i]] <- length(scope[[i]])
return(out)
}

