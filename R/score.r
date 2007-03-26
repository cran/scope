"score" <-
function (scope,...) 
{
out <- sapply(scope,"length")
names(out) <- names(scope)
return(out)
}
