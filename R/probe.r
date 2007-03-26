"probe" <-
function(x,this,FUN='max',that=this,scope=NULL,...){
#prepare aggregate
skim <- skim(x,this,FUN,scope,...)
#try to probe cells for row with aggregate value 
scope <- scope(x,this,that=skim,scope=scope)
#test for selectivity of probe
if(any(score(scope) != 1)) warning("Some scopes not atomic")
#assign
return(scoop(x,that,scope))
}

