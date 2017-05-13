##---------------------------CapFloor-----------------------------------------------------##

#We want to write the function to cap as well as floor the observations in a column
capfloor<-function(indata,varlist,start=0.0005,end=0.9995,returnDF=FALSE){

  if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
  require(pacman,quietly = TRUE)
  p_load(data.table)
  
  setDT(indata)
	for( i in c(1:length(varlist))){
	  
		pts<-indata[,list(st=quantile(eval(parse(text=varlist[i])),start)
		                  ,end=quantile(eval(parse(text=varlist[i])),end)),]
		
		parsestring<-paste0("indata[,",varlist[i],":=ifelse(",varlist[i]
						          	,"<=",pts[,st],",",pts[,st],",ifelse(",varlist[i],">="
						          	,pts[,end],",",pts[,end],",",varlist[i],")),]")
		
	eval(parse(text=parsestring)) }
  
	if(returnDF){ 
		return(setDF(indata))
	}else {
		return(indata)
	}
}
