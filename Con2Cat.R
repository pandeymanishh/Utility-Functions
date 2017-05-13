##---------------------------Con2Cat--------------------------------------------------##			

#Function to discretize the continuous variable 
		
cont2cat<-function(indata,varlist,minquantsize=0.05,returnDF=FALSE){ 

  if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
  require(pacman,quietly = TRUE)
  p_load(data.table)
  
	#Check if these variables are numeric 
	setDT(indata)
	vartype<-indata[,sapply(.SD,class),.SDcols=varlist]
	
	contvar<-names(vartype[which(!(vartype %in% c('factor','character')))])
	
	brks=indata[,sapply(.SD,function(x){quantile(x,seq(0,1,minquantsize))}),.SDcols=contvar] 
	
	for(i in 1:length(contvar)){
		eval(parse(text=paste0("indata[,cut_",contvar[i],":=cut(",contvar[i]
							   ,",unique(brks[,i]),include.lowest=T),]")))
	} 

	if(returnDF){
		return(setDF(indata))
	}else {
		return(indata) 
	  } 
}
		
	