##---------------------------Frequency Distribution--------------------------------------------------##			
		
freq.dist<-function(input,varlist,target_cat=NULL,target_cont=NULL,per_type="row",num.unique=10){
	
  if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
  require(pacman,quietly = TRUE)
  p_load(data.table,fastmatch)

	setDT(input)
	
	#Take the input variables and get the variable types
	
	varclass<-as.data.table(t(input[,lapply(.SD,class),.SDcol=varlist,with=TRUE])
	                        ,keep.rownames = T)
	
	setnames(x =varclass ,old =names(varclass) ,new =c("column","type") )
	
	date.vars<-varclass[type=='Date',1,with=FALSE][[1]]
	
	char.vars<-varclass[type %in% c('factor','character'),1,with=FALSE][[1]]
	
	num.vars<-varclass[!(column %in% c(char.vars,date.vars)),1,with=FALSE][[1]]
	
	#For the numeric variables if the unique categories are less than this number
	#treat it as factor and convert here as well
	
	num.unique.cnt<-sapply(input[,.SD,.SDcols=num.vars],function(x){length(unique(x))})
	
	num.2.cat<-names(num.unique.cnt)[which(num.unique.cnt<=num.unique)]
	
	num.vars.1<-num.vars[!(num.vars %in% num.2.cat)]
	
	#Cut the rest of numerical variables
	
	if(length(num.vars.1)>0){ 
		
	  for(j in c(1:length(num.vars.1))){
		
	    col2cut<-eval(parse(text=paste(num.vars.1[j])),input)
		
	    cutcol<-cut(x =col2cut
	                ,unique(quantile(x =col2cut
	                                 ,probs =c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,1) 
	                                 ,na.rm=TRUE))
	                ,include.lowest =TRUE )
	    
		input<-cbind(input,cutcol)
		
		names(input)[ncol(input)]<-paste0("Cut_",num.vars.1[j])
		}
	}
	
	#Change the numeric to cat for few 
	
	for(col in num.2.cat) set(input,j=match(col,names(input)),value=as.factor(input[[col]]))
	
	#Cont to Cat vars
	Cont2Cat.vars<-names(input)[names(input) %like% "Cut_"]
	
	#We also want to exclude any categorical variable with more than 2000 categories
		
	varlist<-c(char.vars,num.2.cat,Cont2Cat.vars)
		
	#Get the tables together
	freq.table<-lapply(input[,varlist,with=FALSE],function(x){table(x,useNA = "always")})
		
	#Get the Individual table for first var in the loop and tthen do a cbind
	
	for(i in c(1:length(varlist))){ 
		print(paste0("Executing var:",i,"-",length(varlist)-i," Pending")) 
		
	  baset<-as.data.frame(freq.table[[i]])
		
	  names(baset)<-c("Categories","Frequency")
		
	  baset$Percent<-round(baset$Frequency/sum(baset$Frequency),4) 
		
	  baset$Variable<-names(freq.table)[i] 
		
	  baset$order<-1:nrow(baset) 
		#base table prepared 
		#Add the additional information for continuous target
		
	  b <- parse(text = varlist[i]) 
		if(length(target_cont)>0){ 
		
		  	target_cont<-sort(target_cont)
			
		  	for(col in target_cont) set(input,j = col,value=as.numeric(input[[col]]))
				
		  	prf1<-input[,c(varlist[i],target_cont)
		  	            ,with=FALSE][,sapply(.SD,function(x){list(mean=mean(x,na.rm=TRUE)
																	  ,md=median(x,na.rm=TRUE))})
																	  ,by=eval(b)]
		  	
				prf1.nm<-c("Categories",paste(target_cont,"mean",sep="_")
				           ,paste(target_cont,"median",sep="_")) 
				
				prf2.nm<-c("Categories",sort(prf1.nm[-1]))
				
				setnames(x =prf1 ,old=names(prf1),new =prf2.nm )
				
				baset<-merge(x =baset ,y =prf1 ,by = "Categories")
				
				baset<-baset[,c(4,5,1,2,3,6:ncol(baset))]
				} else {
				baset<-baset[,c(4,5,1,2,3)]
			          }
	
	  	#Now look at the categorical variables 
		if(length(target_cat)>0){
		  
			for(j in 1:length(target_cat)){
				#Get the base tablulation
			  
				cat_dist1<-input[,.N,by=eval(paste0(varlist[i],",",target_cat[j]))]
			
				setnames(cat_dist1,names(cat_dist1),c("Categories","Target","Count"))
				
				cat_dist<-data.table:::dcast.data.table(data =cat_dist1 
														,formula = as.formula("Categories~Target")
														,value.var ="Count",fun.aggregate = sum)
				
				cat.nm<-paste(target_cat[j],names(cat_dist)[-1],sep="_")
				
				setnames(cat_dist,old=names(cat_dist),new=c("Categories",cat.nm))
				
				#get rowsum
				cat_dist[,"Frequency":=rowSums(.SD),.SDcol=2:ncol(cat_dist)]
				
				cat_dist.nm<-names(cat_dist)[1:length(cat_dist)-1]
				
				if(sum(per_type=="row")){
					#Get the row percents
					cat.per<-cat_dist[,(.SD),.SDcol=2:(ncol(cat_dist)-1)]/cat_dist$Frequency
					
					setnames(cat.per,old=names(cat.per),new=paste0("Ro.Pr.",names(cat.per)))
					
					cat_dist.comp<-cbind(cat_dist[,.SD,.SDcols=-c("Frequency")],cat.per)
					
					baset<-merge(baset,cat_dist.comp,by="Categories")
					
					baset<-baset[,c(2,1,3:ncol(baset))]
					
					}else if(sum(per_type=="col")){
					  
						#Get the column sums stored in a object
					  
						cat.sm<-rep(cat_dist[,colSums(.SD)
						                     ,.SDcol=2:(ncol(cat_dist)-1),]
						            ,each=nrow(cat_dist))
						
						#Get the column percents
						cat.per<-round(cat_dist[,.SD,.SDcols=2:(ncol(cat_dist)-1),]/cat.sm,4)
						
						cat.per.nm<-paste0("Col.Pr.",names(cat.per))
						
						setnames(cat.per,old=names(cat.per),new=cat.per.nm)
						
						cat_dist.comp<-cbind(cat_dist,cat.per)
						
						nm<-setdiff(names(cat_dist.comp),names(baset))
						
						baset<-merge(baset,cat_dist.comp[,c("Categories",nm)
						                                 ,with=FALSE],by="Categories")
						
						baset<-baset[,c(2,1,3:ncol(baset))]
						#Sort the var by order var
						setorder(baset,order)
					}
				}
				}
			if(i==1) { 
					freq.out<-baset 
				}else freq.out<-rbind(freq.out,baset)
	}
return(freq.out)
}
