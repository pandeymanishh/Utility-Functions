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

##---------------------------Object Size--------------------------------------------------##
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,decreasing=FALSE, head=FALSE, n=5) {
	napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos))) 
	names <- ls(pos = pos, pattern = pattern)
	obj.class <- napply(names, function(x) as.character(class(x))[1])
	obj.mode <- napply(names, mode) 
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class) 
	obj.prettysize <- napply(names, function(x) {capture.output(format(utils::object.size(x), units = "auto")) }) 
	obj.size <- napply(names, object.size)
	obj.dim <- t(napply(names, function(x)as.numeric(dim(x))[1:2])) 
	vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
	obj.dim[vec, 1] <- napply(names, length)[vec] 
	out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
	names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
	if (!missing(order.by)) out <- out[order(out[[order.by]], decreasing=decreasing), ]
	if (head) out <- head(out, n) 
	out
	}
			
						
# shorthand
lsos <- function(..., n=10) { .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)}

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
			
##---------------------------Merge Categories--------------------------------------------------##			
			
#We want to write a function where we can combine categories #Only for low contributors
			
mergecat <- function(input,changedt=NULL,catcol,target,capsize=0.0005,minsize,maxsize,returnDF=FALSE) {
  
  if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
  require(pacman,quietly = TRUE)
  p_load(data.table,fastmatch)
  
	#create table
  
	in.tb<-input[,list(cnt=.N,pt5=quantile(eval(parse(text=target)),0.05)
					   ,pt10=quantile(eval(parse(text=target)),0.10)
					   ,pt20=quantile(eval(parse(text=target)),0.20)
					   ,pt30=quantile(eval(parse(text=target)),0.3)
					   ,pt40=quantile(eval(parse(text=target)),0.4)
					   ,pt50=quantile(eval(parse(text=target)),0.5)
					   ,pt60=quantile(eval(parse(text=target)),0.6)
					   ,pt70=quantile(eval(parse(text=target)),0.7)
					   ,pt80=quantile(eval(parse(text=target)),0.8)
					   ,pt90=quantile(eval(parse(text=target)),0.9)
					   ,pt95=quantile(eval(parse(text=target)),0.95))
				 ,by=catcol]
	
	#Cap at 1% if the cnt is high
	in.tb[,cnt.cap:=ifelse(cnt>nrow(input)*capsize,round(nrow(input)*capsize,0),cnt),]
	
	#Replicte the data
	rep.dt<-in.tb[rep(seq(nrow(in.tb)),cnt.cap),!"cnt.cap",with=F]
	
	#Do the kmeans in a loop
	
	for(i in c(minsize:maxsize)){
	  
		clus<-kmeans(x =rep.dt[,-c(1,2),with=F] ,centers =i ,iter.max = 1000)
		
		ddt<-data.frame(rep.dt[,1,with=FALSE],"clus"=clus$cluster) 
		
		ddt.1<-merge(ddt,in.tb[,c(catcol,"cnt"),with=FALSE],by=catcol)
		
		tb.1<-table(ddt.1$clus)
		tb.2<-data.frame("clus"=i,size=min(tb.1))
		
		if(i==minsize){
			minclus<-tb.2
		}else{
			minclus<-rbind(minclus,tb.2)
		}
		print(paste0(i,"done:",maxsize-i," to go"))
	}
	
	print(minclus)
	
	cl<-as.integer(readline(prompt = "Enter the desired # of clusters")) 
	
	#do a 25 cluster size
	
	clus<-kmeans(x =rep.dt[,-c(1,2),with=F] ,centers =cl ,iter.max = 1000)
	ddt<-unique(data.frame("cat"=rep.dt[,1,with=FALSE],"clus"=clus$cluster)) 
	
	if(nrow(in.tb)!=nrow(ddt)) print("issue in clustering non matching categories")
		ddt.1<-merge(ddt,in.tb[,c(catcol,"cnt"),with=FALSE],by=catcol)
		setDT(ddt.1)
		clus.dist<-ddt.1[,list(sm=sum(cnt)),by="clus"]
		#Mow merge back to the original table 
		setDT(changedt)
		input.1<-merge(input,ddt.1[,c(catcol,"clus"),with=FALSE],by=catcol,all.x=T)
		changedt.1<-merge(changedt,ddt.1[,c(catcol,"clus"),with=FALSE],by=catcol,all.x=T)
		#Replace missing values 
		input.1[is.na(clus),clus:=catcol,with=FALSE]
		changedt.1[is.na(clus),clus:=catcol,with=FALSE]
		in.tb.1<-input.1[,list(cnt=.N,
							   pt5=quantile(eval(parse(text=target)),0.05)
							   ,pt10=quantile(eval(parse(text=target)),0.10)
							   ,pt20=quantile(eval(parse(text=target)),0.20)
							   ,pt30=quantile(eval(parse(text=target)),0.3)
							   ,pt40=quantile(eval(parse(text=target)),0.4)
							   ,pt50=quantile(eval(parse(text=target)),0.5)
							   ,pt60=quantile(eval(parse(text=target)),0.6)
							   ,pt70=quantile(eval(parse(text=target)),0.7)
							   ,pt80=quantile(eval(parse(text=target)),0.8)
							   ,pt90=quantile(eval(parse(text=target)),0.9)
							   ,pt95=quantile(eval(parse(text=target)),0.95))
						 ,by=clus]
		#Whether to replace or not  
		if(!is.null(changedt)){
			input.1[,clus:=as.factor(clus),] 
			input.1<-input.1[,-1,with=FALSE]
			#rename that column
			setnames(x =input.1 ,old ="clus" ,new = catcol) 
			changedt.1[,clus:=as.factor(clus),]
			changedt.1<-changedt.1[,-1,with=FALSE]
			#rename that column
			setnames(x =changedt.1 ,old ="clus" ,new = catcol)
			lst<-list("train"=input.1,"test"=changedt.1,"clus.summary"=minclus 
					  ,"final.clus"=ddt.1,"new.clus.desc"=in.tb.1)
			return(lst)
		}else{
			lst<-list("clus.summary"=minclus,"final.clus"=ddt.1,"new.clus.desc"=in.tb.1) 
		}
		}
	
##---------------------------Check Reg Model--------------------------------------------------##			
	
#We want to write a function which for a regression setup can tell us where the model is failing
#Input a target and the prediction
CheckRegModel <- function(target,pred) { 

  if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
  require(pacman,quietly = TRUE)
  p_load(data.table,reshape,hydroGOF,ggplot2)
  
  if(length(target)!=length(pred)) stop("Length is different for target and predictions")
	#Create the bins
		bsdt<-data.frame("target"=target,"prediction"=pred)
		
		setDT(bsdt) 
		bin<-c(seq(0,0.95,0.05),0.975,0.99,1)
		
		bsdt[,':='(tgt=as.factor(cut(target,breaks =quantile(target,bin),include.lowest =TRUE ))
				   ,error=target-pred) ,]
		levels(bsdt$tgt)<-bin[-1] #Compute the summary and mae now
		bs.summ<-bsdt[,list(cnt=.N
							,avgtgt=round(mean(target),2) 
							,mae=round(mean(abs(target-prediction)),2)
							,rmse=round(rmse(target,prediction),2))
					  ,by=tgt]
		bs.summ[,':='(sc.mae=round(mae/avgtgt,2),sc.rmse=round(rmse/avgtgt,2)),] 
		
		setorder(bs.summ,tgt)
		
		#We also want box plots of the error by the tgt quantile 
		#We want to create breaks dynamically in multiple of 10,100 and so on  
		mx<-max(bsdt$error)
		mn<-min(bsdt$error)
		#Create cut seq
		if(mn<0 & mx>0){
			lw<-round(seq(mn,0,abs(mn/6)),0) 
			hg<-round(seq(0,mx,abs(mx/6)),0) 
		}else if(mn>=0 & mx>=0){
			lw<-0
			hg<-round(seq(0,mx,abs(mx,12)),0) 
		}else if(mn<0 & mx<=0){
			lw<-round(seq(mn,0,abs(mx/12)),0) 
			hg<-0
		}
		rng<-unique(c(lw,hg))
 		p <- ggplot(bsdt,aes(x = tgt, y = error)) +
		     geom_boxplot(fill="orange",colour="blue")+
		     scale_y_continuous(breaks=rng)+
			   theme_bw()
 		
	  #Also create the density plot 
	   mt<-melt(data=bsdt[,c("target","prediction"),with=FALSE],measure.vars=c("target","prediction"))
	   
	   xrng<-seq(min(mt$value),max(mt$value),(max(mt$value)-min(mt$value))/10) 
	   xrng<-round(unique(xrng),0)
	   
	   p.dense<-ggplot(mt,aes(x=value,fill=variable))+
			geom_density(alpha=0.2)+
			scale_x_continuous(breaks=xrng)+
			theme_bw()
	   
	  return(list("Summary"=bs.summ,"boxplot"=p,"density"=p.dense))
	}

##---------------------------Word Cloud--------------------------------------------------##			
			
#Quick and dirty word cloud with flexibility of slic and diced word clouds
			
word.cloud.grp<-function(filter.var,filter.val,text.col,dt1,minfreq=10,maxword=400,stp1=NULL,dtm_par){
	
  if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
  require(pacman,quietly = TRUE)
  p_load(data.table,reshape,hydroGOF,ggplot2,tm,SnowballC,wordcloud,RColorBrewer)
  

	if(!is.na(filter.var) & !is.na(filter.val)){
		eval(parse(text=paste0(paste0("tmp1<-",dt1,"[",filter.var,"=="
		                              ,paste0('"',filter.val,'"' ),",")
		                       ,paste0('"',text.col,'"' ),",with=FALSE]"))) 
	}else{
		eval(parse(text=paste0(paste0("tmp1<-",dt1,"[" ,",")
		                       ,paste0('"',text.col,'"' ),",with=FALSE]")))
	}
  
	eval(parse(text=paste0("tmp2<-Corpus(VectorSource(tmp1$",text.col,"))"))) 
	#Treat the text first
	
	toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
	tmp2 <- tm_map(tmp2, toSpace, "/")
	tmp2 <- tm_map(tmp2, toSpace, "@")
	tmp2 <- tm_map(tmp2, toSpace, "\\|") 
	# Convert the text to lower case
	tmp2 <- tm_map(tmp2, content_transformer(tolower)) 
	# Remove numbers 
	tmp2 <- tm_map(tmp2, removeNumbers) 
	# Remove english common stopwords
	tmp2 <- tm_map(tmp2, removeWords, stopwords("english"))
	# Remove your own stop word
	# specify your stopwords as a character vector
	tmp2 <- tm_map(tmp2, removeWords,stp1)  
	# Remove punctuations
	tmp2 <- tm_map(tmp2, removePunctuation) 
	# Eliminate extra white spaces 
	
	tmp2 <- tm_map(tmp2, stripWhitespace) 
	dtm <- TermDocumentMatrix(tmp2,control = list(global = c(dtm_par, Inf))) 
	dtm.1<-removeSparseTerms(dtm,0.98) 
	m <- as.matrix(dtm.1)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
  set.seed(1234)

	wordcloud(words = d$word, freq = d$freq, min.freq = minfreq
			  ,max.words=maxword, random.order=FALSE, rot.per=0.35
			  ,colors=brewer.pal(8, "Dark2"))
		
	return(d)
	}

##---------------------------Data Dictionary Function--------------------------------------------------##			
								   
								   
data.dict<-function(x){

if(!require(pacman)){ install.packages("pacman",dep=T,quiet=TRUE) }
require(pacman,quietly = TRUE)
p_load(data.table,e1071)

mean1<-function(x){
  if(is.numeric(x)==TRUE){
    m<-mean(x,na.rm=TRUE)
  }else{
      m<-""}
  return(m)}

max1<-function(x){
  if(is.numeric(x)==TRUE){
    m<-max(x,na.rm=TRUE)
  }else{m<-max(as.character(x),na.rm=TRUE)
  }
  return(m)}

min1<-function(x){
  if(is.numeric(x)==TRUE){
    m<-min(x,na.rm=TRUE)
  }else{m<-min(as.character(x),na.rm=TRUE)
  }
  return(m)}

p10<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.10,na.rm=TRUE)}else{m<-""}
  return(m)}

p25<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.25,na.rm=TRUE)}else{m<-""}
  return(m)}

p50<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.5,na.rm=TRUE)}else{m<-""}
  return(m)
}

p75<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.75,na.rm=TRUE)}else{m<-""}
  return(m)
}

p90<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.9,na.rm=TRUE)}else{m<-""}
  return(m)
}

p95<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.95,na.rm=TRUE)}else{m<-""}
  return(m)
}

p99<-function(x){
  if(is.numeric(x)==TRUE){m<-quantile(x,0.99,na.rm=TRUE)}else{m<-""}
  return(m)
}

sum1<-function(x){
  if(is.numeric(x)==TRUE){m<-sum(as.numeric(x),na.rm=TRUE)}else{m<-""}
  return(m)
}

skew1<-function(x){
  if(is.numeric(x)==TRUE){m<-skewness(x,na.rm=TRUE)}else{m<-""}
  return(m)
}

kurt1<-function(x){
  if(is.numeric(x)==TRUE){m<-kurtosis(x,na.rm=TRUE)}else{m<-""}
  return(m)
  }

var1<-function(x){
  if(is.numeric(x)==TRUE){m<-var(x,na.rm=TRUE)}else{m<-""}
  return(m)
}

#Write a function for mode and mode count (based on frequency table)
freq.mode<-function(x){
  x<-tail(sort(table(x)),2)
  md1.nm<-row.names(x)[1]
  md2.nm<-row.names(x)[2]
  x<-as.vector(x)
  md1<-x[1]
  md2<-x[2] 
  if(x[1]==x[2]){
    multi="Yes"
  } else { multi="No" }
  return(list(ModeValue1=md1.nm,ModeValue1.cnt=md1,ModeValue2=md2.nm,ModeValue2.cnt=md2))
}

multi.mode<-function(x){ 
  z<-tail(sort(table(x)),2) 
  if(z[1]==z[2] & length(z)>1){multi="Yes" 
  }  else {
    multi="No" }
  return(multi)
}

mode.1<-function(x){
  z<-tail(sort(table(x)),1)
  md1.nm<-row.names(as.data.frame(z))
  return(md1.nm)
}

mode.1.cnt<-function(x){
  z<-tail(sort(table(x)),1)
  z<-as.vector(z)
  md1<-z[1]
  return(md1)
}

#Count the special characters and the tabulation of the same
count.spl<-function(x,y=FALSE){
  spl.1<-c(""," ",",",".","'",'"',"*","\\","+","-","=","/","|","?","*"
           ,"&","!","#","@","~","%","^","(",")","{","}","[","]","<",">",":",";")
  x.1<-as.character(x)
  z<-length(x.1[x.1 %in% spl.1])
  z.1<-table(x.1[x.1 %in% spl.1])
  z.2<-sort(z,decreasing = TRUE)
  if(y==TRUE){
    return(list(Count_Spl=z,Spl_Table=z.1))
  } else {
      return(z)
  }}


setDT(x)
#Identify the completely empty columns and send out a message
mis<-nrow(x)==as.data.table(is.na(x))[,sapply(.SD,function(x){sum(as.numeric(x))}),]

y<-x[,names(x)[!mis],with=FALSE]
dp<-x[,names(x)[mis],with=FALSE]
print(paste0("There are "
             ,sum(mis)
             ," columns in this dataset with missing entries,these columns are saved in the output list object"))

#.following columns are dropped from the analyis - ",names(dp)))

#Get the variable format
var.format<-y[,sapply(.SD,class),]

if(!is.null(nrow(var.format))) {
  var.format1<-unlist(var.format[1,])
} else { 
    var.format1<-var.format
    }

    #Return the unique counts
    unq.cnt<-y[,sapply(.SD,FUN =function(x){length(unique(x))}),]
    #Get the total row counts only
    row.cnt<-rep(nrow(y),times = ncol(y))
    #Return the ordinal position of the columns in the table
    ord.1<-c(1:ncol(y))
    names(ord.1)<-colnames(y)
    #Get the non missing count
    valid.cnt<-as.data.table(!is.na(y))[,sapply(.SD,function(x){sum(as.numeric(x))}),]
    #Get the missing count
    msng.cnt<-as.data.table(is.na(y))[,sapply(.SD,function(x){sum(as.numeric(x))}),]
    #Count the special characters
    cnt.spl<-y[,sapply(.SD,count.spl),]
    #Get the numerical summaries
    mean.1<-y[,sapply(.SD,mean1),]
    multi.md<-y[,sapply(.SD,multi.mode),]
    md.1<-y[,sapply(.SD,mode.1),]
    md.1.cnt<-y[,sapply(.SD,mode.1.cnt),]
    mn1<-y[,sapply(.SD,min1),]
    pp10<-y[,sapply(.SD,p10),]
    pp25<-y[,sapply(.SD,p25),]
    pp50<-y[,sapply(.SD,p50),]
    pp75<-y[,sapply(.SD,p75),]
    pp90<-y[,sapply(.SD,p90),]
    pp95<-y[,sapply(.SD,p95),]
    pp99<-y[,sapply(.SD,p99),]
    mx<-y[,sapply(.SD,max),]
    sm1<-y[,sapply(.SD,sum1),]
    vr1<-y[,sapply(.SD,var1),]
    sk1<-y[,sapply(.SD,skew1),]
    krt1<-y[,sapply(.SD,kurt1),]
    var.name<-names(mx)
    out.1<-as.data.frame(cbind("Variable_Name"=var.name
                               ,"Variable_Type"=var.format1
                               ,"Ordinal_Position"=ord.1
                               ,"Unique_Count"=unq.cnt))
    
    out.1$Total_Rows<-rep(nrow(y),times=ncol(y))
    
    out.2<-as.data.frame(cbind(out.1,"Valid_Rows"=valid.cnt
                               ,"Missing_Count"=msng.cnt
                               ,"Special_Count"=cnt.spl
                               ,"Sum"=sm1
                               ,"Mean"=mean.1
                               ,"Median"=pp50
                               ,"Multiple_Modes"=multi.md
                               ,"Modal_Value"=md.1
                               ,"Mode_Count"=md.1.cnt
                               ,"Variance"=vr1
                               ,"Skewness"=sk1
                               ,"Kurtosis"=krt1
                               ,"Minimum"=mn1
                               ,"P10"=pp10
                               ,"P25"=pp25
                               ,"P75"=pp75
                               ,"P90"=pp90
                               ,"P95"=pp95
                               ,"P99"=pp99
                               ,"Maximum"=mx))
    
    out.2<-with(out.2,out.2[order(Variable_Type,ord.1),])
    
    return(list(dict=out.2,dropvar=names(dp)))
}



















