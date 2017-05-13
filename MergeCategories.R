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
	