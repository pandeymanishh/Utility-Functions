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
