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



















