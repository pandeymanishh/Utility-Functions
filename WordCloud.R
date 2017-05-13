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
