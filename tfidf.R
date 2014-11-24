df<- read.csv('data/tw1.csv', stringsAsFactors = FALSE)
df <- rename(df, Advertiser = advertisers_company_name)

df1 <- ddply(df, ~Advertiser+api_text,summarise,
             Impressions = sum(impressions),
             Clicks = sum(clicks))
df1$CTR <- (df1$Clicks/df1$Impressions)*100

tfidf <- function(nt,ft,ndocs,ncorpus){
  return ((ft/nt)*log(ncorpus/ndocs))
}

#Total number of documents(tweets)
ncorpus <- length(df1$api_text)

for (i in 1:nrow(df1)) {
  
  #Number of terms in the document(Each tweet: separate document)
  nt<- sapply(strsplit(df1$api_text[i]," ", fixed = FALSE), length)
  #write to the file
  #write(df1$Advertiser[i], file = "data/Result.docx", append = TRUE)
  #write(df1$api_text[i], file ="data/Result.docx", append = TRUE)
  #write(df1$CTR[i], file = "data/Result.docx", append = TRUE)
  df1$Advertiser[i]
  df1$api_text[i]
  df1$CTR[i]
  
  #Split the tweet.
  str <- (strsplit(df1$api_text[i]," ", fixed = FALSE))[[1]]
  
  for(j in 1:nt){
    
    
    #Check if the term is "a,the,and, in"
    common.terms <- c("a", "and", "the")
    if(str[j] %in% common.terms)
    {
      j <- j+1
    }
    #Term Frequency
    ft <- length(grep(str[j], df1$api_text[i], ignore.case = TRUE))
    
    #Number of documents with term in it
    ndocs <- 1
    for(k in 1:nrow(df1)){
      if(df1$Advertiser[k] == df1$Advertiser[i]){
        if(k == i){
          k <- k+1
        }
        if(str[j] %in% df1$api_text[k]){
          ndocs <- ndocs+1
        }
      }
    }
    
    
    #TF_IDF for the term
    tf.idf <- tfidf(nt,ft,ndocs,ncorpus)
    #write(str[j], file ="data/Result.docx", append = TRUE)
    #write(tf.idf, file ="data/Result.docx", append = TRUE)
    
    tf <- data.frame(df1$Advertiser[i],df1$api_text[i],df1$CTR[i], str[j],tf.idf)
    write.table(tf,file="data/Result.csv", col.names = FALSE, append = TRUE)
  }
}

