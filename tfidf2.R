df <- read.csv("data/tw1.csv", stringsAsFactors = FALSE)

df <- df %>% 
  rename(Advertiser = advertisers_company_name) %>% 
  group_by(Advertiser, api_text) %>% 
  summarize(Impressions = sum(impressions), Clicks = sum(clicks)) %>% 
  mutate(CTR = Clicks/Impressions * 100)

# Get term frequency
tf <- function(t, d) {
  # Split document (tweet) into separate words
  terms <- (strsplit(d[i], " ", fixed = FALSE))[[1]]
  
  # Get number of times that "t" appears in the tweet
  tf <- length(terms[terms == t])
  return(tf)
}

# Get inverse document frequency
idf <- function(t, D) {
  # Number of documents in the corpus
  N <- length(D)
  
  # Number of documents where the term "t" appears
  ndocs <- 0
  for (d in D) {
    if (tf(t, d) != 0) {
      ndocs <- ndocs + 1
    }
  }  
  return(log(N/ndocs))
}

# Get term frequency - inverse document frequency
tfidf <- function(t, d, D) {
  return(tf(t, d) * idf(t, D))
}

common.terms <- c("a", "and", "the", "to", "by", "at", "for", " ")
output <- NULL
for (i in 1:nrow(df)) {
  tweet <- df$api_text[i]
  terms <- (strsplit(tweet, " ", fixed = FALSE))[[1]]
  
  for (t in terms) {
    if (!(t %in% common.terms)) {
      tnum <- tfidf(t, tweet, df$api_text)
      output <- c(output, paste("tf-idf for ", t, " is ", tnum, sep = ""))
    }
  } 
}

output <- as.data.frame(output)
write.csv(output, "data/result.csv")