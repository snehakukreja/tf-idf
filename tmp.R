
# Most recent query: 9/30/14
tw.query <- query(
  "SELECT id,
    api_text,
    SUM(promoted_tweet_timeline_impressions) impressions,
    SUM(promoted_tweet_timeline_clicks) clicks
  FROM tw_promoted_tweet_stats p
  LEFT JOIN tw_statuses s ON p.tw_status_id = s.id
  WHERE api_text LIKE '%http%'
  GROUP BY id, api_text;"
)

df <- tw.query %>% group_by(api_text) %>% summarize(ctr = sum(clicks)/sum(impressions) * 100)
df$ctr <- ifelse(is.nan(df$ctr), 0, df$ctr)
df <- arrange(df, -ctr)


tfidf <- function(ft, max_ft, ndocs, ncorpus) {
  (1/ncorpus + ft/max_ft) * log(ncorpus/ndocs)
}

# df <- expand.grid(ft = c(5,10,20,30), ndocs = c(1,2,3,5,10))

for (i in 1:nrow(df)) {
  df$ft[i] <- length(grep('Watch', df$api_text[i], ignore.case = TRUE))
}


model <- lm(ctr ~ api_text, data = df)
summary(model)

ggplot(arrange(df, ctr), aes(api_text, ctr)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  theme_tufte() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(title = '', x = 'Text', y = 'CTR')




