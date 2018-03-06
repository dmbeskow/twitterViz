require(readr)
require(lubridate)
require(dplyr)
require(igraph)
require(ggthemes)

format.twitter.date <- function(datestring){
hashtags <- read_csv('blackPanther_hashtags_20180228-200156.csv', col_types = "cccc")
mention <- read_csv('blackPanther_mentions_20180228-202055.csv', col_types = "cccc")
retweet <- read_csv('blackPanther_retweetNetwork_20180228-203029.csv', col_types = "cccc")

hashtags$datetime <- format.twitter.date(hashtags$date)
hashtags$date <- lubridate::date(hashtags$datetime)

mention$datetime <- format.twitter.date(mention$date)
mention$date <- lubridate::date(mention$datetime)

retweet$datetime <- format.twitter.date(retweet$date)
retweet$date <- lubridate::date(retweet$datetime)

df1 <- hashtags[,c('user','hashtag', 'date')]
names(df1) <- c("from", "to", "date")
df2 <- mention[,c('user', 'mention', 'date')]
names(df2) <- c("from", "to", "date")
df3 <- retweet[,c('retweeter', 'retweeted', 'date')]
names(df3) <- c("from", "to", "date")

edgelist <- rbind(df1, df2, df3)
edgelist <- edgelist[!is.na(edgelist$to),]

mydates <- unique(edgelist$date)

final <- data.frame(
  date = mydates,
  nodes = rep(NA, length(mydates)),
  edges = rep(NA, length(mydates)),
  components = rep(NA, length(mydates)),
  density = rep(NA, length(mydates)),
  component_mean_size = rep(NA, length(mydates)),
  mean_degree = rep(NA, length(mydates))
)

for(i in 1:length(mydates)){
  temp <- edgelist %>% filter(date == mydates[i]) %>% select(from, to)
  g <- graph_from_edgelist(as.matrix(temp))
  final$nodes[i] <- length(V(g))
  final$edges[i] <- length(E(g))
  final$components[i] <- components(g)$no
  final$density[i] <- edge_density(g)
  final$component_mean_size[i] <- mean(table(components(g)$membership))
  final$mean_degree[i] <- mean(degree(g))
}

final2 <- tidyr::gather(final, key = date, value = 'n')
names(final2) <- c('date', 'type', 'n')


t <- ggplot(final2, aes(date, n, color = type)) + geom_line() + scale_fill_hc() + theme_tufte()
t + facet_wrap( ~ type, scales = 'free') +theme(legend.position="none")
ggsave('network_measuers_over_time.pdf')
