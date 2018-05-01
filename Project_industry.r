### twitter data
library(twitteR)

# set up twitter authentication

api_key = "CBmxB5BVLxBBHZJjnXf5aHRJY"
api_secret = "fJMaz22l97EZmIlWDKi3ERSyCONrjYU2MP1CLaOwQcUqLnqmsR"
access_token = "2173535746-OSlgJoWyh9pfB02pdyhQegZ2dcjR7ctbtqDo0O8"
access_token_secret = "Kyi326mBG7uWXQJ4r2KEfJPztrptuiq2gxiIG2DBziOUx"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

### search for tweets about USC
homecareTweets = searchTwitter("homecare", n=500)
homecareTweetsDF = twListToDF(homecareTweets)

### what are them tweeting about
library(tm)
library(stringr)
library(wordcloud)
library(dplyr)
library(ggplot2)


# clean the tweets and then visualize them using a wordcloud
nohandles = str_replace_all(homecareTweetsDF$text, pattern = "@\\w+", replacement = "")

# these coding can applied to any words
wordCorpus = Corpus(VectorSource(nohandles))%>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

pal = brewer.pal(9, "YlGnBu") 
display.brewer.pal(9, "YlGnBu")

pal = pal[-(1:4)]
set.seed(500)

wordcloud(words = wordCorpus, random.order = FALSE, colors = pal)

## sentiment Analysis for homecare tweets
library(syuzhet)

hcSentiment = get_nrc_sentiment(homecareTweetsDF$text)
data = apply(hcSentiment, MARGIN = 2, FUN = sum) #margin: 1-row, 2-col

data = data.frame(count = data)

data$emotion = rownames(data)
ggplot(data, aes(y= count, x = reorder(emotion, count)))+
  geom_col()

# where are they twitting from
homecareTweets2 = searchTwitter("homecare", n=3000)
homecareTweetsDF2 = twListToDF(homecareTweets2)
position = homecareTweetsDF2[15:16]
position = na.exclude(position)

library(maps)
library(ggplot2)
#get map for the USA
states_map = map_data("state") #ggplot2 function to retrieve map data
us_map = ggplot(states_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill ="white" ,color = "black")+
  theme_clean()

# add theme
theme_clean <- function(base_size = 12){
  require(grid) #needed for unit() function
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0,0,0,0), "lines"),
      complete = TRUE
    )
}

position$longitude = as.numeric(position$longitude)
position$latitude = as.numeric(position$latitude)

us_map+
  geom_point(data = position, aes(x = longitude, y = latitude, group=1))







