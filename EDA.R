
library(readr)
library(tidyr)
library(ggplot2)

df <- read_csv("sephora_reviews_final.csv")
df <- as.data.frame(df)
df <- df[df$recommended == TRUE,]
df <- subset(df, select = -c(recommended,eye_color,age,beauty_insider,first_submission_date,last_submission_date,location,incentivized_review))
df <- drop_na(df,names(df))
df$rating <- as.factor(df$rating)

length(unique(df$brand))
length(unique(df$product_id))
length(unique(df$user_name))

plot(df$rating, df$price, xlab = "Rating", ylab = "Price", main = "Price by Rating", col = "lightpink")
barplot(table(df$coverage), ylim = c(0,16000), col = "lightblue", xlab = "Coverage", ylab = "Frequency", main = "Frequency of Coverage Types")
barplot(table(df$finish), ylim = c(0,20000), col = "lightblue", xlab = "Finish", ylab = "Frequency", main = "Frequency of Each Finish")
barplot(table(df$formulation), ylim = c(0,30000), col = "lightblue", xlab = "Formulation", ylab = "Frequency", main = "Frequency of Each Formulation")
barplot(table(df$skin_type), ylim = c(0,16000), col = "lightgreen", xlab = "Skin Type", ylab = "Frequency", main = "Frequency of Skin Type of Reviewers")
barplot(table(df$skin_tone), ylim = c(0,8000), col = "lightgreen", xlab = "Skin Tone", ylab = "Frequency", main = "Frequency of Skin Tone of Reviewers")
barplot(table(df$skin_concerns), ylim = c(0,16000), col = "lightgreen", xlab = "Skin Concern", ylab = "Frequency", main = "Frequency of Skin Concern of Reviewers",las = 2, cex.names = 0.7)

rate1 <- df[df$rating == "1",]
rate5 <- df[df$rating == "5",]

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

library(tm)
text <- rate1$review_text
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
dfu <- data.frame(word = names(words),freq=words)

set.seed(1234)  
wordcloud(words = dfu$word,freq = dfu$freq,min.freq = 1,max.words=200,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"),scale=c(5,0.50))

text <- rate5$review_text
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
dfv <- data.frame(word = names(words),freq=words)

set.seed(1234)  
wordcloud(words = dfv$word,freq = dfv$freq,min.freq = 1,max.words=200,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"),scale=c(5,0.50))

