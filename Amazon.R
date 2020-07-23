library(rvest)
library(XML)
library(magrittr)
library(dplyr)
## Amazon Reviews on Oneplus 7 Pro
aurl <- "https://www.amazon.in/Test-Exclusive-749/dp/B07DJ8K2KT/ref=sr_1_2?crid=W73NOU7OQKEG&dchild=1&keywords=oneplus+7+pro+mobile&qid=1590515852&sprefix=one%2Caps%2C416&sr=8-2"
## Each page contains 7 reviews
## Inorder to get 200 reviews
amazon_reviews <- NULL
for (i in 1:29){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))  # Use html()
  murl
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
View(amazon_reviews)

write.table(amazon_reviews,"Oneplus.txt",row.names = F)

Amazon_data <- readLines("F:/Data Sci/R Programming/Assignment/Text Mining/Oneplus.txt")
View(Amazon_data)
## Preprocessing of data
## Cleaning 1 remove unnecessary spaces
text1 <- gsub("^\\s+|\\s+$", "",Amazon_data) 

## Cleaning 2 remove punctations
text2 <- gsub("[[:punct:]]","",text1)

## Cleaning 3 remove digits
text3 <- gsub("[[:digit:]]","",text2)

## Cleaning 4 remove emojis
text4 <- gsub("<.*>","",text3)

## Cleaning 5 remove emoji and bizzarrie signs
text5 <- iconv(from="latin1",to="ASCII",sub="",text4)

## Cleaning 6 remove blank spaces
text6 <- gsub("^ ","",text5)
text7 <- gsub(" $","",text6)

## Cleaning 7 remove "Read more" as it is unneccesary
text8 <- gsub("Read more","",text7)
View(text8[1:15])
## To remove all empty rows
text8[text8==""] <- NA
text9 <- na.omit(text8)
View(text9[1:15])

write.csv(text9,"Oneplus.csv")


library(tm)
library(wordcloud)
Amazon_corpus <- Corpus(VectorSource(text9))
Clean_Corpus <- tm_map(Amazon_corpus,content_transformer(tolower))
Clean_Corpus <- tm_map(Clean_Corpus,removeNumbers)
Clean_Corpus <- tm_map(Clean_Corpus,removePunctuation)
Clean_Corpus <- tm_map(Clean_Corpus,removeWords,stopwords("english"))
## Clean html Links
remove_URL <- function(x) gsub("http[[:alnum:]]*","",x)
Clean_Corpus <- tm_map(Clean_Corpus,content_transformer(remove_URL))
Clean_Corpus <- tm_map(Clean_Corpus, stripWhitespace)
inspect(Clean_Corpus[1:15])

## Plot WordCloud
pal=brewer.pal(8,"Dark2")
wordcloud(Clean_Corpus,min.freq = 5,max.words = Inf,col=pal)

### Sentimental Analysis ###
library(syuzhet)
Amazon_text <- readLines("F:/Data Sci/R Programming/Assignment/Text Mining/Oneplus.csv")
Amazon_text

## Extracting each Sentences from Amazon text
sv1 <- get_sentences(Amazon_text)
class(sv1)
str(sv1)
head(sv1)
## Extract sentiment vector from each sentences
senti_vector <- get_sentiment(sv1,method="bing")
head(senti_vector)

nrc1_vector <- get_sentiment(sv1,method="nrc")
head(nrc1_vector)

afinn_vector <- get_sentiment(sv1,method="afinn")
head(afinn_vector)

# plot
plot(senti_vector, type = "l", main = "Plot Trajectory",
     xlab = "Score", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- sv1[which.min(senti_vector)]
negative

# To extract the most positive sentence
positive <- sv1[which.max(senti_vector)]
positive

### Categorize each sentence by eight emotions
sentiment <- get_nrc_sentiment(sv1)
SentimentScores <- data.frame(colSums(sentiment[,]))
View(SentimentScores)



## Visualization of Emotions
## Barplot
barplot(colSums(sentiment),main="Sentiment Score based on Oneplus 7 Amazon reviews",
        las=2.5,ylab="Score",cex.names =0.8,col=rainbow(10))


