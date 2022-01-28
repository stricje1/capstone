---
title: "Milestone Report"
author: "Jeffrey Strickland"
date: "1/25/2022"
output: 
  html_document:
    keep_md: TRUE
---

## Project Introduction
The goal of this project is just to display that I am proficient in working with text data and that I am on track to create my prediction algorithm. This milestone report explains my exploratory analysis and my goals for the eventual app and algorithm. This document explains only the major features of the data and briefly summarize my plans for creating the prediction algorithm and Shiny app. This report makes use of tables and plots to illustrate important summaries of the data set. The motivation for this project is to: 
1. Demonstrate that I've downloaded the data and have successfully loaded it in RStudio.
2. Create a basic report of summary statistics about the data sets.
3. Report any interesting findings that I discovered so far.
4. Get feedback on my plans for creating a prediction algorithm and Shiny app. 

## The Data
The data for this project is from a corpus called HC Corpora. The corpora are collected from publicly available sources by a web crawler. The crawler checks for language, so as to mainly get texts consisting of the desired language, English in this case.

Each entry is a line of text tagged with it's date of publication. Where user comments are included they will be tagged with the date of the main entry.

Each entry is tagged with the type of entry, based on the type of website it is collected from (e.g. newspaper or personal blog) If possible, each entry is tagged with one or more subjects based on the title or keywords of the entry (e.g. if the entry comes from the sports section of a newspaper it will be tagged with "sports" subject).In many cases it's not feasible to tag the entries (for example, it's not really practical to tag each individual Twitter entry or no subject is found by the automated process, in which case the entry is tagged with a '0'. To save space, the subject and type is given as a numerical code.

Once the raw corpus has been collected, it is parsed further, to remove duplicate entries and split into individual lines. The data is accessed from the Coursera site, given by the url below.

## Downloading and Unzipping the Data
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("Coursera-SwiftKey.zip")) {
  download.file(url, "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip", exdir="C:/Users/jeff/Documents/R/capstone")}

## Loading Requied Libraries



## Reading the Datasets
As the data is compiled by lines, we use `readLines()` to get text lines from the appropriate directory. We also ensure we get the proper encoding and skip null entries.


```r
blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
```

```
## Warning in readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", :
## incomplete final line found on './final/en_US/en_US.news.txt'
```

```r
twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
```

## Exploratory Data Analysis
The first exploration we perform is determining the size of each file, in MB: 


```r
blogs_size <- file.size("./final/en_US/en_US.blogs.txt")/ 1024^2
news_size <- file.size("./final/en_US/en_US.news.txt")/ 1024^2
twitter_size <- file.size("./final/en_US/en_US.twitter.txt")/ 1024^2
```

Next, we construct a table that has the file size we just calaculated, as well as the number of lines per file, the number of empty lines, characters, whiyte spaces, and word counts.


```r
stats <- data.frame(
  FileName=c(".blogs","news","twitter"),
  FileSizeinMB=c(blogs_size,
                 news_size,
                 twitter_size),
  t(rbind(sapply(list(blogs,news,twitter), stri_stats_general),
          WordCount=
            sapply(list(blogs, news, twitter),
                   stri_stats_latex)[4,]))
)
kable(stats)
```



|FileName | FileSizeinMB|   Lines| LinesNEmpty|     Chars| CharsNWhite| WordCount|
|:--------|------------:|-------:|-----------:|---------:|-----------:|---------:|
|.blogs   |     200.4242|  899288|      899288| 206824382|   170389539|  37570839|
|news     |     196.2775|   77259|       77259|  15639408|    13072698|   2651432|
|twitter  |     159.3641| 2360148|     2360148| 162096241|   134082806|  30451170|

## Data Preparation
Next, we sample the data from the files, since the files sizes are very large. Then we create a corpus (body of text) comprised of the thee samples, blogs, news, and tweets.


```r
subblogs <- sample(blogs, size = 1000)
subnews <- sample(news, size = 1000)
subtwitter <- sample(twitter, size = 1000)
sampledata <- c(subblogs, subnews, subtwitter)
corpus <- VCorpus(VectorSource(sampledata))
```

#Create Transform Function
Since, our intent is to clean the raw text data to make it suitable for our analysis, we construct transformation functions to:
* Remove urls
* Remove puncuation
* Remove Arabic numbers
* Remove apostrophe
* Remove non-ASCII characters (special characters)
* Remove repeats
* Convert to lower case
* Remove extra spaces


```r
removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
removeSign <- function(x) gsub("[[:punct:]]","",x)
removeNum <- function(x) gsub("[[:digit:]]","",x)
removeapo <- function(x) gsub("'","",x)
removeNonASCII <- function(x) iconv(x, "latin1", "ASCII", sub="")
removerepeat <- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1", x)
toLowerCase <- function(x) sapply(x,tolower)
removeSpace <- function(x) gsub("\\s+"," ",x)
```

## Transform the corpus
Now we use the above functions to quickly transform the data, removing stopwords, punctuation, whitespaces, numbers etc. from the corpuses.


```r
corpus<-tm_map(corpus,content_transformer(removeapo))#remove apostrophe
corpus<-tm_map(corpus,content_transformer(removeNum))#remove numbers
corpus<-tm_map(corpus,content_transformer(removeURL)) #remove web url
corpus<-tm_map(corpus,content_transformer(removeSign)) #remove number and punctuation except apostrophe
corpus<-tm_map(corpus,content_transformer(removeNonASCII)) #remove non-ASCII
corpus<-tm_map(corpus,content_transformer(toLowerCase))# convert uppercase to lowercase
corpus<-tm_map(corpus,content_transformer(removerepeat))# remove repeated alphabets in a words
corpus<-tm_map(corpus,content_transformer(removeSpace)) #remove multiple space
corpus<-tm_map(corpus,removeWords,stopwords("english")) #remove common english words
```

## Wordcloud
To examine the data, we will produce a word cloud showing frequently used terms in the datasets. The word clouds show generally the top words with size varying by frequency.


```r
wordcloud(corpus, max.words=75, random.order=TRUE, rot.per=.15, colors=colorRampPalette(brewer.pal(9,"Blues"))(32), scale=c(3, .3))
```

![](milestone_report_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Create the DocumentTermMatrizes
Now, we tokenize the text by forming n-grams (words) as tokens. A `1-gram` contains one word, a `2-gram` contains two words, and an `n-gram` contains `n` words.


```r
dtm1 <- TermDocumentMatrix(corpus)
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm2 <- TermDocumentMatrix(corpus, control = list(tokenize = bigram))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm3 <- TermDocumentMatrix(corpus, control = list(tokenize = trigram))
```
## n-gram Frequencies
Now, we extract the top 20 (most frequent) words for each category of n-grams, from 1 to 3. The then plot their frequencis using a horizontal bar-chart for each ste of n-grams.

### 1-Gram Frequency


```r
freq1 <- rowSums(as.matrix(dtm1))
freq1 <- sort(freq1, decreasing = TRUE)
dfFreq1 <- data.frame(word = names(freq1), freq=freq1)
options(repr.plot.width=8, repr.plot.height=3)
ggplot(dfFreq1[1:20, ], aes(word, freq)) + 
  ggtitle("1-grams Frequencies") +
  geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
  coord_flip() + scale_y_continuous(name="Word Frequency") +
  scale_x_discrete(name="1-grams") +
  theme(plot.title =  element_text(face="bold", color="steelblue4",
                                   size=12),
        axis.text.x = element_text(face="bold", color="steelblue4",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="steelblue4",
                                   size=8, angle=0))
```

![](milestone_report_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### 2-Gram Frequency


```r
freq2 <- rowSums(as.matrix(dtm2))
freq2 <- sort(freq2, decreasing = TRUE)
dfFreq2 <- data.frame(word = names(freq2), freq=freq2)
options(repr.plot.width=8, repr.plot.height=3)
ggplot(dfFreq2[1:20, ], aes(word, freq)) + 
  ggtitle("2-grams Frequencies") +
  geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
  coord_flip() + scale_y_continuous(name="Word Frequency") +
  scale_x_discrete(name="2-grams") +
  theme(plot.title =  element_text(face="bold", color="steelblue4",
                                   size=12),
        axis.text.x = element_text(face="bold", color="steelblue4",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="steelblue4",
                                   size=8, angle=0))
```

![](milestone_report_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### 3-Gram Frequency


```r
freq3 <- rowSums(as.matrix(dtm3))
freq3 <- sort(freq3, decreasing = TRUE)
dfFreq3 <- data.frame(word = names(freq3), freq=freq3)
options(repr.plot.width=8, repr.plot.height=3)
ggplot(dfFreq3[1:20, ], aes(word, freq)) + 
  ggtitle("3-grams Frequencies") +
  geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
  coord_flip() + scale_y_continuous(name="Word Frequency") +
  scale_x_discrete(name="3-grams") +
  theme(plot.title =  element_text(face="bold", color="steelblue4",
                                   size=12),
        axis.text.x = element_text(face="bold", color="steelblue4",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="steelblue4",
                                   size=8, angle=0))
```

![](milestone_report_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Future work
The goal of the project is to create a predictive model, which predicts the most probable words to follow an input from the user. This model will be evaluated and deployed as a shiny application.



