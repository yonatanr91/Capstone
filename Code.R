#Download Data
setwd("C:/Users/yrafael/Desktop/Data Science Course/Course Code/Capstone")
if(!file.exists("Coursera-SwiftKey.zip")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" 
        download.file(fileUrl, destfile="C:/Users/yrafael/Desktop/Data Science Course/Course Code/Capstone/Coursera-SwiftKey.zip")
        unzip("Coursera-SwiftKey.zip")
}

if(!file.exists("badwords.txt")) {
        fileUrl2 <- "http://www.bannedwordlist.com/lists/swearWords.txt"
        download.file(fileUrl2, destfile="C:/Users/yrafael/Desktop/Data Science Course/Course Code/Capstone/badwords.txt")
}

#Read Data
badwords <- readLines("badwords.txt")

setwd("C:/Users/yrafael/Desktop/Data Science Course/Course Code/Capstone/final/en_US")
US_Blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8")
US_News <- readLines("en_US.news.txt", encoding = "UTF-8")
US_Twitter <- readLines("en_US.twitter.txt")

#Info about data
setwd("C:/Users/yrafael/Desktop/Data Science Course/Course Code/Capstone/final/en_US")
stats <- data.frame(Source = c("Blogs", "News", "Twitter"),
                    Size_MB = c(file.info("en_US.blogs.txt")$size * 0.000001, file.info("en_US.news.txt")$size * 0.000001, file.info("en_US.twitter.txt")$size * 0.000001),
                    NumberLines = c(length(US_Blogs), length(US_News), length(US_Twitter)))
stats

#Necessary packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(dplyr)
library(slam)

#Tokenization
sample_US_Blogs <- sample(US_Blogs, length(US_Blogs)*.15) 
sample_US_News <- sample(US_News, length(US_News)*.15)
sample_US_Twitter <- sample(US_Twitter, length(US_Twitter)*.15)
combineddata <- c(sample_US_Blogs, sample_US_News, sample_US_Twitter)
cleandata <- gsub("[^A-Za-z0-9 ]", "", combineddata)       #remove non-alphabetic/numeric characters

profanity <- VectorSource(badwords)

tokenizer <- function (filename){
        corpus <- VCorpus(VectorSource(filename))
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        #corpus <- tm_map(corpus, removeWords, stopwords("en"))
        corpus <- tm_map(corpus, removeWords, profanity)
        corpus <- tm_map(corpus, stripWhitespace)
        #corpus <- tm_map(corpus, stemDocument)          #maybe add later
}

corpusdata <- tokenizer(cleandata)

#Modeling
BiToken <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TriToken <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadToken <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
QuintToken <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

tdm1 <- rollup(TermDocumentMatrix(corpusdata), 2, na.rm = TRUE, FUN = sum)
tdm2 <- rollup(TermDocumentMatrix(corpusdata, control = list(tokenize = BiToken)), 2, na.rm = TRUE, FUN = sum)
tdm3 <- rollup(TermDocumentMatrix(corpusdata, control = list(tokenize = TriToken)), 2, na.rm = TRUE, FUN = sum)
tdm4 <- rollup(TermDocumentMatrix(corpusdata, control = list(tokenize = QuadToken)), 2, na.rm = TRUE, FUN = sum)
tdm5 <- rollup(TermDocumentMatrix(corpusdata, control = list(tokenize = QuintToken)), 2, na.rm = TRUE, FUN = sum)

df1 <- data.frame(ST = rownames(as.matrix(tdm1)), Freq = rowSums(as.matrix(tdm1)), row.names = NULL)
df2 <- data.frame(ST = rownames(as.matrix(tdm2)), Freq = rowSums(as.matrix(tdm2)), row.names = NULL)
df3 <- data.frame(ST = rownames(as.matrix(tdm3)), Freq = rowSums(as.matrix(tdm3)), row.names = NULL)
df4 <- data.frame(ST = rownames(as.matrix(tdm4)), Freq = rowSums(as.matrix(tdm4)), row.names = NULL)
df5 <- data.frame(ST = rownames(as.matrix(tdm5)), Freq = rowSums(as.matrix(tdm5)), row.names = NULL)

UniGram <- df1 %>% arrange(desc(Freq)) %>%
        filter(Freq > 1) %>%
        mutate(ST = as.character(ST)) %>%
        mutate(STShort = gsub("\\s*\\w*$", "", ST), Pred = sub('^.* ([[:alnum:]]+)$', '\\1', ST), Odds = Freq / sum(Freq))
BiGram <- df2 %>% arrange(desc(Freq)) %>%
        filter(Freq > 1) %>%
        mutate(ST = as.character(ST)) %>%
        mutate(STShort = gsub("\\s*\\w*$", "", ST), Pred = sub('^.* ([[:alnum:]]+)$', '\\1', ST), Odds = Freq / sum(Freq))
TriGram <- df3 %>% arrange(desc(Freq)) %>%
        filter(Freq > 1) %>%
        mutate(ST = as.character(ST)) %>%
        mutate(STShort = gsub("\\s*\\w*$", "", ST), Pred = sub('^.* ([[:alnum:]]+)$', '\\1', ST), Odds = Freq / sum(Freq))
QuadGram <- df4 %>% arrange(desc(Freq)) %>%
        filter(Freq > 1) %>%
        mutate(ST = as.character(ST)) %>%
        mutate(STShort = gsub("\\s*\\w*$", "", ST), Pred = sub('^.* ([[:alnum:]]+)$', '\\1', ST), Odds = Freq / sum(Freq))
QuintGram <- df5 %>% arrange(desc(Freq)) %>%
        filter(Freq > 1) %>%
        mutate(ST = as.character(ST)) %>%
        mutate(STShort = gsub("\\s*\\w*$", "", ST), Pred = sub('^.* ([[:alnum:]]+)$', '\\1', ST), Odds = Freq / sum(Freq))

#Check what it looks like
head(UniGram, 10)
head(BiGram, 10)
head(TriGram, 10)
head(QuadGram, 10)
head(QuintGram, 10)

#Evaluate Size
format(object.size(UniGram), units = "Mb")
format(object.size(BiGram), units = "Mb")
format(object.size(TriGram), units = "Mb")
format(object.size(QuadGram), units = "Mb")
format(object.size(QuintGram), units = "Mb")

#Save
setwd("C:/Users/yrafael/Desktop/Data Science Course/Course Code/Capstone/Capstone_Project_YR")
save(UniGram, file = "UniGram.RData")
save(BiGram, file = "BiGram.RData")
save(TriGram, file = "TriGram.RData")
save(QuadGram, file = "QuadGram.RData")
save(QuintGram, file = "QuintGram.RData")

#Exploratory Analysis
#Build Word Cloud
par(mfrow = c(1, 3))
wordcloud(words = UniGram$ST, freq = UniGram$Freq,
          max.words=100, random.order=FALSE, colors = brewer.pal(4, "Set1"), main = "Single Gram")
text(x=0.5, y=1.2, "Single Gram")
wordcloud(words = BiGram$ST, freq = BiGram$Freq,
          max.words=100, random.order=FALSE, colors = brewer.pal(4, "Set1"), main = "Bi Gram")
text(x=0.5, y=1.2, "Bi Gram")
wordcloud(words = TriGram$ST, freq = TriGram$Freq,
          max.words=100, random.order=FALSE, colors = brewer.pal(4, "Set1"), main = "Tri Gram")
text(x=0.5, y=1.2, "Tri Gram")

#Bar Plot
par(mfrow = c(1, 3))
barplot(rev(UniGram[1:15,]$Freq), las = 2, names.arg = rev(UniGram[1:15,]$ST),
        col ="darkblue", main ="Single Gram",
        xlab = "Frequencies", horiz=TRUE)
barplot(rev(BiGram[1:15,]$Freq), las = 2, names.arg = rev(BiGram[1:15,]$ST),
        col ="darkblue", main ="Bi Gram",
        xlab = "Frequencies", horiz=TRUE)
barplot(rev(TriGram[1:15,]$Freq), las = 2, names.arg = rev(TriGram[1:15,]$ST),
        col ="darkblue", main ="Tri Gram",
        xlab = "Frequencies", horiz=TRUE)

#Prediction Model
prediction <- function(input) {
        inputtoken <- tokenizer(input)
        inputcontent <- inputtoken[[1]]$content
        inputstring <- unlist(strsplit(inputcontent, " "))
        inputlength <- length(inputstring)
        
        wordprediction <- data.frame()
        
        word4 <- inputstring[inputlength - 3]
        print(word4)
        word3 <- inputstring[inputlength - 2]
        print(word3)
        word2 <- inputstring[inputlength - 1]
        print(word2)
        word1 <- inputstring[inputlength]
        print(word1)
        
        if (inputlength >= 4) {
                print(1)
                wordprediction <- rbind(wordprediction, QuintGram[QuintGram$STShort == paste(word4, word3, word2, word1),])
                if (nrow(wordprediction) < 6) {
                        print(2)
                        wordprediction <- rbind(wordprediction, QuadGram[QuadGram$STShort == paste(word3, word2, word1),])
                        if (nrow(wordprediction) < 6) {
                                print(3)
                                wordprediction <- rbind(wordprediction, TriGram[TriGram$STShort == paste(word2, word1),])
                                if (nrow(wordprediction) < 6) {
                                        print(4)
                                        wordprediction <- rbind(wordprediction, BiGram[BiGram$STShort == word1,])
                                        if (nrow(wordprediction) < 6) {
                                                print(5)
                                                wordprediction <- rbind(wordprediction, UniGram[1:5,])
                                        }
                                }
                        }
                }
        }
        if (inputlength == 3) {
                print(6)
                wordprediction <- rbind(wordprediction, QuadGram[QuadGram$STShort == paste(word3, word2, word1),])
                if (nrow(wordprediction) < 6) {
                        print(7)
                        wordprediction <- rbind(wordprediction, TriGram[TriGram$STShort == paste(word2, word1),])
                        if (nrow(wordprediction) < 6) {
                                print(8)
                                wordprediction <- rbind(wordprediction, BiGram[BiGram$STShort == word1,])
                                if (nrow(wordprediction) < 6) {
                                        print(9)
                                        wordprediction <- rbind(wordprediction, UniGram[1:5,])
                                }
                        }
                }
        }
        if(inputlength == 2) {
                print(10)
                wordprediction <- rbind(wordprediction, TriGram[TriGram$STShort == paste(word2, word1),])
                if (nrow(wordprediction) < 6) {
                        print(11)
                        wordprediction <- rbind(wordprediction, BiGram[BiGram$STShort == word1,]) 
                        if (nrow(wordprediction) < 6) {
                                print(12)
                                wordprediction <- rbind(wordprediction, UniGram[1:5,])
                        }
                }
        }
        if(inputlength == 1) {
                print(13)
                wordprediction <- rbind(wordprediction, BiGram[BiGram$STShort == word1,])
                if (nrow(wordprediction) < 6) {
                        print(14)
                        wordprediction <- rbind(wordprediction, UniGram[1:5,])
                }
        }
        if(inputlength == 0) {
                print(15)
                wordprediction <- UniGram[1:5,]
        }
        print(head(wordprediction,10))
        return(wordprediction)
}
       
#Evaluate Accuracy
system.time(prediction("I'd give anything to see arctic monkeys this"))
system.time(prediction("Hey sunshine, can you follow me and make me the"))
system.time(prediction("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"))


#Clean up space
rm(US_Blogs, US_News, US_Twitter, badwords, stats, sample_US_Blogs, sample_US_News, sample_US_Twitter, combineddata, cleandata, corpusdata, tokenizer, profanity, BiToken, TriToken, QuadToken, QuintToken, tdm, tdm2, tdm3, tdm4, tdm5, df1, df2, df3, df4, df5, df1sparce, df2sparce, df3sparce, df4sparce, df5sparce, prediction())


