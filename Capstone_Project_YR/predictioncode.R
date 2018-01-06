#Tokenizer for prediction model
tokenizer <- function (filename){
        corpus <- VCorpus(VectorSource(filename))
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        #corpus <- tm_map(corpus, removeWords, stopwords("en"))
        #corpus <- tm_map(corpus, removeWords, profanity)
        corpus <- tm_map(corpus, stripWhitespace)
        #corpus <- tm_map(corpus, stemDocument)          #maybe add later
}



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
        else if (inputlength == 3) {
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
        else if(inputlength == 2) {
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
        else if(inputlength == 1) {
                print(13)
                wordprediction <- rbind(wordprediction, BiGram[BiGram$STShort == word1,])
                if (nrow(wordprediction) < 6) {
                        print(14)
                        wordprediction <- rbind(wordprediction, UniGram[1:5,])
                }
        }
        else if(inputlength == 0) {
                print(15)
                wordprediction <- UniGram[1:5,]
        }
        print(head(wordprediction,10))
}