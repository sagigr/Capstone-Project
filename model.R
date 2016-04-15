library(tm)

bigram <- readRDS(file="bigram.Rda")
trigram <- readRDS(file="trigram.Rda")
fourgram <- readRDS(file="fourgram.Rda")


nextWordPredictor <- function(inputTxt) {
    
    if(nchar(inputTxt) > 0) {
        inputTxt <- tolower(inputTxt)
        inputTxt <- removeNumbers(inputTxt)
        inputTxt <- removePunctuation(inputTxt)
        inputTxt <- stripWhitespace(inputTxt)
        
        inputList <- unlist(strsplit(inputTxt, " "))
        
        print(inputList)
        
        
        numWords <- length(inputList)
        
        print(numWords)
        
        runBigram <- function(words){
            bigram[bigram$terms$one == words,]$terms$two
        }
        
        runTrigram <- function(words){
            trigram[trigram$terms$one == words[1] &
                       trigram$terms$two == words[2],]$terms$three
        }
        
        runFourgram <- function(words) {
            fourgram[ fourgram$terms$one == words[1] &
                         fourgram$terms$two == words[2] &
                         fourgram$terms$three == words[3],]$terms$four
        }
        
        if(numWords == 1) {
            predList <- runBigram(inputList[1])
        }else if (numWords == 2) {
            word1 <- inputList[1]
            word2 <- inputList[2]
            predList <- runTrigram(c(word1, word2))
            
            if(length(predList) == 0){
                predList <- runBigram(word2)
            }
        }else {
            word1 <- inputList[numWords-2]
            word2 <- inputList[numWords-1]
            word3 <- inputList[numWords]
            predList <- runFourgram(c(word1, word2, word3))
            
            if(length(predList) == 0){
                predList <- runTrigram(c(word2,word3))
            }
            
            if(length(predList) == 0){
                predList <- runBigram(word3)
            }
        }
        
        n <- 4
        tp <- length(predList)
        
        if( tp >= n){
            predList <- predList[1:n]
        }
        
        as.character(predList)
    }else{
        ""
    }
    
    
    
}
