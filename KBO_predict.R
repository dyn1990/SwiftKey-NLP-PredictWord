#########################################################
##### predict the next word based on katz backoff #####
#########################################################
# unigram <- readRDS("1gram.rds")
# bigram <- readRDS("2gram.rds")
# trigram <- readRDS("3gram.rds")
# gtSmooth <- readRDS("gtSmooth.rds")
##############################
##### predict conditions #####
##############################

predict.word <- function(phrase){
        tokens <- strsplit(phrase, " ")[[1]]
        if (length(tokens) == 1){
                  bigram.subset <- bigram %>% 
                          filter(pre == tokens[1])
                  if (nrow(bigram.subset) > 0){
                            records <- min(nrow(bigram.subset), 3)
                            bigram.subset <- bigram.subset[1:records, ]
                            count.bi <- bigram.subset$discount
                            count.uni <- unigram %>% filter(word == tokens[1])
                            count.uni <- count.uni$discount
                            prob <- count.bi / count.uni
                            bigram.subset <- bigram.subset %>% mutate(probFinal = prob)
                            return(bigram.subset[,c("last", "probFinal")])
                  }
                  else{
                    unigram.subset <- unigram[,c("word", "prob")]
                    colnames(unigram.subset) <- c("last", "probFinal")
                    return(unigram.subset)
                  }
        }
        
        if (length(tokens) >= 2){
                tokens <- c(tokens[length(tokens) - 1], tokens[length(tokens)])
                firstTwoTerm <-  paste(tokens[1], tokens[2], collapse = " ")
                trigram.subset <- trigram %>% 
                        filter(pre == firstTwoTerm)
                if (nrow(trigram.subset) > 0){
                        records <- min(nrow(trigram.subset), 3)
                        trigram.subset <- trigram.subset[1:records, ]
                        count.tri <- trigram.subset$discount
                        count.bi <- bigram %>% filter(word == firstTwoTerm)
                        count.bi <- count.bi$discount
                        prob <- count.tri / count.bi
                        trigram.subset <- trigram.subset %>% mutate(probFinal = prob)
                        return(trigram.subset[,c("last", "probFinal")])
                }
                else{
                  bigram.subset <- bigram %>% 
                          filter(pre == tokens[2])
                  if (nrow(bigram.subset) > 0){
                          records <- min(nrow(bigram.subset), 3)
                          bigram.subset <- bigram.subset[1:records, ]
                          count.bi <- bigram.subset$discount
                          count.uni <- unigram %>% filter(word == tokens[2])
                          print(count.bi)
                          count.uni <- count.uni$discount
                          prob <- count.bi / count.uni
                          bigram.subset <- bigram.subset %>% mutate(probFinal = prob)
                          return(bigram.subset[,c("last", "probFinal")])
                  }
                  else{
                    unigram.subset <- unigram[,c("word", "prob")]
                    colnames(unigram.subset) <- c("last", "probFinal")
                          return(unigram.subset)
                  }
                  
                }
          
        }
        
}  