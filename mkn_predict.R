##############################################################
##### predict the next word based on modified Kneser Ney #####
##############################################################

cal.discount <- function(x, n){
        if (x == 0) return(0)
        if (x < 3) return(mkn.discount[x, n + 1])
        if (x >= 3) return(mkn.discount[4, n + 1])
}

mkn.predict <- function(phrase){
        tokens <- strsplit(phrase, " ")[[1]]
        if (length(tokens) == 1){
                bigram.subset <- bigram %>% 
                        filter(pre == tokens[1])
                if (nrow(bigram.subset) > 0){
                        records <- min(nrow(bigram.subset), 3)
                        finalProb <-  mapply(get2gram.prob, bigram.subset$word[1:records])
                        bigram.subset <- bigram.subset[1:records, ] %>%
                                mutate(prob = finalProb) %>% 
                                arrange(desc(prob)) %>%
                                select(c("last", "prob"))
                  return(bigram.subset[1:3, ])
                }
                else{
                  unigram.subset <- unigram[,c("word", "prob")]
                  colnames(unigram.subset) <- c("last", "probFinal")
                  return(unigram.subset[1:3,])
                }
                
        }
        if (length(tokens) >= 2){
                tokens <- c(tokens[length(tokens) - 1], tokens[length(tokens)]) 
                trigram.subset <- trigram %>% 
                        filter(pre == paste(tokens[1], tokens[2], collapse = " "))
                if (nrow(trigram.subset) > 0){
                        records <- min(nrow(trigram.subset), 3)
                        finalProb <-  mapply(get3gram.prob, trigram.subset$word[1:records])
                        trigram.subset <- trigram.subset[1:records, ] %>%
                          mutate(prob = finalProb) %>% 
                          arrange(desc(prob)) %>%
                          select(c("last", "prob"))
                        return(trigram.subset[1:3, ])
                }
                else{
                        bigram.subset <- bigram %>% 
                                filter(pre == tokens[2])
                        if (nrow(bigram.subset) > 0){
                                records <- min(nrow(bigram.subset), 3)
                                finalProb <-  mapply(get2gram.prob, bigram.subset$word[1:records])
                                bigram.subset <- bigram.subset[1:records, ] %>%
                                        mutate(prob = finalProb) %>% 
                                        arrange(desc(prob)) %>%
                                        select(c("last", "prob"))
                                return(bigram.subset[1:3, ])
                        }
                        else{
                          unigram.subset <- unigram[,c("word", "prob")]
                          colnames(unigram.subset) <- c("last", "prob")
                          return(unigram.subset[1:3,])
                        }
                        
                }
                
        }
}



