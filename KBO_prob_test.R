prob.text <- function(phrase){
        tokens <- strsplit(phrase, " ")[[1]]
        firstTwoTerm <-  paste(tokens[1], tokens[2], collapse = " ")
        lastTerm <- tokens[3]
        trigram.subset <- trigram %>% 
                filter(pre == firstTwoTerm, last == lastTerm)
        if (nrow(trigram.subset) > 0){
                # print("we found matching trigram term")
                count.tri <- trigram.subset$discount
                count.bi <- bigram %>% filter(word == firstTwoTerm)
                count.bi <- count.bi$discount
                
                # print(sprintf("the term frequency of the trigram is %f", count.tri))
                # print(sprintf("the term frequency of tge preceeding is %f", count.bi))
                prob <- count.tri / count.bi
                return(prob)
        }
        else{
                # print("this is an unobserved trigram, we need to back off")
                cal.weight <- trigram %>%
                        filter(pre == firstTwoTerm)
                if (nrow(cal.weight) > 0){
                        # print("we found some preceeding bigram")
                        # print("we can use them to calculate the back off weight for bigram")
                        alpha.bi <- cal.alpha(firstTwoTerm)
                        # print(sprintf("weight distributed to bigram is %f", alpha.bi))
                }
                else{
                        # print("we don't even find matching bigrams, need to back off with 1")
                        alpha.bi <- 1
                        # print(sprintf("weight distributed to bigram is %f", alpha.bi))
                }
                # print("done with weight redistribution, let's back off now")
                bigram.subset <- bigram %>% 
                        filter(pre == tokens[2], last == lastTerm)
                if (nrow(bigram.subset) > 0){
                        # print("we found one matching bigram")
                        count.bi <- bigram.subset$discount
                        count.uni <- unigram %>% filter(word == tokens[2])
                        count.uni <- count.uni$discount
                        prob <- alpha.bi * count.bi / count.uni
                        return(prob)
                }
                else{
                        # print("no matching in bigram, we need to back off to unigram")
                        cal.weight <- bigram %>%
                                filter(pre == tokens[2])
                        if (nrow(cal.weight) > 0){
                                # print("we found some preceeding unigram")
                                # print("calculate unigram weight")
                                alpha.uni <- cal.alpha(tokens[2])
                                # print(sprintf("weight distributed to unigram is %f", alpha.uni))
                                
                        }
                        else{
                                # print("we don't even find matching unigrams, need to back off with 1")
                                alpha.uni <- 1
                                # print(sprintf("weight distributed to unigram is %f", alpha.uni))
                                
                        }
                        # print("ok, let's back off again")
                        unigram.subset <- unigram %>% 
                                filter(word == lastTerm)
                        if (nrow(bigram.subset) > 0){
                                # print("we found one matching unigram")
                                prob <- alpha.uni * unigram.subset$prob
                                return(prob)
                        }
                        else{
                                # print("no matching ngrams are found at all, the word is very rare")
                                # print("we have nothing to back off")
                                # print("we return the left over probability mass for unseen unigram")
                                prob <- gtSmooth$uni[1]/sum(unigram$n)
                                return(prob)
                        }
                        
                        
                }
                
        }
}  