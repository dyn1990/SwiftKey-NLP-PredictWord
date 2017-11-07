#####################################
##### build the frequency table #####
#####################################

################################
##### Good Turing Discount #####
################################

gt.discount <- function(x, ngram){
  if (x < 3){
    return(gtSmooth[[ngram]][x + 1])
  }
  else{
    return(x)
  }
}



#######################################################################
##### generate ngram table with GT smooth and Katz back off weight#####
######################################################################

gtSmooth <- data.frame(count = seq(0, 6, 1),
                       uni = rep(0, 7),
                       bi  = rep(0, 7),
                       tri = rep(0, 7))

unigram = Tokenization(cText, n_gram = 1)
unigram <- countWord(unigram) 
for (i in seq(0, 6, 1)){
      if (i == 0) {gtSmooth$uni[i+1] <- sum(unigram$n == 1)/sum(unigram$n)}
      else{
        c.ratio <- sum(unigram$n == (i + 1)) / sum(unigram$n == i)
        gtSmooth$uni[i+1] <- (i + 1) * c.ratio
      }
      
}

bigram = Tokenization(cText, n_gram = 2)
bigram <- countWord(bigram) %>%
          mutate(wordtoseprate = word) %>%
          separate(wordtoseprate, c("pre", "last"), sep = " ")
for (i in seq(0, 6, 1)){
  if (i == 0) {gtSmooth$bi[i+1] <- sum(bigram$n == 1)/sum(bigram$n)}
  else{
    c.ratio <- sum(bigram$n == (i + 1)) / sum(bigram$n == i)
    gtSmooth$bi[i+1] <- (i + 1) * c.ratio
  }
  
}


trigram = Tokenization(cText, n_gram = 3)
trigram <- countWord(trigram) %>%
          mutate(wordtoseprate = word) %>%
          separate(wordtoseprate, sprintf("word-%d", seq(3)), sep = " ") %>%
          unite("pre", "word-1", "word-2", sep = " ")
colnames(trigram)[4] <- "last"
for (i in seq(0, 6, 1)){
  if (i == 0) {gtSmooth$tri[i+1] <- sum(trigram$n == 1)/sum(trigram$n)}
  else{
    c.ratio <- sum(trigram$n == (i + 1)) / sum(trigram$n == i)
    gtSmooth$tri[i+1] <- (i + 1) * c.ratio
  }
  
}


##### Katz Back Off Left Over Prob #####
##### beta = 1 - sum(c*/c)         ##### 
##### c* is # of discounted ngram  #####
##### c is the preceding (n-1)gram #####

cal.beta <- function(ngram){
  number <- length(strsplit(ngram$pre[1], split = " ")[[1]])
  if(number < 1){
    ngram <- ngram %>%
      mutate(discount = mapply(FUN = gt.discount, ngram$n, ngram = number + 2)) %>%
      mutate(leftProb = 1 - sum(discount) / sum(n))
  }
  if(number >= 1){
    ngram <- ngram %>%
      mutate(discount = mapply(FUN = gt.discount, ngram$n, ngram = number + 2)) %>%
      group_by(pre) %>% 
      mutate(leftProb = 1 - sum(discount) / sum(n))
  }
  return(ngram)
}


trigram <- trigram %>% cal.beta() %>% ungroup
bigram <- bigram %>% cal.beta()  %>% ungroup 
unigram <- unigram %>%
mutate(pre = word) %>%
cal.beta() %>% ungroup %>%
mutate(prob =  discount / sum(n))
  
#####

cal.alpha <- function(phrase){
  tokens <- strsplit(phrase, split = " ")[[1]]
  if(length(tokens) == 2){
    beta <- trigram %>%
      filter(pre == phrase)
    beta <- beta$leftProb[1]
    denom <- bigram %>%
      filter(pre == tokens[2])
    alpha <- beta / denom$leftProb[1]
    # print(denom$leftProb[1])
  }
  if(length(tokens) == 1){
    alpha <- bigram %>%
      filter(pre == phrase)
    alpha <- alpha$leftProb[1]
  }
  return(alpha)
}
######

trigram <- trigram %>% 
  mutate(alpha =  mapply(cal.alpha, phrase = trigram$pre))
bigram <-bigram %>% 
  mutate(alpha =  mapply(cal.alpha, bigram$pre))

unigram <- select(unigram, c("n", "pre", "discount", "prob"))
bigram <- select(bigram, c("n", "pre", "last", "discount", "alpha"))
trigram <- select(trigram, c("n", "pre", "last", "discount", "alpha"))
  
saveRDS(unigram, file="1gram.rds")
saveRDS(bigram, file="2gram.rds")
saveRDS(trigram, file="3gram.rds")
saveRDS(gtSmooth, file="gtSmooth.rds")
