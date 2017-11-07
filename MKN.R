#######################################################################
#####  For simplicity the term frequency table is loaded from KBO #####
#######################################################################
# unigram <- readRDS("1gram.rds")
# bigram <- readRDS("2gram.rds")
# trigram <- readRDS("3gram.rds")
# gtSmooth <- readRDS("gtSmooth.rds")


# mkn.discount <- readRDS("mkn_discount.RDS")
cal.discount <- function(x, n){
        if (x == 0) return(0)
        if (x < 3) return(mkn.discount[x, n + 1])
        if (x >= 3) return(mkn.discount[4, n + 1])
}
##### to calculate P_continuation(the)
##### fisrt


###################
####################
############ bigram order
get2gram.prob <- function(input){
  # print(input)
        ##### bigram order
        tokens <- strsplit(input, " ")[[1]]
        firstTerm <-  tokens[1]
        lastTerm <- tokens[2]
        ### find the matching bigram
        bi.match <- bigram %>% filter(pre == firstTerm) %>%
                filter(last == lastTerm)
        discount <- 0
        bi.count <- 0
        if (nrow(bi.match) > 0){
                discount <- cal.discount(bi.match$n, 2)
                bi.count <- max(bi.match$n - cal.discount(bi.match$n, 2), 0)
        }
        ##### calculate the count of the lower order term 
        ##### to calculate the first term in the probability
        uni.set <- bigram %>% filter(pre == firstTerm)
        if (nrow(uni.set) > 0) {
                prob_0 <- bi.count / sum(uni.set$n)
        } else {
                prob_0 <- 0
        }

        ##### the second term
        ##### unique preceeding words
        bi.subset <- bigram %>% filter(last == lastTerm)
        uni.cont.num <- nrow(bi.subset)
        #####
        uni.cont.deno <-  nrow(bigram)
        uni.cont <- uni.cont.num / uni.cont.deno
        
        lambda.uni <- discount / sum(uni.set$n) * nrow(uni.set)
        
        prob.bi <- prob_0 + lambda.uni * uni.cont
        
        if (prob.bi == 0) prob.bi <- 0.01758266 / sum(unigram$n)  ## got from gtSmooth
        return(prob.bi)
}

get3gram.prob <- function(input){
  # print("this is 3gram")
  # print(input)
        ##### trigram order
        tokens <- strsplit(input, " ")[[1]]
        lowerTerm <- paste(tokens[2], tokens[3], collapse = " ")
        # print(lowerTerm)
        prob.bi <- get2gram.prob(lowerTerm)
        
        firstTwoTerm <-  paste(tokens[1], tokens[2], collapse = " ")
        lastTerm <- tokens[3]
        
        tri.match <- trigram %>% filter(pre == firstTwoTerm) %>%
                filter(last == lastTerm)
        discount <- 0
        tri.count <- 0
        if (nrow(tri.match) > 0){
                discount <- cal.discount(tri.match$n, 2)
                tri.count <- max(tri.match$n - cal.discount(tri.match$n, 2), 0)
        }
        
        bi.set <- trigram %>% filter(pre == firstTwoTerm)
        if (nrow(bi.set) > 0) {
                count.bi <- sum(bi.set$n)
                prob_1 <- tri.count / sum(bi.set$n)
        } else {
                prob_1 <- 0
                count.bi <- 0
        }
        
        lambda.bi <- discount / sum(bi.set$n) * nrow(bi.set)
        prob.tri <- prob_1 + lambda.bi * prob.bi
        if (prob.tri == 0) prob.tri <- .2544237 / sum(bigram$n)  ## got from gtSmooth
        return(prob.tri)
}





