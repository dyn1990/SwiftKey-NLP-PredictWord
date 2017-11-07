###################################################
##### calulate the accuracy of the two models #####
###################################################

set.seed(1111)
test.trigram2 <- sample_n(test.trigram, 3e4)

out <- test.trigram2 %>% mutate(predict = "") %>%
        mutate(last = " ")
t <- Sys.time()
for (i in 1:nrow(test.trigram2)){
        text <- test.trigram2$word[i]
        text <- strsplit(text, " ")[[1]]
        firstTwoTerm <- paste(text[1], text[2], collapse = " ")
        lastTerm <- text[3]
        out$last[i] <- lastTerm
        out$predict[i] <- mkn.predict(firstTwoTerm)
        if(i %% 100 == 0) {
                print(sprintf("I am at %f", i))
                print(sprintf("time has elapsed per 100 = %f", Sys.time() - t))
                t <- Sys.time()
        }
}
out <- out %>% arrange(desc(n))
index <- out$predict == out$last
accuracy <- sum(out$n[index]) / sum(out$n)
saveRDS(out, "mkn_result.RDS")




out.kbo <- test.trigram2 %>% mutate(predict = "") %>%
        mutate(last = " ")
t <- Sys.time()
for (i in 1:nrow(test.trigram2)){
        text <- test.trigram2$word[i]
        text <- strsplit(text, " ")[[1]]
        firstTwoTerm <- paste(text[1], text[2], collapse = " ")
        lastTerm <- text[3]
        out.kbo$last[i] <- lastTerm
        out.kbo$predict[i] <- predict.word(firstTwoTerm)
        if(i %% 100 == 0) {
                print(sprintf("I am at %f", i))
                print(sprintf("time has elapsed per 100 = %f", Sys.time() - t))
                t <- Sys.time()
        }
}
out.kbo <- out.kbo %>% arrange(desc(n))
index <- out.kbo$predict == out.kbo$last
accuracy.kbo <- sum(out.kbo$n[index]) / sum(out.kbo$n)
saveRDS(out.kbo, "kbo_result.RDS")