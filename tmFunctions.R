####################################################
##### Some useful functions for data cleaning  #####
####################################################


##### Clean Data Corpus
cleanText <- function(x, rs = FALSE){
  x[, 1] = gsub("[^[:ascii:]]", "", x[, 1], perl = TRUE)
  x[, 1] = gsub("[^[:alnum:][:space:]']", "", x[, 1])
  
  x = Corpus(DataframeSource(x))
  x = tm_map(x, tolower)
  x = tm_map(x, removeNumbers)
  x = tm_map(x, stripWhitespace)
  x = data.frame(text = matrix(unlist(x), byrow = TRUE),
                 stringsAsFactors = FALSE) %>%
    mutate(textID = 1:length(text)) %>%
    arrange(textID) %>%
    as_tibble()
}

##### Clean word input for prediction
cleanInput <- function(x, rs = FALSE){
  x = gsub("[^[:ascii:]]", "", x, perl = TRUE)
  x = gsub("[^[:alnum:][:space:]']", "", x)
  x = Corpus(VectorSource(x))
  x = tm_map(x, tolower)
  x = tm_map(x, removeNumbers)
  x = tm_map(x, stripWhitespace)
  x = data.frame(text = matrix(unlist(x)[1], byrow = TRUE),
                 stringsAsFactors = FALSE) %>%
    as_tibble()
  return(x$text)
}


##### count number of lines of a corpus
countLine <- function(x){
        lineNumber = x %>% summarize(`number of lines`= n())
}

##### word tokenization
Tokenization <- function(x, text, n_gram = 1){
        if (n_gram == 1){
                x = x %>% unnest_tokens(word, text)
        }
        else if (n_gram > 1){
                x = x %>% unnest_tokens(word, text, token = "ngrams", n = n_gram) %>%
                        mutate(wordtoseprate = word) %>%
                        separate(wordtoseprate, sprintf("word-%d", seq(n_gram)), sep = " ")
        }
        
}

##### count number of word given a data frame
countWord <- function(x){
        x = x %>% 
                count(word, sort = TRUE)
}

##### calculate the number of words covers the corpus instances
wordCoverage <- function(x, ratio){
        return(sum(cumsum(x$n) <= ratio * sum(x$n)))
}

##### bar plot
plot_tf <- function (data, term, freq, title){
        
        column = deparse(substitute(term))
        data[, column] = factor(data[[column]], levels = rev(data[[column]]))
        term = eval(substitute(term), data, parent.frame())
        freq = eval(substitute(freq), data, parent.frame())
        g <- ggplot(data = data, aes(x = term,
                                     y = freq )) +
                geom_col(show.legend = TRUE, width = 0.8) + xlab("word") + ylab("frequency") +
                theme(axis.text.y = element_text(angle=65)) + 
                labs(title = title) + coord_flip()
}


