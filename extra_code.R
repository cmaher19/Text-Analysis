peterpan1 <- read.csv('peterpan.csv') %>%
  mutate(linenumber=row_number()) %>%
  mutate(text = as.character(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


peterpan1



# Peter Pan example from Gutenberg - create a line number variable, tokenize the data, and remove stop words
peterpan <- gutenberg_download(16)

#peterpan <- read.csv("peterpan.csv") %>%
#mutate(linenumber=row_number(), text = as.character(text)) %>%
#unnest_tokens(word, text) %>%
#anti_join(stop_words) %>%
#select(-X)

peterpan <- peterpan %>%
  mutate(linenumber=row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# in ui
box(plotOutput("plot1")),
box(plotOutput("plot2")),
box(plotOutput("plot3")),
box(plotOutput("plot4")),
box(plotOutput("plot5"))
# in server
output$plot1 <- renderPlot({
  peterpan %>%
    count(word) %>%
    filter(n > 50) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) + geom_col(fill="purple") + coord_flip() + ggtitle('Most Common Words') +
    ylab("Count") + xlab("Word")
})
# Words with the largest contribution according to AFINN lexicon
output$plot2 <- renderPlot({
  peterpan %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(score) %>%
    count(word, sort=TRUE) %>%
    ungroup() %>%
    mutate(word_score = score*n) %>%
    filter(abs(word_score) > 25) %>%
    mutate(word = reorder(word, word_score)) %>%
    ggplot(aes(word, n*score, fill=n*score>0)) + geom_col(show.legend = FALSE) + 
    coord_flip() + ggtitle("Words with largest Sentiment Contribution")
})
# Most commonly occurring positive and negative words according to the bing lexicon
output$plot3 <- renderPlot({
  peterpan %>%
    inner_join(get_sentiments("bing")) %>%
    filter(word != "darling") %>%
    group_by(sentiment) %>%
    count(word, sort=TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill=sentiment)) + geom_col(show.legend = FALSE) + 
    facet_wrap(~sentiment, scales="free_y") + coord_flip() + 
    ggtitle('Most Common Sentiments with Bing Lexicon')
})
# Break book down by chunks of 75 lines and analyze sentiments that way
output$plot4 <- renderPlot({
  peterpan %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(index = linenumber %/% 75) %>%
    summarise(sentiment = sum(score)) %>%
    ggplot(aes(index, sentiment, fill=sentiment > 0)) + geom_col(show.legend = FALSE) + 
    ggtitle("Sentiment Scores every 75 Lines")
})
output$plot5 <- renderPlot({
  pp_afinn <- peterpan %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(index = linenumber %/% 75) %>%
    summarise(sentiment = sum(score)) %>%
    mutate(method = "AFINN")
  
  pp_bing <- peterpan %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing") %>%
    count(method, index = linenumber %/% 75, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  # The lexicons show roughly the same trends
  # Note: remove darling from afinn list
  bind_rows(pp_afinn, pp_bing) %>%
    ggplot(aes(index, sentiment, fill=method)) + geom_col(show.legend = FALSE) + 
    facet_wrap(~method, ncol = 1, scales = "free_y") + ggtitle("Comparing Sentiment Lexicons for Peter Pan")
})
