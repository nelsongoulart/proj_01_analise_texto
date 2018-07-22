visualizeWordcloud <- function(term, freq, title = "", min.freq = 25, max.words = 100){
  mypal <- brewer.pal(8, "Dark2")
  wordcloud(words = term, 
            freq = freq, 
            colors = mypal, 
            scale = c(8, .3), 
            rot.per = .15, 
            min.freq = min.freq, 
            max.words = max.words, 
            random.order = F)
}

book_filter1 <- words_books_bb %>% 
  filter(title=="The Prince")
visualizeWordcloud(term = book_filter1$word, freq = book_filter1$n)

book_filter2 <- words_books_bb %>% 
  filter(title=="Leviathan")
visualizeWordcloud(term = book_filter2$word, freq = book_filter2$n)

book_filter3 <- words_books_bb %>% 
  filter(title=="Second Treatise of Government")
visualizeWordcloud(term = book_filter3$word, freq = book_filter3$n)

book_filter4 <- words_books_bb %>% 
  filter(title=="The Social Contract & Discourses")
visualizeWordcloud(term = book_filter4$word, freq = book_filter4$n)

###

p1 <- tidy_books %>% 
  filter(author == "Machiavelli, Niccolò") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  #filter(n > 45) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, 
             n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()

p2 <- tidy_books %>% 
  filter(author == "Hobbes, Thomas") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  #filter(n >= 247) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, 
             n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()

p3 <- tidy_books %>% 
  filter(author == "Locke, John") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  #filter(n >= 85) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, 
             n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()

p4 <- tidy_books %>% 
  filter(author == "Rousseau, Jean-Jacques") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  #filter(n >= 10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, 
             n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()

pppp <- cbind(p1, p2, p3, p4)