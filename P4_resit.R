#Anne


if (!require("pacman")) install.packages("pacman");library(pacman)
pacman::p_load(tidyverse, vosonSML, tidytext, topicmodels, SnowballC, igraph, RColorBrewer, wordcloud 
               , stringr, janitor, ggplot2, dplyr, fastDummies, reshape2, stopwords, ichimoku, tm, textdata)


#Q1

reddit <- Authenticate("reddit") |>
  Collect(threadUrls = "https://www.reddit.com/r/news/comments/vjpfbh/supreme_court_overturns_roe_v_wade_states_can_ban/", verbose = TRUE )

#Q2

data = mutate(reddit, thread=str_split_i(structure, "_", 1))

#data = select(data, all_of(c("comment_score","comment","user","structure","thread")))

data = mutate(data, clean_comment = str_remove_all(comment, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")) |>
  filter(!is.na(clean_comment))

#all words
aw = data |>
  unnest_tokens(word, clean_comment, token="words", to_lower=TRUE,) |> 
  mutate(word=str_extract(word, "[a-z']+")) |>                       
  filter(!is.na(word)) 

#removal stop words
aw = aw |>
  anti_join(stop_words, by="word") |>
  anti_join(get_stopwords(source = "stopwords-iso"), by = "word")

aw = filter(aw, user!= "[removed]" & user!= "[deleted]")

aw2 = aw |> count(word, sort = TRUE) 
str(aw2)

#Q4
#create tidy dataframe words_by_comment
wbc = aw |> count(id, comment, word, sort = TRUE) 


#Q5
#calculate sentiment comment_sentiment
cs <- wbc |>
  inner_join(get_sentiments("afinn"), 	
             by=join_by(word))|>
  group_by(id)|>  
  summarise(sentiment=sum(value*n))


#Q6
tfidf = wbc |>
  bind_tf_idf(word, id, n) |>
  arrange(-tf_idf, d)


#Q7
#topic moddeling
#number of topics
topics = 8

#lda moddeling
top_lda = LDA(dtm, k = topics, method = "gibbs", control = list(seed = 1234))

#word distribution in topics
wt = tidy(top_lda, matrix = "beta")


#wordclouds  
one = wt |> filter(topic==2) |> slice_max(beta, n=40)
two = wt |> filter(topic==6) |> slice_max(beta, n=40)
three = wt |> filter(topic==4) |> slice_max(beta, n=40)
four = wt |> filter(topic==7) |> slice_max(beta, n=40)


#color coding
mycolors <- brewer.pal(8, "Dark2")


#wordcloud 1
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Word Salad: An eclectic mix of words about society, success, and life")
wordcloud(one$term, one$beta, random.order = FALSE, color = mycolors, main = "Title")

#wordcloud 2
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Reflections on Life, Success, and Relationships")
wordcloud(two$term, two$beta, random.order = FALSE, color = mycolors)

#wordcloud 3
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Navigating Life's Challenges and Choices")
wordcloud(three$term, three$beta, random.order = FALSE, color = mycolors)

#wordcloud 4
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Reflections on Life, Self, and Achievement")
wordcloud(four$term, four$beta, random.order = FALSE, color = mycolors)













