
if (!require("pacman")) install.packages("pacman");library(pacman)
pacman::p_load(tidyverse, vosonSML, tidytext, topicmodels, SnowballC, igraph, RColorBrewer, wordcloud 
               , stringr, janitor, ggplot2, dplyr, fastDummies, reshape2, stopwords, ichimoku, tm)


#exercise 1

reddit <- Authenticate("reddit") |>
  Collect(threadUrls = "https://www.reddit.com/r/AskMen/comments/129d2xs/why_do_society_act_as_if_if_youre_not_successful/", verbose = TRUE )

reddit |> head(5)

reddit |> glimpse()

#exercise 2

data = mutate(reddit, thread=str_split_i(structure, "_", 1))

data = select(data, all_of(c("comment_score","comment","user","structure","thread")))

data = mutate(data, clean_comment = str_remove_all(comment, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")) |>
  filter(!is.na(clean_comment))

#exercise 3

#all words
aw = data |>
  unnest_tokens(word, clean_comment, token="words", to_lower=TRUE,) |> 
  mutate(word=str_extract(word, "[a-z']+")) |>                       
  filter(!is.na(word)) 


aw2 = aw |> count(word, sort = TRUE) 
str(aw2)


ggplot(aw2, aes(word,n))+
  geom_point()


#threads
wbt = aw |> count(thread, word, sort = TRUE) 
str(wbt)


#total words per thread
tw = group_by(aw, thread) |> count(sort = TRUE)


#highest and lowest counts of words per thread
head(tw, n=10)
tail(tw, n=10)

#average number of words per thread
#mean = mean(tw$n)
cat("Average amount of words per thread:", mean(tw$n))

#median number of words per thread
#median = median(tw$n)
cat("Median number of words:", median(tw$n))


#exercise 4

#number of threads
N = wbt$thread |> n_distinct()

#number of threads that a word appears in (doc_words)
dw = wbt |> count(word, sort = TRUE)

#create df with term frequency-inverse document frequency
df = wbt |> 
  left_join(tw |> rename(total=n), by = "thread") |>
  left_join(dw |> rename (nd=n), by = "word")|>
  mutate(tf = n/total, idf = log(N/nd), tf_idf = tf*idf) |>
  arrange(-tf_idf, thread)

tail(df)

#tfidf function
tfidf = wbt |>
  bind_tf_idf(word, thread, n) |>
  arrange(-tf_idf, thread)

tail(tfidf)


#exercise 5

#document term matrix
dtm = cast_dtm(tfidf, thread, word, n)
print(dtm)

#calculate storage size of different objects

#document-term matrix
object.size(dtm)

#tidy-format
object.size(select(tfidf, thread, word, n))

#full data frame
object.size(select(tfidf, thread, word, n) |> pivot_wider(names_from = word, values_from = n, names_prefix = "_"))

#full data-matrix
object.size(select(tfidf, thread, word, n) |> pivot_wider(names_from = word, values_from = n, names_prefix = "_") |> data.matrix())




#number of topics
topics = 8

#lda moddeling
top_lda = LDA(dtm, k = topics, method = "gibbs", control = list(seed = 1234))

#word distribution in topics
wt = tidy(top_lda, matrix = "beta")
head(wt, n=10)

#topic distribution in documents
td = tidy(top_lda, matrix = "gamma")
head(td, n=10)

#visualise topic words in wordcloud
top40 <- wt |> filter(topic==1) |> slice_max(beta, n=40)

mycolors <- brewer.pal(8, "Dark2")

wordcloud(top40$term, top40$beta, random.order = FALSE, color = mycolors)





#removal stop words
sw = tibble(word=c('and', 'but', 'most', 'gets', 'to', 'you', 'and', 'the', 'i', 'in', 'a', 'of', 's', 'your', 's', 'your', 'is', 'that', 'are', 'my' ))

aw = aw |>
  anti_join(stop_words, by="word") |>
  anti_join(get_stopwords(source = "stopwords-iso"), by = "word") |>
  anti_join(sw, by = "word")




#aw = aw |> 
  #filter(!word %in% stop_words$word) |>
  #filter(!word %in% sw$word) 







#stemming
aw = mutate(aw, stem = wordStem(word))


#recode previous parts but with new aw
aw_new = aw[, c("thread","word","stem")]




tfidf2 = aw_new |> count(thread, word, sort = TRUE)  |>
  bind_tf_idf(word, thread, n) |>
  arrange(-tf_idf, thread)


dtm2 = cast_dtm(tfidf2, thread, word, n)


lda = LDA(dtm2, k = 10, method = "gibbs", control = list(seed = 1234))

wt_new = tidy(lda, matrix = "beta")

#testing for words to add in the anti_join
count = aw_new |> count(word)


check = 20

table = read.table(text=gsub("(?<=[a-z])\\s+", "\n", head(sort(count$n, decreasing = TRUE), n=check), perl=TRUE), 
                   header=FALSE, col.names = c("numbers"))

out = lapply(table$numbers, function(s) filter(count, n == s))

output = do.call(rbind, out)

list(output$word)



#wordclouds                 use copy paste this command in the console: list(unique(one$term))    You'll get a string back copy past this string
#into chatgpt with the sentence "what would be a nice title for these words: " in front of it. Then change the titles below. Titles are the one
#the text functions
one = wt_new |> filter(topic==2) |> slice_max(beta, n=40)
two = wt_new |> filter(topic==6) |> slice_max(beta, n=40)
three = wt_new |> filter(topic==4) |> slice_max(beta, n=40)
four = wt_new |> filter(topic==7) |> slice_max(beta, n=40)

#wordcloud 1
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Words on Society, Life, and Conversation")
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
wordcloud(three$term, top40$beta, random.order = FALSE, color = mycolors)

#wordcloud 4
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Reflections on Life, Self, and Achievement")
wordcloud(four$term, top40$beta, random.order = FALSE, color = mycolors)





#distribution of topics within thread
dc = c(73 , 16, 46, 89, 32)

Nt = length(dc)

ggplot(td |> filter(document %in% dc), aes(topic, gamma, fill = document), ylab = "proportion") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ document, ncol = Nt)

#topic 7 threads
topics(top_lda)[topics(top_lda) ==7]

#thread 46 comments
select(filter(data, thread==46), comment)


#exercise 6
mycolors2 <- brewer.pal(9, "YlOrRd")

#plot layouts
layout <- layout_with_kk(actorGraph) #Kamada-Kawai layout
layout1 <- layout_with_graphopt(actorGraph) #Graphopt layout
layout2 <- layout_with_fr(actorGraph) #Fruchterman-Reingold layout



#before 14:00:00 on 2023-04-02
plot_data_before = filter(reddit, !is.na(user) & user!= "[deleted]") |> filter(comm_date<"2023-04-02 14:00:00") |> filter(comment_score > 0)

#create a network (actornetwork)
an_before = plot_data_before |> Create("actor") |> AddText(plot_data_before)

#create a network (actorgraph)
actorGraph_before = an_before |> Graph(writeToFile = TRUE)

#define personal colours
persco_before = add_row(plot_data_before, user = plot_data_before$author[1:1], comment_score = plot_data_before$post_score[1:1]) |>
  group_by(user) |>
  summarize(pop = round(log(sum(comment_score)))+1)|>
  ungroup() |>
  mutate(color = mycolors2[pop]) |>
  arrange(factor(user, levels = V(actorGraph_before)$user))


V(actorGraph_before)$color <- persco_before |> pull(color) 






#after 14:00:00 on 2023-04-02
#remove rows without a proper username and filter by date
plot_data_after = filter(reddit, !is.na(user) & user!= "[deleted]") |> filter(comm_date>"2023-04-02 14:00:00") |> filter(comment_score > 0)

#create a network (actornetwork)
an_after = plot_data_after |> Create("actor") |> AddText(plot_data_after)

#create a network (actorgraph)
actorGraph_after = an_after |> Graph(writeToFile = TRUE)

#define personal colours
persco_after = add_row(plot_data_after, user = plot_data_after$author[1:1], comment_score = plot_data_after$post_score[1:1]) |>
  group_by(user) |>
  summarize(pop = round(log(sum(comment_score)))+1)|>
  ungroup() |>
  mutate(color = mycolors2[pop]) |>
  arrange(factor(user, levels = V(actorGraph_after)$user))


V(actorGraph_after)$color <- persco_after |> pull(color) 



#plots before 14:00:00
plot(actorGraph_before, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout, main = "Kamada-Kawai before")
plot(actorGraph_before, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout1, main = "Graphopt before")
plot(actorGraph_before, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout2, main = "gFruchterman-Reingold before")


#plots after 14:00:00
plot(actorGraph_after, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout, main = "Kamada-Kawai after")
plot(actorGraph_after, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout1, main = "Graphopt after")
plot(actorGraph_after, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout2, main = "Fruchterman-Reingold after")


#plots combined
plot(actorGraph_before, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout, vertex.color = "green")
par(new = TRUE)
plot(actorGraph_after, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout, vertex.color = "red", main = "Kamada-Kawai")


plot(actorGraph_before, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout1, vertex.color = "green")
par(new = TRUE)
plot(actorGraph_after, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout1, vertex.color = "red", main = "Graphopt")


plot(actorGraph_before, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout2, vertex.color = "green")
par(new = TRUE)
plot(actorGraph_after, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5, layout = layout2, vertex.color = "red", main = "Fruchterman-Reingold")


































