library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(widyr)
library(tidyr)
library(stringr)
library(scales)
library(twitteR)
library(tm)
library(ggplot2)
library(ggplot)
library(igraph)
library(ggraph)
library(reshape2)
library(wordcloud)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'fXZKq3cBuzf0SxNF3HtBhS1QP'
consumer_secret <- 'WGU570efXI1mqEewnO11ayK1VoiAcUUIXEOWGGtHq5FHid5xOi'
access_token <- '1217533548321619968-WS9uPTcaPEmusN7DUi3tlXqo9UREfG'
access_secret <- 'gtpnH4Ea9fC0DC8A4GgZ0BlrbqgHKb1dPQBOa43X4O9Fl'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# select 2 : No

###################################
#pull 3 Twitter datasets, #########
#associated with one common theme : plant-based meat #####
plantbased <- twitteR::searchTwitter('#plantbased', n = 1000, since = '2015-01-01',retryOnRateLimit = 1e3)
pb = twitteR::twListToDF(plantbased)

vegetarian <-twitteR::searchTwitter('#vegetarian', n = 1000, since = '2015-01-01', retryOnRateLimit = 1e3)
veg = twitteR::twListToDF(vegetarian)

protein <- twitteR::searchTwitter('#protein', n = 1000, since = '2015-01-01', retryOnRateLimit = 1e3)
prtn = twitteR::twListToDF(protein)

#View(pb)

# create my own stop word library
# print(stop_words) 
# stop_words is dataframe with two variable, word and lexicon
cust_stop <- data_frame(word = c("http", "https", "rt", "t.co", "amp" ,"h", "a", "q", "b", "c", "n", "w", "o", "f", "g", "i", "m", "d", "u", "th", "aber", "it", "t", "al", "el"), lexicon = rep("cust", each =25))

# tokenize, rmv stop words, count
tidy_prtn <- prtn %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) #%>%
#  count(word, sort = T)

#plantbased df
tidy_pb <- pb %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) #%>%
#  count(word, sort = T)

#veg df
tidy_veg <- veg %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) #%>%
#  count(word, sort = T)

####We want to combine all the datasets and do frequencies 
# correllation is the best framework to compare
frequency_twitter <- bind_rows(mutate(tidy_veg, author = "vegetarian"),
                               mutate(tidy_pb, author = "plantbased"),
                               mutate(tidy_prtn, author = "protein")
) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `protein`, `vegetarian`)

#correlograms
ggplot(frequency_twitter, aes(x=proportion, y=`plantbased`, 
                              color = abs(`plantbased`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "plantbased", x=NULL)

# interpretation: unless finding what is similar, we want to explore dissimilarities, to differentiate 
# the two works, which not on the diagonal. using text to classify new customers, per customer. 
# compare how multiple groups people talking
cor.test(data=frequency_twitter[frequency_twitter$author == "vegetarian",],
         ~proportion + `plantbased`)

cor.test(data=frequency_twitter[frequency_twitter$author == "protein",],
         ~proportion + `plantbased`)

######plotting the token frequencies:
freq_hist_pb <-tidy_janeausten_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n > 600) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)
# long tail dist, not normal

#cast dtm on pb
tidy_pb <- pb %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word, sort = T) %>%
  cast_dtm(id, word, n)

tidy_prtn <- prtn %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word, sort = T) %>%
  cast_dtm(id, word, n)

tidy_veg <- veg %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word, sort = T) %>%
  cast_dtm(id, word, n)

### Sentiment wordclounds for twitter ############
##### Lets take a look at the lexicons one by one #######

afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')
bing <- get_sentiments('bing')

tidy_usa <- d %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_usa #look at trump - he is positive!!! :)

tidy_usa %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#we need to use the NRC sentiments
tidy_usa %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="nn", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)#higher lambda - lower rate of decrease, which is the positive plot


#we like binary in data science, but afinn (1-5) offers more biz insights

#created 3 dataframe, 3 lexicon, now combine tgt as a union.
#combine by rows, rowbind
sentiments <- bind_rows(mutate(afinn, lexicon='afinn'),
                        mutate(nrc,lexicon='nrc'),
                        mutate(bing,lexicon='bing')
)
sentiments %>%
  filter(lexicon=='nrc')

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") #what is your sentiment

###############
emma_book <- original_books %>%
  filter(book == "Emma") #which book did you select?

afinn <- emma_book %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index=lienumber %/% 80) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  emma_book%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  emma_book %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index=lienumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

####### TF-IDF framework in Jane Austen's work #######

#let's look at the data
original_books <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort=TRUE) %>%
  ungroup()

total_words <- original_books %>%
  group_by(book) %>%
  summarize(total=sum(n))

book_words <- left_join(original_books, total_words)

print(book_words)

ggplot(book_words, aes(n/total, fill = book))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~book, ncol=2, scales="free_y")
#what do the tails represent? 
#answer: exremely common words! we are really interested in the not so common words. 

########## ZIPF's law ################

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=book))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

################# TF_IDF ##########################

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words # we get all the zeors because we are looking at stop words ... too common

book_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

# looking at the graphical apprach:
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=book))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~book, ncol=2, scales="free")+
  coord_flip()

######## Most common positive and negative words #############

bing_counts <- original_books %>%
  filter(book == "Emma") %>% #which book did you select?
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#higher lambda - lower rate of decrease, which is the positive plot

###Pairwise correlations between words #########
my_tidy_df <- austen_books() %>%
  filter(book == "xxxxxxxx") %>% #what book do you want to use? Emma?
  mutate(section = row_number() %/% 80) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

my_tidy_df
#taking out the least common words
word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= xxxxxxxxxx) %>% #what is the minimum frequency you want to use
  pairwise_cor(word,section, sort=TRUE)
#pairwise_cor() check correlation based on how ofter words appear in the same section

word_cors %>%
  filter(item1 == "jane")

####### creating barcharts for correlatoins ############

word_cors %>%
  filter(item1 %in% c(xxxxxxxxxxxxxxxx)) %>% #which words do you want to use?
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

####### creating a correlation network #################

#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation > xxxxxxxx) %>% #what correlation cutoff do you want to use
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "xxxxxxxxxxxxx", size=xxxxxxxx)+ #what color do you want to use?
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE))))%>%
  ungroup() %>%
  unnest_tokens(word, text)
#########
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

austen_bigrams #We want to see the bigrams (words that appear together, "pairs")

austen_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
quadrogram <- austen_books() %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram

###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
#quadro gram compy heacy
#tfidf insight rich
#bygram makes more sense than individual words
bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

##### lets do the same for a quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(book, quadrogram) %>%
  bind_tf_idf(quadrogram, book, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

######## visualising negated words ###################
###### negated words in sentiment analysis ###########

negation_tokens <- c('no','never','without','not')#what negation tokens do you want to use?

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words

#### we can visuals the negated words ###########
#we'll create a function to plot the negations###
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}
#closing the negated_words_plot function

negated_words_plot(x="not") #this is your first negation word
negated_words_plot(x="no") #this is your second negation word
negated_words_plot(x="without") #this is your third negation word

####### VISUALISING A BIGRAM NETWORK #################

bigram_graph <- bigram_counts %>%
  filter(n>20) %>%   #lowr to 2 or 3
  graph_from_data_frame()

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#######piping basics
#mean of horse power and disp for gear 3,4,5
mtcars %>%
  group_by(gear) %>%
  summarise(Avg_hp = mean(hp)) %>%
  arrange(desc(Avg_hp))

my_func <- function(x) {
  my_mean <- mean(x)
  my_sd <- sd(x)
  return(c(my_mean,my_sd))
}
my_func(mtcars$hp)
