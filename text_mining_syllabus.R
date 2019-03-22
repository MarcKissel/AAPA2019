
library(tm)
library(tidyverse)
library(tidytext)

#read in data
syllabus_files <- list.files(pattern = "pdf$")
R_pdf <- readPDF(control = list(text = "-layout"))

word_list <- Corpus(URISource(syllabus_files), 
                   readerControl = list(reader = R_pdf))
word_list_tdm <- TermDocumentMatrix(word_list, control = list(removePunctuation = TRUE,
                                                            stopwords = TRUE,
                                                      tolower = TRUE,
                                                            stemming = TRUE,
                                                            removeNumbers = TRUE,
                                                            bounds = list(global = c(3, Inf)))) 


####tabel 2
#see where the top words are found within each syllabus
top_words <- findFreqTerms(word_list_tdm, lowfreq = 100, highfreq = Inf)
inspect(word_list_tdm[top_words,])

top_words_tdm <- inspect(word_list_tdm[top_words,])


#figure 1

all_words <- rownames(as.matrix(word_list_tdm))
length(all_words)

word_sorted <- order(all_words,decreasing=TRUE)
all_words[head(word_sorted)]
all_words[tail(word_sorted)]


findFreqTerms(word_list_tdm,lowfreq=80)

wf=data.frame(term=names(all_words),occurrences=all_words)

p <- ggplot(subset(wf, all_words>100), aes(term, occurrences))
p <- p + geom_bar(stat= "identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

p <- ggplot(subset(wf, all_words>100), aes(fct_reorder(term, occurrences), occurrences))
p <- p + geom_bar(stat= "identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
p + labs(x= "# of occurrences")


#####table_1


all_words <- rownames(as.matrix(word_list_tdm))

library(tidytext)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

a <- as.tibble(all_words)
a <- a %>% rename(word = value)

a %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)


#not stemmed
opinions_ns.tdm <- TermDocumentMatrix(opinions, control = list(removePunctuation = TRUE,
stopwords = TRUE,
tolower = TRUE,
stemming = FALSE,
removeNumbers = TRUE,
bounds = list(global = c(3, Inf))))

all_words_ns <- rownames(as.matrix(opinions_ns.tdm))

ns <- as.tibble(all_words_ns)
ns <- ns %>% rename(word = value)


ns %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)


#assocaiton
#####

  
findAssocs(word_list_tdm, "will", 0.8)




