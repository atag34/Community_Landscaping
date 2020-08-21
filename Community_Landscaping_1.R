library(httr)
library(rvest)
library(curl)
library(tm)
library(quanteda)
library(plotly)
library(topicmodels)

results <- read.csv("results-20200806-140844.csv", stringsAsFactors = F)


g_dat <- as.data.frame(
  table(
    tolower(
      gsub(
        pattern = ",\\d+$",
        replacement = "",
        x = unlist(
          strsplit(x=c(results$Organizations),
                   split=";")),
        perl = T
      )
    )
  )
)

g_dat$links <- lapply(g_dat$Var1, FUN = function(x){results$DocumentIdentifier[which(grepl(x,results$Organizations))]})

# Remove organizations that come up less than 10 times, this may be too restrictive
g_dat <- g_dat[g_dat$Freq>=10,]


g_dat$webtext <- NA



remove_orgs <- c('united states',
                 'facebook',
                 'cnn',
                 'twitter',
                 'associated press',
                 'instagram',
                 'white house',
                 'international monetary fund',
                 'youtube',
                 'new york times',
                 'google',
                 'reuters',
                 'washington post',
                 'olympics',
                 'linkedin')

g_dat <- g_dat[!g_dat$Var1 %in% remove_orgs, ]

g_dat$webtext <- NA

for (i in which(is.na(g_dat$webtext))){
  org_text <- list()
    
    for (k in 1:length(g_dat$links[[i]])){
      tryCatch({
    page <- read_html(GET(g_dat$links[[i]][k],timeout(5)))
    # or this for the text under the p nodes
    p_text<-page%>% html_nodes("p") %>% html_text()
    org_text[[k]] <- p_text
    message(k,' of ',length(g_dat$links[[i]]),' for org ',i,' of ',nrow(g_dat),' ',paste(as.character(g_dat$Var1[i])))
    #Sys.sleep(1)
    closeAllConnections()
  },error = function(e){})
    }
g_dat$webtext[i] <- paste0(unlist(org_text),collapse =" ")  
}


rownames(g_dat) <- g_dat$Var1

write.csv(g_dat[,-3],"web_text.csv")
g_test <- read.csv("web_text.csv", row.names = "X")

web_corp <- corpus(g_dat[,c("Freq","webtext")], text_field = "webtext")

# Use single word tokens for now and clean up the corpus for making a DTM/DFM (DFM relevant for quanteda package)
token <- tokens(
            web_corp,
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_numbers = TRUE,
            remove_url = TRUE,
            include_docvars = TRUE
        )

web_dtm_one <- dfm(token,
                   tolower= TRUE,
                   remove=stopwords('english')
                )

# filter terms that occur in less than 5% of org news and more than 90% of org news to try and elminiate random sparse ness and or
# irrelevant words to understanding similarity. Somewhat arbitrary choic
web_dtm_one_filt  <- dfm_trim(web_dtm_one,
                         min_docfreq = 0.05,
                         max_docfreq = 0.90,
                         docfreq_type = "prop"
            )

head(web_dtm_one_filt)
# Document-feature matrix of: 6 documents, 7,878 features (87.9% sparse) and 1 docvar.
# features
# docs                                     women's radio network internet women people dan hill president logic
#   afghan women association international     187   162      69       23   219     78   3    1        25     1
#   albertsons                                   0     0       0        0     0     45   0    0         0     0
#   alcan                                        0     0       2        0     1     33   0    0         2     0
#   america open technology institute            0     0       0       14     0     42   0   14        42     0
#   american civil liberties union               7     1       2        0    15     46   0    1         4     0
#   american community survey                    0     0       0        0     0     33   0    0         2     0

# I was thinking stemming might not be necessary but seeing already that women's and women is showing up here we may have to.

web_dtm_stem <- dfm(token,
                   tolower= TRUE,
                   remove=stopwords('english'),
                   stem = TRUE
)


web_dtm_stem_filt  <- dfm_trim(web_dtm_stem,
                              min_docfreq = 0.20,
                              max_docfreq = 0.80,
                              docfreq_type = "prop"
                              
)
head(web_dtm_stem_filt)
# docs                                     women radio network internet peopl dan hill presid logic inc
# afghan women association international   406   162      70       23    78   3    1     25     1   3
# albertsons                                 0     0       0        0    52   0    9      0     0   0
# alcan                                      1     0       2        0    34   0    0      2     1   1
# america open technology institute          0     0       0       14    56   0   14     56     0   0
# american civil liberties union            22     1       2        0    48   0    1      4     0   0
# american community survey                  0     0       0        0    33   0    0      2     0   0

# This looks a bit cleaner
# Convert to tf-idf
dfm_tfidf <- dfm_tfidf(web_dtm_stem_filt, scheme_tf = "prop")

pc_text_pr = stats::prcomp(test[,-1], #The data in question.
                    center=TRUE,
                    scale = TRUE)






## Try again with Bigram
# Use single word tokens for now and clean up the corpus for making a DTM/DFM (DFM relevant for quanteda package)


my_corp <- Corpus(VectorSource(web_corp))
my_corp <- tm_map(my_corp,content_transformer(tolower))
my_corp <- tm_map(my_corp,removePunctuation)
my_corp <- tm_map(my_corp,removeNumbers)
my_corp <- tm_map(my_corp,stripWhitespace)
my_corp <- tm_map(my_corp,stemDocument)
my_corp <- tm_map(my_corp,removeWords,stopwords('en'))
my_corp <- corpus(my_corp)

dtm <- dfm(my_corp)


# filter terms that occur in less than 5% of org news and more than 90% of org news to try and elminiate random sparse ness and or
# irrelevant words to understanding similarity. Somewhat arbitrary choic
web_dtm_filt  <- dfm_trim(dtm,
                              min_docfreq = 0.05,
                              max_docfreq = 0.85,
                              docfreq_type = "prop"
)



#dfm_tfidf <- dfm_tfidf(web_dtm_filt, scheme_tf = "count")

dtm_topics <- convert(web_dtm_filt, to = "topicmodels")
lda <- topicmodels::LDA(dtm_topics, k = 20)

test <- tsne(dtm_topics,perplexity = 20)





mylsa <- textmodel_lsa(web_dtm_one_filt, nd = 10)

plot(
  mylsa$docs[, 1:2],
  pch = 19,
  xlab = "Dimension 1",
  ylab = "Dimension 2",
  main = "LSA dimensions by subcorpus"
)


final_df <- g_dat[,c("Freq",'webtext')]




#new_dat <- read.csv('news_word_table.csv')
#colnames(new_dat) <- c('name','source','webtext')
#row.names(new_dat) <- new_dat$X

customwords <- (c("live","watch","said","\"","trump"))

BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

#tokenize= BigramTokenizer,

my_corp <- corpus(final_df, text_field = "webtext")
my_corp <- Corpus(VectorSource(my_corp))
my_corp <- tm_map(my_corp,content_transformer(tolower))
my_corp <- tm_map(my_corp,removePunctuation)
my_corp <- tm_map(my_corp,removeNumbers)
my_corp <- tm_map(my_corp,stripWhitespace)
my_corp <- tm_map(my_corp,removeWords,stopwords('en'))
my_corp <- VCorpus(VectorSource(my_corp))

tm_filter(my_corp, FUN = function(x) any(grep("fresno", (x))))

#my_dtm <- DocumentTermMatrix(my_corp,control = list(weighting = weightTfIdf))

better_dtm <- removeSparseTerms(my_dtm,0.99)






pc_text = stats::prcomp(test[], #The data in question.
                    nfactors = 2, #The number of PCs to extract.
                    rotate = "none")

topic_words <- sapply(get_topics(lda),FUN =  function(x){paste(get_terms(lda,5)[,x], collapse = ", ")})
Topic_Cluster <- as.factor(get_topics(lda))

p <- ggplot(as.data.frame(test), aes(x = V1, y = V2,label = rownames(dtm_topics),text=topic_words,color= get_topics(lda))) + 
  geom_point()

pp <- ggplotly(p)









# Assigns a unique identifier to each text
docvars(mycorpus, "Textno") <-
  sprintf("%02d", 1:ndoc(mycorpus)) 