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
g_dat <- g_dat[g_dat$Freq>1,]

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

g_dat <- g_dat[,c("Var1","Freq","webtext")]
rownames(g_dat) <- g_dat$Var1

#new_dat <- read.csv('news_word_table.csv')
#colnames(new_dat) <- c('name','source','webtext')
#row.names(new_dat) <- new_dat$X

customwords <- (c("live","watch","said","\"","trump"))

BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

#tokenize= BigramTokenizer,

my_corp <- corpus(g_dat, text_field = "webtext")
my_corp <- Corpus(VectorSource(my_corp))
my_corp <- tm_map(my_corp,content_transformer(tolower))
my_corp <- tm_map(my_corp,removePunctuation)
my_corp <- tm_map(my_corp,removeNumbers)
my_corp <- tm_map(my_corp,stripWhitespace)
my_corp <- tm_map(my_corp,removeWords,stopwords('en'))
my_corp <- VCorpus(VectorSource(my_corp))

tm_filter(my_corp, FUN = function(x) any(grep("fresno", (x))))

my_dtm <- DocumentTermMatrix(my_corp,control = list(weighting = weightTfIdf))

better_dtm <- removeSparseTerms(my_dtm,0.99)




fa.parallel(test, #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.

pc_text = principal(test, #The data in question.
                    nfactors = 2, #The number of PCs to extract.
                    rotate = "none")


p <- plot_ly(as.data.frame(pc_text$scores), x = ~PC1, y = ~PC2, text=row.names(as.data.frame(pc_text$scores))) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2')))
