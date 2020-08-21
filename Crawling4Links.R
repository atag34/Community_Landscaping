library(jsonlite)
library(dplyr)
library(RCurl)
library(rvest)
library(httr)
library(stringr)
library(tm)
library(wordcloud2)

domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]

### function to Search for org in location on google for their site
getwebsite <- function(org,city,state){
g_search <- read_html(paste0("https://www.google.com/search?&q=",gsub(" ","+",org),"+",gsub(" ","+",city),"+",state)) #create google search
results <- g_search %>% #use search results and extract links
              html_nodes(xpath = "//div/div/div/a/div[not(div)]") %>%
                  html_text()
  
#url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
#url_pattern <- "((ftp|http|https|www))?(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+(\\.com|\\.net|\\.edu|\\.co|\\.gov|\\.io)"
url_pattern <- "^(http:\\/\\/www\\.|https:\\/\\/www\\.|http:\\/\\/|https:\\/\\/)?[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})?(\\/.*)?"
url <- str_extract(results[which(!is.na(results))], url_pattern)[1]
url <- domain(url)
  return(url)
}

## Read urls from the page and return local links (IE it wont collect offsite links)
getalllinks <- function(g_url){
  
  main_site <- domain(g_url)
  
  links <- unique(html_attr(html_nodes(read_html(paste0("https://www.",main_site)), "a"), "href"))
  
  if (length(links)==0){
    links <- unique(html_attr(html_nodes(read_html(paste0("http://www.",main_site)), "a"), "href"))
    links <- links[which((startsWith(links, "/") & !startsWith(links,"//")) | grepl(main_site,links))]
    links <- ifelse(grepl(main_site,links),links,paste0(main_site,links))
  }else{
    links <- links[which((startsWith(links, "/") & !startsWith(links,"//")) | grepl(main_site,links))]
    links <- ifelse(grepl(main_site,links),links,paste0(main_site,links))}

#second level search
    links <- unique(unlist(lapply(links,FUN= function(x){ tryCatch({unique(html_attr(html_nodes(read_html(paste0(x)), "a"), "href"))}, error=function(e){})})))
    links <- links[which((startsWith(links, "/") & !startsWith(links,"//")) | grepl(main_site,links))]
    links <- ifelse(grepl(g_url,links),links,paste0(main_site,links))
    return(links)

}
#test <- getwebsite("The Ford Foundation","new york","NY")



blank <- list()

for (i in 1:length(links)){
tryCatch({
page <- read_html(paste0(ifelse(grepl("www.",links[i]),links[i],ifelse(any(grepl("http://",links)),paste0("http://www.",links[i]),paste0("https://www.",links[i]))))) 
#then find the link nodes, then extract the attribute text (ie the links)
link <-page%>% html_nodes("a") %>% html_attr( "href")
#return second string of first list element 
#(Use sapply if there are more than 1 link in document)
desiredlink<-str_split(link, "\\?q=")[[1]][2]

#Find the text in all of the span nodes
span_text<-page%>% html_nodes("span") %>% html_text()
# or this for the text under the p nodes
p_text<-page%>% html_nodes("p") %>% html_text()
blank[[i]] <- p_text
message(i)
Sys.sleep(1)
},error = function(e){})
}

all <- unlist(strsplit(unlist(blank)," "))


mycorp <- Corpus(VectorSource(paste(all,collapse =" ")))
mycorp <- tm_map(mycorp,tolower)
mycorp <- tm_map(mycorp,removePunctuation)
mycorp <- tm_map(mycorp,removeNumbers)
mycorp <- tm_map(mycorp,stripWhitespace)
mycorp <- tm_map(mycorp,removeWords,stopwords('en'))
my_dtm <- DocumentTermMatrix(mycorp)


m<- as.matrix(sort(table(mycorp$content$content),decreasing = T))
v <- sort(rowSums(m),decreasing = T)
d <- data.frame(word=names(v),freq=v)
wordcloud(words=d$word[c(-1)],freq = d$freq[c(-1)],rot.per=0, colors=pal2,min.freq = 10,max.words = 100)


####### comon words
# information
# website
# cookies
# jscript
#javascript
#var
#email


results[which(grepl(pattern = "@^(http\\:\\/\\/|https\\:\\/\\/)?([a-z0-9][a-z0-9\\-]*\\.)+[a-z0-9][a-z0-9\\-]*$@i", results,perl=T))]

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
str_extract(results, url_pattern)
