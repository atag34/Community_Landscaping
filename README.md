Community\_Landscaping
================
Anthony Tagliente
8/3/2020

## Community Landscaping

The project acted as a proof of concept to help identify local actors
attempt to cluster them together based on an analysis of their websites.
Hopefully this would inform work on the ground in identifying potential
partners and networks based on those findings.

## GDELT

GDELT is a great resource available for utilizing quick analysis of
large amounts of news data. In this case we are primarily interested in
looking at Organizations tagged with mentions in specific locations. I
will use Fresno, California as an example use case given its relvance at
the time to my work with the Heron Foundation.

You can read more about GDELT specifically on their website
<https://www.gdeltproject.org/>

We will specifically be using the Global Knowledge Graph data set from
google BigQuery <https://www.gdeltproject.org/data.html#googlebigquery>
in orger to build a base set of organizations and their number of
menttions.

## Google BigQuery

You will need an account with billing information attached to use google
BigQuery services. If you plan to make adjustments, I would recommend
testing them on tight date parameters to limit the amount of data you
are querying. This can and waste resources, which you may incur a cost
for at some point. There is more information on pricing etc. at [Google
Pricing](https://cloud.google.com/bigquery/?utm_source=google&utm_medium=cpc&utm_campaign=na-US-all-en-dr-skws-all-all-trial-p-dr-1009135&utm_content=text-ad-none-any-DEV_c-CRE_288513563439-ADGP_Hybrid+%7C+AW+SEM+%7C+SKWS+%7C+US+%7C+en+%7C+PHR+~+Big+Data+~+BigQuery+~+bigquery-KWID_43700035823139005-kwd-301529154162&utm_term=KW_bigquery-ST_bigquery&gclid=Cj0KCQjw6575BRCQARIsAMp-ksNNLFJ0P1lbf6gjy_0Hvr2aGdsTm5UHPwgVeQ-HBZgaa9sgQhoPTQUaAkO2EALw_wcB#section-10)

As this is a proof of concept test, and an ongoing project, there is
still some work to be done on improving the search criteria. For now, I
have identified two ways for fresno to be tagged while trying to keep it
specifically to California. I have allso tried to exlude what I had
found to be common additional locations tags of articles creating extra
noise in the data.

This is an issue that will need to resolved, perhaps by increasing the
exlusions.

For now I am using stories from 2020, it would likely be more accurate
and inclusive with a longer time parameter.

We can query the database simply using SQL syntax

``` sql
SELECT  Locations, V2Locations, Organizations, V2Organizations, Persons, V2Persons
FROM `gdelt-bq.gdeltv2.gkg_partitioned` 
WHERE (DATE(_PARTITIONTIME) BETWEEN "2020-01-03" AND "2020-08-03") 
AND (Locations LIKE '%Fresno, CA%' OR 
     Locations LIKE '%Fresno, California%' OR 
     V2Locations LIKE '%Fresno, CA%' OR
     V2Locations LIKE '%Fresno%, California%')
AND (Locations NOT LIKE '%Los Angeles%' OR V2Locations NOT LIKE '%Los Angeles%')
AND (Locations NOT LIKE '%Sacramento%' OR V2Locations NOT LIKE '%Sacramento%')
AND (Locations NOT LIKE '%Texas%' OR V2Locations NOT LIKE '%Texas%')
AND (Locations NOT LIKE '%Arizona%' OR V2Locations NOT LIKE '%Arizona%')
AND (Locations NOT LIKE '%Oregon%' OR V2Locations NOT LIKE '%Oregon%')
AND (Locations NOT LIKE '%San Francisco%' OR V2Locations NOT LIKE '%San Francisco%')
AND (Locations NOT LIKE '%Iowa%' OR V2Locations NOT LIKE '%Iowa%')
AND (Locations NOT LIKE '%Nevada%' OR V2Locations NOT LIKE '%Nevada%')
AND (Locations NOT LIKE '%New Mexico%' OR V2Locations NOT LIKE '%New Mexico%')
LIMIT 10000
```

I saved the scv output here for this demonstration which can be read
into R:

``` r
# csv of results from gdelt search
results <- read.csv("results-20200806-140844.csv", stringsAsFactors = F)

# showing Organizations column for first row
results$Organizations[1]
```

    ## [1] "instagram;democrat party;media research center;washington post;facebook;california state university;hoover institution at stanford university;national center;white house;world health organization;defense intelligence agency;monroe county public health department;united states;youtube;new york times;national security agency"

We can see that the data will need to split by the `:` character, to
improve the odds of correctly counting their frequency, we want to clean
the names of numbers and force all letter to lower case.

Then we can add a links column and fill it with all links from our GDELT
results with the organizations name mentioned.

``` r
# Create a new dataframe based on a frequency table of the organizations in our results object
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

# search for links where the organizations name(Var1) occurs in the organizations column and add it to the g_dat table we created in the links column.
g_dat$links <- lapply(g_dat$Var1,
                      FUN=function(x){
                        results$DocumentIdentifier[which(
                                                      grepl(x,
                                                            results$Organizations
                                                            )
                                                      )
                                                   ]
                        }
                      )
```

This will create a data frame of organizations and the frequency which
they appear in the data set and the links to articles their name was
mentioned in.

``` r
#head(arrange(.data = g_dat,desc(g_dat$Freq)),10)
```

I will manually run through some names that seem like they should be
excluded. My reasoning being that they are either obviously of national
interest or may be a news source rather than the topic. I tried to leave
anything ambigious in. Leaving more names to search through will
increase the amount of noise but also potentially capture more important
organizations you might be interested in.1

``` r
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

# Remove rows where the name is in the above list
g_dat <- g_dat[!g_dat$Var1 %in% remove_orgs, ]

# head(arrange(.data = g_dat,desc(g_dat$Freq)),10)
```

We can add a field to collect the text of the news article links. If
running this for yourself, it will take a bit of time. This process
could be improved with asynch requests which I will come back and adjust
another time.

``` r
g_dat$webtext <- NA

for (i in which(is.na(g_dat$webtext))){
  org_text <- list()
    
    for (k in 1:length(g_dat$links[[i]])){
      tryCatch({
    page <- read_html(GET(g_dat$links[[i]][k],timeout(5)))
    # or this for the text under the p nodes
    p_text<-page%>% html_nodes("p") %>% html_text()
    org_text[[k]] <- p_text
    message(k,' of ',length(g_dat$links[[i]]),' for org ',i,' of ',nrow(g_dat),'',paste(as.character(g_dat$Var1[i])))
    #Sys.sleep(1)
    closeAllConnections()
  },error = function(e){})
    }
g_dat$webtext[i] <- paste0(unlist(org_text),collapse =" ")  
}
# add rownames which are used as Doc IDs for corpus
rownames(g_dat) <- g_dat$Var1

#Saving dataframe as backup and to have as an easily usable example for this markdown
write.csv(g_dat[,-3],"web_text.csv")
```

``` r
# Read in if you are running this interactivly.
g_dat_in <- read.csv("web_text.csv", row.names = "X")
```

Now that we have a table containing the org names as well as the text of
the news articles linked in the gdelt database. We can begin to
construct a corpus, which is a useful object for doing text analysis.

In this case I will be using the `quanteda` package and the `tm`
package.

``` r
web_corp <- corpus(g_dat_in[,c("Freq","webtext")], text_field = "webtext")

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
# irrelevant words to understanding similarity. Somewhat arbitrary choice
web_dtm_filt  <- dfm_trim(dtm,
                              min_docfreq = 0.05,
                              max_docfreq = 0.9,
                              docfreq_type = "prop"
)
web_dtm_filt
```

    ## Document-feature matrix of: 269 documents, 5,508 features (79.1% sparse).
    ##                                         features
    ## docs                                     women radio network internet peopl dan
    ##   afghan women association international   429   162      70       23    78   3
    ##   albertsons                                 0     0       0        0    45   0
    ##   alcan                                      1     0       2        0    33   0
    ##   america open technology institute          0     0       0       14    43   0
    ##   american civil liberties union            22     1       2        0    46   0
    ##   american community survey                  0     0       0        0    33   0
    ##                                         features
    ## docs                                     hill presid logic inc
    ##   afghan women association international    1     25     1   3
    ##   albertsons                                9      0     0   0
    ##   alcan                                     0      2     1   1
    ##   america open technology institute        14     43     0   0
    ##   american civil liberties union            1      4     0   0
    ##   american community survey                 0      2     0   0
    ## [ reached max_ndoc ... 263 more documents, reached max_nfeat ... 5,498 more features ]

We now have a Document Feature Matrix, which is similar to a Document
Term Matrix adjusted to work with other quanteda functions. Since the
goal here is to have an easily interpretible chart, it would be nice to
understand what words are being associated with what orgs, partiularly
to check that the cluistering makes sense.

Since we don’t already know what any topics should be we can use an un
supervised approach to topic modeling using the LDA function from the
`textmodels` package.

``` r
# We can add a layer of topic modeling to give us a better insight into similar org news texts and common words associated with their grouping.

# Create a topic model object from the corpus
dtm_topics <- convert(web_dtm_filt, to = "topicmodels")

# and use the LDA function to determine 15 topic clusters, the number can be better optimized. This will take a bit of time to finish running.
lda <- topicmodels::LDA(dtm_topics, k = 15)

get_terms(lda,5)
```

    ##      Topic 1     Topic 2     Topic 3  Topic 4       Topic 5  Topic 6    
    ## [1,] "household" "tree"      "court"  "coronavirus" "counti" "fish"     
    ## [2,] "share"     "say"       "year"   "fight"       "covid"  "river"    
    ## [3,] "median"    "chill"     "school" "wednesday"   "tular"  "water"    
    ## [4,] "citi"      "pistachio" "us"     "peopl"       "case"   "fli"      
    ## [5,] "photo"     "winter"    "work"   "event"       "newsom" "reservoir"
    ##      Topic 7  Topic 8   Topic 9   Topic 10 Topic 11  Topic 12   Topic 13 
    ## [1,] "newsom" "polic"   "women"   "mask"   "student" "today"    "fbi"    
    ## [2,] "hospit" "offic"   "drug"    "health" "colleg"  "year"     "tech"   
    ## [3,] "food"   "fire"    "compani" "wear"   "univers" "american" "appl"   
    ## [4,] "valley" "counti"  "generic" "public" "may"     "graduat"  "iphon"  
    ## [5,] "counti" "sheriff" "also"    "energi" "aid"     "high"     "compani"
    ##      Topic 14 Topic 15
    ## [1,] "low"    "health"
    ## [2,] "high"   "counti"
    ## [3,] "wind"   "case"  
    ## [4,] "mph"    "covid" 
    ## [5,] "feet"   "public"

We also want to significantly reduce the number of dimensions in order
to better display the data. t.sne is very good at reducing and high
numbers of dimensions for the purpose of visualizations.

``` r
dat_tsne <- tsne(dtm_topics)
```

    ## Warning in if (class(X) == "dist") {: the condition has length > 1 and only the
    ## first element will be used

    ## sigma summary: Min. : 0.240969594214663 |1st Qu. : 0.45820397339818 |Median : 0.72637107940506 |Mean : 0.771875900737361 |3rd Qu. : 1.1151539880689 |Max. : 1.45051369086749 |

    ## Epoch: Iteration #100 error is: 15.5868199336183

    ## Epoch: Iteration #200 error is: 0.361289849188814

    ## Epoch: Iteration #300 error is: 0.331223945366042

    ## Epoch: Iteration #400 error is: 0.300987417117487

    ## Epoch: Iteration #500 error is: 0.299305661885399

    ## Epoch: Iteration #600 error is: 0.297176245519942

    ## Epoch: Iteration #700 error is: 0.288198328648763

    ## Epoch: Iteration #800 error is: 0.286777577042117

    ## Epoch: Iteration #900 error is: 0.278642631962738

    ## Epoch: Iteration #1000 error is: 0.278354624927647

# Making the Final Plot.

``` r
topic_words <- sapply(get_topics(lda),
                      FUN = function(x){
                                paste(get_terms(lda,5)[,x],
                                      collapse = ", ")
                        }
                      )
Topic_Cluster <- as.factor(get_topics(lda))

p <- ggplot(
        as.data.frame(dat_tsne), 
        aes(
          x = V1, 
          y = V2,
          text=paste0('Organization:',rownames(dtm_topics),
                      "\n",
                     'Terms:',topic_words),
          color=Topic_Cluster)
        ) +
      geom_point() +
      labs(title='Organization Cluster: Fresno, CA',
           color='Topic Cluster') 

p
```

![](example_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Using an
interactive version of the chart, you can explore the various clusters
and try to determine connections through external research. Using the
`plotly` package and the `ggplotly()` function you can easily convert
our previous ggplot2 chart into a plotly interactive chart.

``` r
pi <- ggplotly(p)
pi
```

![Sample of interactive version.](Interactive_plot.png) As a brief
example, looking in a grouping that seems to be mostly police offices,
we see close by a company called del rey packing co. Which would seem a
bit odd as to why they might have similar news about them. ![Zoomed area
of plot with labels.](del_ray_packaging.png) You can probably infer they
have some involvement with police but by searching online you can verify
that relationship.
![<https://ktla.com/news/local-news/woman-dies-after-hair-clothing-get-caught-in-raisin-processin-machine-in-fresno-county/>.](del_ray_news.png)
