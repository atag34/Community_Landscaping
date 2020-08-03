library(jsonlite)
library(plyr)
library(dplyr)
library(cluster)
library(tm)
library(stringi)
library(proxy)
library(RCurl)
library(psych)
library(foreach)
library(doSNOW)
library(plotly)
library(NbClust)


df <- read.csv("C:/Users/atagliente/Box/Heron Home Box/Raw Data/News analysis/Fresno CA (Organization).wordcloud.csv")

empty_list <- list()
cl<-makeCluster(6) 
registerDoSNOW(cl)
system.time(
result <- foreach(i=1:500,.packages=c("dplyr","RCurl", "jsonlite"),.combine=c("bind_rows"), .multicombine = TRUE) %dopar% { 
  tryCatch({ 
    
    frame <- fromJSON(paste0("https://api.gdeltproject.org/api/v2/doc/doc?query=\"",curlPercentEncode(df$Words[i]),"\"&mode=wordcloudenglish&format=JSON"))
    frame <- t(frame$wordcloud)
    colnames(frame) <- frame[1,]
    frame <- frame[2,]
    frame <- data.frame(lapply(frame, type.convert), stringsAsFactors=FALSE)
    frame$id_name <- paste0(df$Words[i])
    frame
    # message(i/50)
    # all_b <- bind_rows(all_b,frame)
  },error=function(e){message("error")})
  
  }
)
# This has taken about 2.5 min to run on 500 expect at least 8 for 1000 in future.
stopCluster(cl)


all <- result[,-173]
names <- result[,173]
all[is.na(all)] <- 0
all <- all[, colSums(all, na.rm = TRUE) >= 100]

# system.time( 
# fa.parallel(all, #The data in question.
#             fa = "pc", #Display the eigenvalues for PCA.
#             n.iter = 100) #Number of simulated analyses to perform.
# )
#abline(h = 1)

pc_text = principal(all, #The data in question.
                      nfactors = 3, #The number of PCs to extract.
                      rotate = "none")

factor.plot(pc_text,
            labels = names)

reduced <- as.data.frame(cbind(names,pc_text$scores), stringsAsFactors = FALSE)
reduced[,2] <- as.numeric(reduced[,2])
reduced[,3] <- as.numeric(reduced[,3])
reduced[,4] <- as.numeric(reduced[,4])


nc <- NbClust(reduced[,-1],min.nc = 2,max.nc = 10, method="kmeans")
fit.km <- kmeans(reduced[,-1],6)
reduced$cluster <- fit.km$cluster
reduced <- reduced[-102,] #remove outlier making chart difficult to read

library(plotly)


p <- plot_ly(reduced, x = ~PC1, y = ~PC2, z=~PC3, color = ~cluster, colors = c('#009775',"#9E256F","#0B71B7","#FF8000","#767ED2"), text = ~names) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="scatter3d/basic")
chart_link


