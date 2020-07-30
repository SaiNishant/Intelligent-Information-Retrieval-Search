library(textmineR)


# create a document term matrix 
tdm <- CreateDtm(doc_vec = results$snippet, # character vector of documents
                 doc_names = results$ID, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system



tdm.tfidf <- tm::weightTfIdf(dtm)




# construct the matrix of term counts to get the IDF vector
tf_mat <- TermDocFreq(dtm)

tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)

hc <- hclust(cdist, "ward.D")

km<-kmeans(cdist,40)
table(km$cluster)


clustering <- cutree(hc, 10)


EM_model<-Mclust(cdist)
summary(EM_model)

ind<-which(is.na(cdist))
cdist[ind]<-0


clustering.kmeans <- kmeans(cdist, 44) 
clustering.hierarchical1 <- hclust(cdist, method = "ward.D2") 
hc1<-cutree(clustering.hierarchical1,44)
fviz_dend(clustering.hierarchical, palette = "jco",
          rect = TRUE, show_labels = FALSE)

cut<-cutree(clustering.hierarchical,44)

clustering.hierarchical1 <- hclust(cdist, method = "com")
clustering.hierarchical2 <- hclust(cdist, method = "single")
clustering.hierarchical3 <- hclust(cdist, method = "ave")
clustering.hierarchical4 <- hclust(cdist, method = "centroid")
dend1<-as.dendrogram(clustering.hierarchical1)
dend2<-as.dendrogram(clustering.hierarchical2)
dend3<-as.dendrogram(clustering.hierarchical3)
dend4<-as.dendrogram(clustering.hierarchical4)
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2, "Average" = dend3, "Centroid" = dend4)
cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)

hc_a <- agnes(cdist, method = "complete")

hc_a$ac

clustering.dbscan <- fpc::dbscan(cdist, 44) 

master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = 44) 
slave.dbscan <- clustering.dbscan$cluster 
stacked.clustering <- rep(NA, length(master.cluster))  
names(stacked.clustering) <- 1:length(master.cluster) 
for (cluster in unique(master.cluster)) { 
  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  slave1.votes <- table(slave.hierarchical[indexes]) 
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  slave2.votes <- table(slave.dbscan[indexes]) 
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  stacked.clustering[indexes] <- slave2.maxcount 
}

points <- cmdscale(cdist, k = 44) 
palette <- colorspace::diverge_hcl(44) # Creating a color palette 
previous.par <- par(mfrow=c(2,1), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
#plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
#plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
par(previous.par) # recovering the original plot space parameters



cs = cluster.stats(cdist, clustering.kmeans$cluster)
cscs2 = cluster.stats(cdist, cut)
cs2

library(ggplot2)
algorithm<-c("K-means","Hierarchical-Clustering")
Within_cluster_ss<-c(cs$within.cluster.ss,cs2$within.cluster.ss)
valid<-data.frame(algorithm,Within_cluster_ss)

x<-ggplot(valid,aes(algorithm,Within_cluster_ss,fill=algorithm))+geom_bar(stat="identity",width = 0.9)+geom_text(aes(label = paste(round(Within_cluster_ss,2))), vjust = -0.3)+theme(aspect.ratio = 2/1)
x+ coord_cartesian(ylim = c(950,970))
