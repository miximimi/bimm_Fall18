# important function list: 
# 
# kmeans(x)
# hclust(dist(x))
# cutree(hc)
# table()


tmp <- c(rnorm(30,-3), rnorm(30,3))
x <- cbind(x=tmp, y=rev(tmp))
plot(x)
a <- kmeans(x,centers = 2,nstart = 20)
a
a["cluster"]
palette(c("blue","red","yellow"))
points(a$centers, col = c(1,2), pch = 8, cex = 2)
points(x, col = a$cluster)


# 2nd: 3 cluster centers
tmp <- c(rnorm(30,-3), rnorm(30,3))
x <- cbind(x=tmp, y=rev(tmp))
plot(x)
cl1 <- kmeans(x,centers = 2,nstart = 20)
cl2 <- kmeans(x,centers = 3,nstart = 20)
points(cl$centers, col = "green", pch = 20, cex = 2)
points(x, col = cl$cluster)

cl$totss
cl2$totss

cl$tot.withinss
cl2$tot.withinss

################## hieracial cluster method
d <- dist(x)
hc <- hclust(d)
hc
plot(hc) #out = dendrogram; default: method="complete"
abline(h=8,col="red")
cutree(hc,k = 2)
cutree(hc,h = 6)

##################
# Step 1. Generate some example data for clustering
x <- rbind(
  matrix(rnorm(100, mean=0, sd = 0.3), ncol = 2), # c1
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2), # c2
  matrix(c(rnorm(50, mean = 1, sd = 0.3), # c3
           rnorm(50, mean = 0, sd = 0.3)), ncol = 2))
colnames(x) <- c("x", "y")
# Step 2. Plot the data without clustering
plot(x)
# Step 3. Generate colors for known clusters
# (just so we can compare to hclust results)
col <- as.factor( rep(c("c1","c2","c3"), each=50) )
palette(c("red","blue","brown"))
plot(x, col=col, pch=20)
# Q. Use the dist(), hclust(), plot() and cutree()
# functions to return 2 and 3 clusters

d <- dist(x)
hc <- hclust(d)
plot(hc) 
abline(h=8,col="red")
cut_1 <- cutree(hc,k = 2)
cut_2 <- cutree(hc,k = 3)

plot(x, col=cut_1, pch=20) #plot: two cuts
plot(x, col=cut_2, pch=20) #plot: two cuts

# Q. How does this compare to your known 'col' groups?

table(cut_2,col) #number of wrongly assigned group

################ PCA: 
#data analysis for many dimensions
## Initialize a blank 100 row by 10 column matrix
mydata <- matrix(nrow=100, ncol=10)
## Lets label the rows gene1, gene2 etc. to gene100
rownames(mydata) <- paste("gene", 1:100, sep="")
## Lets label the first 5 columns wt1, wt2, wt3, wt4 and wt5
## and the last 5 ko1, ko2 etc. to ko5 (for "knock-out")
colnames(mydata) <- c( paste("wt", 1:5, sep=""),
                       paste("ko", 1:5, sep="") )
## Fill in some fake read counts
for(i in 1:nrow(mydata)) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  mydata[i,] <- c(wt.values, ko.values)
}
head(mydata)

##
## lets do PCA
pca <- prcomp(t(mydata), scale=TRUE) #T: transpose; always make scale
## See what is returned by the prcomp() function
attributes(pca)
# $names
#[1] "sdev" "rotation" "center" "scale" "x"
#
# $class
#[1] "prcomp"

#new PC plot
plot(pca$x[,1], pca$x[,2]) 
## Precent variance is often more informative to look at
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) #standarize(?) proportion of score

pca.var.per #how much each pca counts
# [1] 91.0 2.8 1.9 1.3 0.8 0.7 0.6 0.5 0.3 0.0

#see which pca is the most important
barplot(pca.var.per, main="Scree Plot",
        xlab="Principal Component", ylab="Percent Variation")


## A vector of colors for wt and ko samples
colvec <- colnames(mydata)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"
plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)")) 


## Click to identify which sample is which
#interactive
identify(pca$x[,1], pca$x[,2], labels=colnames(mydata))
## Press ESC to exit… 


## Lets focus on PC1 as it accounts for > 90% of variance
loading_scores <- pca$rotation[,1] 


summary(loading_scores)
#
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# -0.104763 -0.104276 -0.068784 -0.005656 0.103926 0.104797

## We are interested in the magnitudes of both plus
## and minus contributing genes
gene_scores <- abs(loading_scores) 

gene_score_ranked <- sort(gene_scores, decreasing=TRUE) 


# final goal: which cell is cancer, which one is not. And which gene is possibly cancer



################################# Hands on Session worksheet

x <- read.csv("data/UK_foods.csv")
dim(x)
head(x)

knitr::kable(x, caption="The full UK foods data table")


