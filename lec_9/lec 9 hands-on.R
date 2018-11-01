# lec_9_hands-on

# 1. import data and convert data.fame to matrix
wisc.df <- read.csv('WisconsinCancer.csv')

str(wisc.df) # review structure

wisc.data <- as.matrix(wisc.df[,3:32],dimnames = wisc.df$id)
rownames(wisc.data) <- wisc.df$id

# Exploratory data analysis
# Q1. How many observations are in this dataset?
dim(wisc.data)
# Q2. How many variables/features in the data are suffixed with _mean?
a = length(grep('_mean',colnames(wisc.df),value = F))
# there are a # of mean values in the report

# Q3. How many of the observations have a malignant diagnosis?
diagnosis <- wisc.df$diagnosis=='M'
table(wisc.df$diagnosis)

# Section 2.
# Performing PCA

colMeans(wisc.data) 
apply(wisc.data,2,sd)

scaled_wisc.data <- scale(wisc.data)

# standarize wisc.data for difference between Observ high sd and mean 
library('stats')
wisc.pr <- prcomp(scaled_wisc.data)

PCA_wisc <- summary(wisc.pr) # ??????

# Q4. From your results, what proportion of the original variance is 
#captured by the first principal components (PC1)?
PCA_wisc$importance[2,1] #get the proportion of variance from actual summary table

# Q5. How many principal components (PCs) are required to describe at
#least 70% of the original variance in the data?
cum_PCA_wisc70up <- PCA_wisc$importance[3,]>0.7
# idx_wisc70up <- PCA_wisc$importanc[cum_PCA_wisc70up]
length(PCA_wisc$importance[3,])-sum(PCA_wisc$importance[3,]>=0.7)+1


# Q6. How many principal components (PCs) are required to describe at
#least 90% of the original variance in the data?
length(PCA_wisc$importance[3,])-sum(PCA_wisc$importance[3,]>=0.9)+1

# Create a biplot of the wisc.pr using the biplot() function.
# Q7. What stands out to you about this plot? Is it easy or difficult to understand? Why?
biplot(wisc.pr,scale = T)
# hard to see anything
attributes(wisc.pr) # find dollars
dim(wisc.pr$x)
#plot PC1 vs PC2
plot(wisc.pr$x[,1],wisc.pr$x[,2],xlab = "PC1", ylab = "PC2",col=diagnosis+1)
palette() #check which color is Malign and bad

# Q8. Repeat the same for principal components 1 and 3. What do you notice about these plots?
# smaller
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")


pr.var <- wisc.pr$sdev^2 # Calculate the variance of each principal component
pve <- pr.var/sum(pr.var) #Calculate the variance explained by each principal 
# component by dividing by the total variance explained of all principal components.
plot(pve,xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "o")

barplot(pve, ylab = "Precent of Variance Explained",
        names.arg=paste0("PC",1:length(pve)), las=2, axes = FALSE)
axis(2, at=pve, labels=round(pve,2)*100 ) #exibit only useful y-axis data

cumsum()
plot(cumsum(pve),xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "o")
#above: judge how well PCA doing

# Q9. For the first principal component, what is the component of 
# the loading vector (i.e. wisc.pr$rotation[,1]) for the feature concave.points_mean?
# concave.points_mean



# Q10. What is the minimum number of principal components required to explain 80% of 
# the variance of the data?

# 3. cluster using PCA result

data.scaled <- scale(wisc.data)
data.dist <- dist(data.scaled)
wisc.hclust <- hclust(d = data.dist)

# Q11. Using the plot() function, what is the height at which the clustering model
# has 4 clusters?

plot(wisc.hclust)
wisc.hclust.clusters <- cutree(wisc.hclust,k = 4) #assign data to four clusters
table(wisc.hclust.clusters,diagnosis) 

# Q12. Can you find a better cluster vs diagnoses match with by cutting into a 
# different number of clusters between 2 and 10?

wisc.hclust.clusters <- cutree(wisc.hclust,k = 4) #assign data to four clusters
table(wisc.hclust.clusters,diagnosis) # compare result with diagnosis

#Section 4.
# K-means clustering and comparing results



# wisc.km <- kmeans(wisc.data, centers= ___, nstart= ___)


# Section 5.
# Clustering on PCA results
# use our model to persict

## Use the distance along the first 7 PCs for clustering i.e. wisc.pr$x[, 1:7]
# need distance 
d.pr <- dist(wisc.pr$x[,1:7]) #use only PCs explaining 90%
wisc.pr.hclust <- hclust(d.pr, method='complete')
plot(wisc.pr.hclust)

# Q14. How well does the newly created model with four clusters separate out the two diagnoses?
wisc.hclust.clusters2 <- cutree(wisc.pr.hclust,k = 4) #assign data to four clusters
table(wisc.hclust.clusters,diagnosis) 
table(wisc.hclust.clusters2,diagnosis) # I think the second is even more noisy...?

# Section 6.



### perdictive modeling with PCA components

## Predicting Malignancy Of New samples
url <- "https://tinyurl.com/new-samples-CSV"
new <- read.csv(url)
npc <- predict(wisc.pr, newdata=new)
plot(wisc.pr$x[,1:2],col = diagnosis+1)
     # , col=grps)
points(npc[,1], npc[,2], col="blue", pch=16,cex=3)


url <- "https://tinyurl.com/new-samples-CSV"
new <- read.csv(url)
npc <- predict(wisc.pr, newdata=new)
plot(wisc.pr$x[,1:2], col=diagnosis+1)
points(npc[,1], npc[,2], col="blue", pch=16)




plot(c(1,2,3,4),col = "red")

points(c(1.5,1.5), c(2.5,2.5), col="blue", pch=16,cex=3)





