# Sree Teja Kongara

install.packages("cluster")
install.packages("fpc")

county = read.csv(file.choose())
# Choosing the 5 most important attributes which will deliver the most meaningful clustering results
county1 <- county[,c("Income", "IncomePerCap", "IncomeErr","IncomePerCapErr", "Employed")]
View(county1) # Since attributes are on same scale, there is no need for normalization

# Setting up an elbow analysis to determine optimum number of clusters
set.seed(200)
k.max <- 10
wss <- sapply(1:k.max,function(k){
  kmeans(county1,k,nstart = 20,iter.max = 20)$tot.withinss
})
wss
plot(1:k.max,wss,type = "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
# The most dramatic change in slope is observed at 3. So using k value of 3 for clustering

### kMeans analysis
# Taking a sample of 200 records from the dataset
idx <- sample(1:dim(county1)[1],200)
countySample <-county1[idx,]
kmeans.result <- kmeans(countySample,3)
kmeans.result$size
# Here we three clusters of sizes 119, 3095 and 6
# Plotting the clusters and their centers
plot(countySample[c("Income", "IncomePerCap")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Income","IncomePerCap")], col = 1:3, pch=8, cex=2)

### Hierarchical Clustering
# Taking a sample of 20 records from the dataset
county2 <- county[,c("Income", "IncomePerCap", "IncomeErr","IncomePerCapErr", "Employed","Poverty")]
idx <- sample(1:dim(county2)[1],20)
countySample2 <-county2[idx,]
countySample2$Poverty <- NULL
hc <- hclust(dist(countySample2), method="ave")
plot(hc, hang = -1, labels=county2$Poverty[idx])
# Cutting the tree into 3 clusters
rect.hclust(hc, k =3)
groups <- cutree(hc, k=3)

### DBScan analysis
library(fpc)
ds <- dbscan(county1, eps=0.42, MinPts = 5)
ds
plotcluster(county1, ds$cluster)

#### Q1: How do the cluster distributions that the different algorithms generate differ from one another?

# In the outputs received, the kMeans analysis produced clusters that were overlapping each other significantly, i.e., the inter-cluster distance was
# negligible. The Hierarchical clustering method produced three clearly defined clusters, with proper divisions and no visible overlapping. The DBScan
# method, however showed no visible clusters and only produced outliers despite repeated attempts and adjustments in radius and minimum points. 
########################################################################################################################################################
#### Q2: Which algorithm produces the best results?

# In our opinion, the Hierarchical clustering algorithm produced the most defined clusters without overlapping of points and thus most effective.
# The kMeans analysis produced clusters with heavy overlapping, whereas the DBScan method failed completely in generating clusters and was only
# able to produce outliers(noise).
########################################################################################################################################################
#### Q3: Given the nature of your dataset and its attribute values, why would that alrgorithm produce the best results?

# The hierarchical clustering algorithm was able to cluster the data points based on our class 'Poverty' and separate the values into three clusters
# namely(10.3-13.3),(5.4),(9.1 to 22.7). Here we see overlapping in the last cluster, but it is minimal compared to the overlapping observed in the 
# kMeans output. This is important to our particular dataset as identifying and isolating poverty values helps classify different levels of poverty
# within counties.
########################################################################################################################################################
#### Q4: What do the results tell you about the data?  What business, political, or medical decisions could management make based on the results of 
#### your analysis?
# The results observed tell us that the effect of the chosen attributes on the poverty levels within counties has different impacts when the poverty
# level is at 5.4, between (10.3 and 13.3) and between (9.1 and 22.7). Based on these clusters, the nature of impact of the different attributes can then
# be analyzed and further correlations can be established, giving us further insight. This would help us make political or governing decisions while
# tackling poverty at the county level in the country.
########################################################################################################################################################