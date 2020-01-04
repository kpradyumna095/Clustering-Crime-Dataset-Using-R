Crime <- read.csv(file.choose())
View(Crime)

# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Crime[,2:4]) #excluding the region name columnbefore normalizing

d <- dist(normalized_data, method = "euclidean") # distance matrix
d

fit <- hclust(d, method="complete")

plot(fit) # display dendrogram
# By looking towards dendrogram record no 15 & 29 are first formed for grouping as these are close to each other or same no. of crime 
#like wise all grouping is done and arranged in hierarchy.
#Now we have to decide how many cluster we want to do

plot(fit, hang=-1)

rect.hclust(fit, k=3, border="red")

groups <- cutree(fit, k=3)
groups

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Crime, membership)

View(final)
#By watching this we can say that 3 clusters are formed with same no. of crimes categories  attempted among them

final1 <- data.frame(Crime$Murder, membership)
View(final1)
plot(final1)

final2 <- data.frame(Crime$Assault, membership)
View(final2)
plot(final2)

final3 <- data.frame(Crime$UrbanPop, membership)
View(final3)
plot(final3)

final4 <- data.frame(Crime$Rape, membership)
View(final4)
plot(final4)

aggregate(Crime[,-1],by=list(final$membership),mean)
# From above we can infer that cluster1 had more crime cases followed by cluster 2, cluster 3.
# Among all assault is more crime type commited then followed by Urbanpop,Rape and murder resp.


