
decisiontreedata = cbind.data.frame(discretize(imdbData$director_facebook_likes,categories = 3),discretize(imdbData$num_critic_for_reviews,categories = 3),discretize(imdbData$num_voted_users,categories = 3),discretize(imdbData$num_user_for_reviews,categories = 3),imdbData$language,imdbData$country,imdbData$content_rating,discretize(imdbData$movie_facebook_likes,categories = 3),discretize(imdbData$imdb_score,categories = 3),discretize(imdbData$gross,categories = 3))
decisiontreedata =na.omit(decisiontreedata)
colnames(decisiontreedata) = c("director_facebook_likes","num_critic_for_review","num_voted_users","num_users_for_review","language","country","content_rating","movie_facebook_likes","gross")

#decisiontree try1
output.tree <- ctree(
  decisiontreedata$gross ~ decisiontreedata$movie_facebook_likes + decisiontreedata$content_rating  + decisiontreedata$num_users_for_review + decisiontreedata$num_voted_users + decisiontreedata$num_critic_for_review + decisiontreedata$director_facebook_likes , 
  data = decisiontreedata)
clusterdata = cbind.data.frame(imdbData$num_critic_for_reviews,imdbData$duration,imdbData$num_voted_users,imdbData$num_user_for_reviews,discretize(imdbData$gross,categories = 5))
colnames(clusterdata) = c("num_critic_for_reviews","duration","num_voted_users","num_users_for_review","gross")
clusterdata=na.omit(clusterdata)
plot(output.tree)

dat1 = data.frame(clusterdata)
index <- 1:nrow(dat1)
testindex <- sample(index, trunc(length(index)*1/4))
clustertestset <- dat1[testindex,]
clustertrainset <- dat1[-testindex,]


##new tree method
decisiontreedata = cbind.data.frame(discretize(imdbData$director_facebook_likes,categories = 3),discretize(imdbData$num_critic_for_reviews,categories = 3),discretize(imdbData$num_voted_users,categories = 3),discretize(imdbData$num_user_for_reviews,categories = 3),imdbData$language,imdbData$country,imdbData$content_rating,discretize(imdbData$movie_facebook_likes,categories = 3),discretize(imdbData$gross,categories = 5))
decisiontreedata =na.omit(decisiontreedata)
decisiontreedata = data.frame(decisiontreedata)
dat1 = data.frame(decisiontreedata)
index <- 1:nrow(dat1)
colnames(decisiontreedata) = c("director_facebook_likes","num_critic_for_review","num_voted_users","num_users_for_review","language","country","content_rating","movie_facebook_likes","gross")
testindex <- sample(index, trunc(length(index)*1/4))
decisiontreetestset = decisiontreedata[testindex,]
decisiontreetrainset = decisiontreedata[-testindex,]




output.tree <- ctree(
  gross ~ movie_facebook_likes + content_rating  + num_users_for_review + num_voted_users + num_critic_for_review + director_facebook_likes, 
  data = decisiontreetrainset)
plot(output.tree)
decisiontreePredctree<- predict(output.tree, decisiontreetestset[ ,c(8,7,4,3,2,1)])
table(decisiontreePredctree, decisiontreetestset$gross, dnn = c("Actual", "Predicted"))


##new method 2
#

output1.tree <- rpart(
    gross ~ movie_facebook_likes + content_rating  + num_users_for_review + num_voted_users + num_critic_for_review + director_facebook_likes, 
    decisiontreetrainset)
plot(output1.tree)
text(output1.tree, pretty = 0)
decisiontreePred <- predict(output1.tree, decisiontreetestset[ ,c(8,7,4,3,2,1)], type = 'class')
table(decisiontreePred, decisiontreetestset$gross)



library(ggplot2)
ggplot(clusterdata, aes(num_voted_users,num_critic_for_reviews, duration, color = gross)) + geom_point()
set.seed(20)
imdbCluster <- kmeans(clustertestset[,1:3], 3, nstart = 25)

imdbCluster$cluster <- as.factor(imdbCluster$cluster)
ggplot(clustertestset, aes(num_voted_users,num_critic_for_reviews, duration, color = imdbCluster$cluster)) + geom_point()
#predict.kmeans(clustertrainset,imdbCluster)


cl1 = kcca(clustertrainset[, 1:3], k=3, kccaFamily("kmeans"))
cl1    


pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=clustertrainset[, 1:3])

image(cl1)
points(clustertrainset[, 1:3], col=pred_train, pch=19, cex=0.3)
points(clustertestset[, 1:3], col=pred_test, pch=22, bg="orange")



##new method
# scale the data to values in the range[0,1]

clusterdat <- data.frame(lapply(clusterdata[,1:4], function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))





imdbCluster <- kmeans(clusterdat[,1:4], 5, nstart = 25)
table(imdbCluster$cluster,clusterdata$gross)
plot(imdbCluster$cluster)


library(cluster)
library(fpc)

clusplot(clusterdata, imdbCluster$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
plotcluster(clusterdata[,1:4], imdbCluster$cluster)
