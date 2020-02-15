insurance <- read.csv("/Users/yuxinma/Desktop/insurance.csv")

# Random Forest
## Prep

# Check NA/Inf
apply(insurance, 2, function(x) any(is.na(x)))
apply(insurance, 2, function(x) any(is.infinite(x)))

# Categorical variables
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

## Train & Test
library(caTools)
set.seed(123)
split <- sample.split(insurance$charges,SplitRatio = 0.8)
train <- subset(insurance,split = TRUE)
test <- subset(insurance,split = FALSE)

## Prediction
library(scorer)
library(randomForest)
predict_rf = randomForest(x = train[,1:6], y = train$charges,
                          ntree = 100, importance = T)

pred <- predict(predict_rf, newdata = test[,1:6])
(rf_r2 <- r2_score(test$charges, pred))

## Variable Importance
library(ggplot2)
varimp <- data.frame(predict_rf$importance)
ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="darkblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

## Plot Pred vs True (by Age & Charges)
ggplot(test)+
  geom_point(aes(x=age , y = charges,color ="red"))+
  geom_line(aes(x=age , y  = pred,color ="blue")) + 
  scale_color_manual(labels = c("Pred", "True"), values = c("red", "blue"))

# Unsupervised - KMeans Clustering
## Prep
# Numeric variables
insurance$sex_num <- as.numeric(insurance$sex)
insurance$smoker_num <- as.numeric(insurance$smoker)
insurance$region_num <- as.numeric(insurance$region)

## Elbow plot to select K
library(factoextra)
fviz_nbclust(insurance[,c(1,3,4,7,8,9,10)], kmeans, method = "wss")

## KMeans

ins_km <- kmeans(insurance[,c(1,3,4,7,8,9,10)], centers = 3, nstart = 25)

#fviz_cluster(ins_km, data = insurance[,c(1,3,4,7,8,9,10)], repel = FALSE,
#             geom = "point",
#             show.clust.cent = TRUE,
#             alpha = 0)

## Plot clustering (by Age & Charges)
insurance$cluster <- as.factor(ins_km$cluster)
ggplot(insurance)+
  geom_point(aes(x = age , y = charges, color = cluster))

## Continue variables mean values
aggregate(insurance[,c(1,3,4,7)], list(insurance$cluster), mean)

## Categorical variables proportion
### Smoker
prop.table(table(insurance[insurance$cluster == 1,][5]))
prop.table(table(insurance[insurance$cluster == 2,][5]))
prop.table(table(insurance[insurance$cluster == 3,][5]))

