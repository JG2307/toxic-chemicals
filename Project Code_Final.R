library(tidyverse)
install.packages('e1701', dependencies = TRUE)
install.packages('caret', dependencies = TRUE)
library(stringr)


# Code For Importing and Indexing 
head(qsar_oral_toxicity)
names(qsar_oral_toxicity)


qsar_oral_toxicity <- read.csv('qsar_oral_toxicity.csv')
names(qsar_oral_toxicity) <- c("x1")
OralTox2 <- as.data.frame(str_split_fixed(qsar_oral_toxicity$x1, ";", 1025))


# Adding sums column 
Unclassified <- OralTox2[-c(1025),drop=FALSE]
OralTox2["Sum"] <-rowSums(sapply(Unclassified[, c(1:1024)],
                         function(x) as.numeric(as.character(x))))

# Code for Clustering 
clust_dat <- qsar_oral_toxicity %>% select(V1, V2)
sil_width <- vector()
for (i in 2:10) {
  kms <- kmeans(clust_dat, centers = i)
  sil <- silhouette(kms$cluster, dist(clust_dat))
  sil_width[i] <- mean(sil[, 3])
}

# Code for barplot and proportions table 
OralTox2 %>% group_by(V1025) %>% summarize(number_rows = n()) 
barplot(table(OralTox2$V1025), col='#7CE3D8')

# Code for linear regression and confusion matrix 

qsar_oral_toxicity <- read.csv('qsar_oral_toxicity.csv')
names(qsar_oral_toxicity) <- c("x1")
OralTox <- as.data.frame(str_split_fixed(qsar_oral_toxicity$x1, ";", 1025))
OralTox$V1025 <- factor(OralTox$V1025, levels=c("negative","positive"))
split_row <- round(nrow(OralTox)*.60, 0)
train <- OralTox[1:split_row, ]
test <- OralTox[(split_row+1):nrow(OralTox), ]

model <- glm(V1025 ~ ., family="binomial", data=train)
summary(model)
p <- predict(model, newdata=test, type="response")
classes <- ifelse(p>=0.5, 1, 0)
p_class <- factor(classes, labels=c("negative","positive"))

confusionMatrix(data=p_class, reference=test$V1025) 
