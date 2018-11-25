#1&2
# access the MASS package
library(MASS)
library(dplyr)
library(ggplot2)
library(corrplot)
data("Boston")
boston_scaled <- scale(Boston)



# load the data
data("Boston")

# explore the dataset
str(Boston)
summary(Boston)

#3
# plot matrix of the variables
pairs(Boston)

# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) %>% round(digits = 2)

# print the correlation matrix
cor_matrix

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)

#4
# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)


# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)


# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)


# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]


# MASS and train are available

# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)
classes <- as.numeric(train$crime)
plot(lda.fit, dimen = 2, col = classes, pch = classes)

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

# create test set 
test <- boston_scaled[-ind,]


#6

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)


# lda.fit, correct_classes and test are available

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

# Clustering and distances

data("Boston")
boston_scaled <- scale(Boston)
boston_scaled <- as.data.frame(boston_scaled)

#Distance Calc

dist_eu <- dist(boston_scaled, method="euclidean")
dist_man <- dist(boston_scaled, method="manhattan")
# Euclidean
summary(dist_eu)
# Manhattan
summary(dist_man)


#K-means
km <-kmeans(boston_scaled, centers = 5)
summary(km)

k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled, k)$tot.withinss})
qplot(x = 1:k_max, y = twcss, geom = 'line')

#Finding the optimal number of clusters

km <-kmeans(boston_scaled, centers = 2)
pairs(boston_scaled[3:7], col = km$cluster, lower.panel = NULL)
km <-kmeans(boston_scaled, centers = 3)
pairs(boston_scaled[3:7], col = km$cluster, lower.panel = NULL)

km <-kmeans(boston_scaled, centers = 3)
pairs(boston_scaled[3:7], col = km$cluster, lower.panel = NULL)
km <-kmeans(boston_scaled, centers = 3)
pairs(boston_scaled[3:7], col = km$cluster, lower.panel = NULL)

2 is better
