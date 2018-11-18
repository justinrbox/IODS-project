---
title: "Chapter3"
author: "Justin Box"
date: "11/13/2018"

*Data ref: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt*
---

#Reading the Data
  
student_mat <- read.csv(file = "student-mat.csv", header=TRUE, sep=";")
student_por <- read.csv(file = "student-por.csv", header=TRUE, sep=";")

# Joining the Data
head(student_mat)

library(dplyr)

## common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers

math_por <- inner_join(student_mat, student_por, by = join_by, suffix = c(".math", ".por"))

# explore
colnames(math_por)
glimpse(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(student_mat)[!colnames(student_mat) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

# Mutating Data

library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex") + geom_bar()

write.csv(alc, file = "alc.csv")

#Data Analysis


## access the tidyverse libraries tidyr, dplyr, ggplot2
library(tidyr); library(dplyr); library(ggplot2)

## glimpse at the alc data
glimpse(alc)

## use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

## draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")+ geom_bar()

## produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

## initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")

## initialize a plot of high_use and studytime
g1 <- ggplot(alc, aes(x = high_use, y = studytime, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("studytime")

# initialize a plot of 'high_use'
g3 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by internet access
g3 + facet_wrap("internet") + geom_bar()

# initialize a plot of 'high_use'
g4 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by address
g4 + facet_wrap("address") + geom_bar()

# initialize a plot of 'high_use'
studytime_v_high_use <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by studytime
studytime_v_high_use + facet_wrap("studytime") + geom_bar()

##Logistic Reg

m <- glm(high_use ~ address + absences + studytime + failures, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)



### compute odds ratios (OR)
OR <- coef(m) %>% exp

### compute confidence intervals (CI)
CI <- confint(m) %>% exp

### print out the odds ratios with their confidence intervals
cbind(OR, CI)

##Cross Tab

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)




# tabulate target variable v. predictions

table(high_use = alc$high_use, prediction = alc$prediction)

library(dplyr); library(ggplot2)

# initialize plot of 'high_use' v. 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability , y = high_use, col = prediction))

# define the geom as points and draw the plot
g  +  geom_point()

# tabulate target variable v. predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()


# define loss function 
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]