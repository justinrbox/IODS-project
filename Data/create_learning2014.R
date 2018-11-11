"Justin Box"

"6.11.28"

"This is Exercise 2"

### Reading dataset Learning 2014

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", header = T, sep = "\t")

"This data is in a table with a header and separated by a tab"

#### Create analysis dataset

### Combining 

library(dplyr)

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")

surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")

strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")


deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$Deep <- rowMeans(deep_columns)


surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$Surf <- rowMeans(surface_columns)


strategic_columns <- select(lrn14, one_of(strategic_questions)) 
lrn14$Stra <- rowMeans(strategic_columns)



keep_columns <- c("gender","Age","Attitude","Deep","Stra","Surf","Points")


learning2014 <- select(lrn14, one_of(keep_columns))

colnames(learning2014)

###Scaling Attitude
lrn14$Attitude <- lrn14$Attitude / 10

###Excluding 0

learning2014 <- filter(learning2014, Points > 0)

write.table(learning2014, file = "learning2014.txt", append = F, quote = T, sep = "\t", eol = "\r", dec = ".", row.names = T, col.names = T, qmethod = c("escape", "double"))

read.table("learning2014.txt")

