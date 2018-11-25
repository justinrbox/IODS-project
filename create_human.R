library(dplyr)
library(data.table)
library(stringr)

##Reading the Data

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

str(hd)
dim(hd)

str(gii)
dim(gii)

##Summaries

summary(hd)
summary(gii)

colnames(hd)
[1] "HDI.Rank"                              
[2] "Country"                               
[3] "Human.Development.Index..HDI."         
[4] "Life.Expectancy.at.Birth"              
[5] "Expected.Years.of.Education"           
[6] "Mean.Years.of.Education"               
[7] "Gross.National.Income..GNI..per.Capita"
[8] "GNI.per.Capita.Rank.Minus.HDI.Rank"  

colnames(gii)
[1] "GII.Rank", "Country", "Gender.Inequality.Index..GII.", "Maternal.Mortality.Ratio", "Adolescent.Birth.Rate", "Percent.Representation.in.Parliament", "Population.with.Secondary.Education..Female.", "Population.with.Secondary.Education..Male.", "Labour.Force.Participation.Rate..Female.", "Labour.Force.Participation.Rate..Male." 

##Rename
hd_old = c("HDI.Rank", "Country", "Human.Development.Index..HDI.", "Life.Expectancy.at.Birth", "Expected.Years.of.Education", "Mean.Years.of.Education", "Gross.National.Income..GNI..per.Capita", "GNI.per.Capita.Rank.Minus.HDI.Rank")
hd_new = c("HDI.Rank", "Country", "HDI", "L.Exp.Birth", "Exp.Yr.Ed", "Mean.Yr.Ed", "GNIperCap", "GNIperCap-HDI.Rank")

setnames(hd, old=hd_old, new=hd_new)

colnames(hd)

gii_old = c("GII.Rank", "Country", "Gender.Inequality.Index..GII.", "Maternal.Mortality.Ratio", "Adolescent.Birth.Rate", "Percent.Representation.in.Parliament", "Population.with.Secondary.Education..Female.", "Population.with.Secondary.Education..Male.", "Labour.Force.Participation.Rate..Female.", "Labour.Force.Participation.Rate..Male.")
gii_new = c("GII.Rank", "Country", "GII", "Mat.Mort.Rat", "Ad.Birth.Rate", "%Rep.in.Par", "Pop.w.2nd.Ed.F", "Pop.w.2nd.Ed.M", "Lab.Part.Rate.F", "Lab.Part.Rate.M")

setnames(gii, old=gii_old, new=gii_new)
colnames(gii)

##Mutate

gii <- mutate(gii, Edu2.FM = Pop.w.2nd.Ed.F/Pop.w.2nd.Ed.M)
gii <- mutate(gii, Labo.FM = Lab.Part.Rate.F/Lab.Part.Rate.M)

##Joining the Datasets

human <- inner_join(hd, gii, by = "Country")
dim(human)

##Save

fid = "~/Desktop/Open Data Science Course Stuff/IODS-project/Data/human.csv"
write.csv(human, file=fid, row.names = FALSE)

##Re-Read
read_data <- read.table(fid, header=TRUE, sep=",")