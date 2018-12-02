library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
# Link to the metadata (http://hdr.undp.org/en/content/human-development-index-hdi)

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
hd_new = c("HDI.Rank", "Country", "HDI", "Life.Exp", "Edu.Exp", "Mean.Yr.Ed", "GNIperCap", "GNIperCap-HDI.Rank")

setnames(hd, old=hd_old, new=hd_new)

colnames(hd)

gii_old = c("GII.Rank", "Country", "Gender.Inequality.Index..GII.", "Maternal.Mortality.Ratio", "Adolescent.Birth.Rate", "Percent.Representation.in.Parliament", "Population.with.Secondary.Education..Female.", "Population.with.Secondary.Education..Male.", "Labour.Force.Participation.Rate..Female.", "Labour.Force.Participation.Rate..Male.")
gii_new = c("GII.Rank", "Country", "GII", "Mat.Mor", "Ado.Birth", "Parli.F", "Edu2.F", "Edu2.M", "Labo.F", "Labo.M")

setnames(gii, old=gii_old, new=gii_new)
colnames(gii)

##Mutate

gii <- mutate(gii, Edu2.FM = Edu2.F/Edu2.M)
gii <- mutate(gii, Labo.FM = Labo.F/Labo.M)

##Joining the Datasets

human <- inner_join(hd, gii, by = "Country")
dim(human)

##Save

fid = "~/Desktop/Open Data Science Course Stuff/IODS-project/Data/human.csv"
write.csv(human, file=fid, row.names = FALSE)

##Re-Read
read_data <- read.table(fid, header=TRUE, sep=",")

colnames(human)

##Description of the Data

"HDI.Rank" = Country rank in Human Development Index
"Country"            
"HDI" = Human Development Index              
"L.Exp.Birth" = Life expectancy at birth      
"Exp.Yr.Ed" = Expected years of schooling      
"Mean.Yr.Ed" = Mean years of schooling       
"GNIperCap" = Gross national income (GNI) per capita        
"GNIperCap-HDI.Rank" = Gross national income (GNI) per capita - Human Development Index 
"GII.Rank"= Country rank in Gender Inequality Index
"GII" = Gender Inequality Index              
"Mat.Mort.Rat" = Maternal mortality ratio      
"Ad.Birth.Rate" = Adolescent birth rate    
"%Rep.in.Par"  = Percetange of female representatives in parliament       
"Pop.w.2nd.Ed.F"  = Population of women with at least some secondary education
"Pop.w.2nd.Ed.M"  = Population of men with at least some secondary education
"Lab.Part.Rate.F" = Population of women participating in labor
"Lab.Part.Rate.M" = Population of men participating in labor
"Edu2.FM"  = The ratio of Female / Male populations with secondary education (edu2F / edu2M)
"Labo.FM"  = The ratio of Female / Male participation in labour force (work2F / work2M)
----------------------------
# transform the Gross National Income (GNI) variable to numeric
library(stringr)

# remove the commas from GNI and print out a numeric version of it
gni_numeric <- str_replace(human$gni, pattern=",", replace ="") %>% as.numeric

# mutate
human$GNI <- str_replace(human$GNI, ",", "")
human$GNI <- as.numeric(human$GNI)

# keep only these columns
keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- dplyr::select(human, one_of(keep))

# remove rows with missing values 
complete.cases(human)
data.frame(human[-1], comp = complete.cases(human))
human <- filter(human, complete.cases(human))

# remove regions that comprise the last 7 rows
tail(human, n=10L)
last <- nrow(human) - 7
human_ <- human[1:last, ]


rownames(human_) <- human_$Country # add countries as rownames
human_ <- dplyr::select(human_, -Country) # remove the Country variable
dim(human_) # 155 observations and 8 variables

write.csv(human_, file="human.csv", row.names = TRUE) # write data to file
# test <- read.csv(file="human.csv")


