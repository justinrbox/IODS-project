#Chapter 6: The final frontier!
#Justin Box


library(dplyr); library(tidyr)

# Load BPRS and RATS
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Names of BPRS
names(BPRS)
names(RATS)

# Structure of BPRS
str(BPRS)

# Summary of BPRS
summary(BPRS)
summary(RATS)

# Convert the categorical variables to factors
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Convert the data sets to long form

# Convert to long form and add a week variable to BPRS and a Time variable to RATS
BPRSL <-  BPRS %>%
  gather(key = weeks, value = bprs, -treatment, -subject) %>%
  mutate(week = as.integer(substr(weeks,5,5)))

RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 

#the new data sets and comparison to their wide form versions
names(BPRS); names(BPRSL)
str(BPRS); str(BPRSL)
summary(BPRS); summary(BPRSL)
dim(BPRS); dim(BPRSL)

names(RATS); names(RATSL)
str(RATS); str(RATSL)
summary(RATS); summary(RATSL)
dim(RATS); dim(RATSL)

# Write the Files
write.csv(BPRS, file = "data/BPRS.csv", row.names = FALSE)
write.csv(RATS, file = "data/RATS.csv", row.names = FALSE)

write.csv(BPRSL, file = "data/BPRSL.csv", row.names = FALSE)
write.csv(RATSL, file = "data/RATSL.csv", row.names = FALSE)