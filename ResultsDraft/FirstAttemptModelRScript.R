#Reading in and preparing the data for analysis
library(tidyverse) # for data manipulation and quick data reading and writing
library(metafor)
library(readxl) # for importing Excel files
library(dplyr)
data <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/RevisedData.csv")
summary(data)
cleandata <- subset(data, sd1i != "n/a") #cleaning up dataframe by removing n/as
cleandata <- subset(cleandata, sd2i != "n/a")
cleandata <- subset(cleandata, m1i != "n/a")
cleandata <- subset(cleandata, m2i != "n/a")
cleandata <- subset(cleandata, n1i != "n/a")  
cleandata <- subset(cleandata, n2i != "n/a")
cleandata[30:31] <- lapply(cleandata[30:31], as.numeric) #These columns were of the class character
cleandata[40:41] <- lapply(cleandata[40:41], as.numeric) #Cant pass character values to binary operator, must be numeric

#calculating effect sizes
output <- escalc(measure="ROM", 
               n1i=cleandata$n1i,
               n2i=cleandata$n2i,
               m1i=cleandata$m1i,
               m2i=cleandata$m2i,
               sd1i=cleandata$sd1i,
               sd2i=cleandata$sd2i,
               var.names=c("LRR", "LRR_var"))

cleandata <- cbind(cleandata, output)

#check distributions to see if there is anything wrong with the data
hist(cleandata$LRR)
hist(cleandata$LRR_var)

#subset the data into different diversity measures (diversity and abundance),
#as well as contrasting management practices (conventional vs organic),
#and also different microbes (bacteria, funghi, eukaryotes etc.)

bacteria_subset <- filter(cleandata, Study == "Bacteria", sd1i != "n/a") #extract rows with bacteria data, not including rows with n/as

#Constructing the model
mod1 <- rma.mv(yi=cleandata$LRR,
               V=cleandata$LRR_var,
               random=list(~ 1 | ID),
               slab=ID,
               data=cleandata)
summary(mod1)

#Making a forest plot of the model
forest(mod1)

#Pooling estimates and variances from the model to see overall effect
estimates <- c(coef(mod1))
variances <- c(vcov(mod1))

forest(estimates, variances, slab="Pooled effect")

#Adding moderators?
cleandata %>%
  count(ID)

#################################################################################
#Extracting data from big dataframe that we need (ID, Study, Site Management, n1i, n2i, m1i, m2i, sd1i, sd2i)

data <- data[ , c("ID", "Study", "Site.1...Management", "n1i", "m1i", "sd1i")]


















