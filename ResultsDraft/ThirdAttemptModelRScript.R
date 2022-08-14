#Loading required packages and importing data
library(tidyverse) # for data manipulation and quick data reading and writing
library(metafor)
library(dplyr)
data <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/TestData.csv")

#Cleaning data and preparing for analysis
data <- subset(data, sd1i != "n/a") #removing n/as
data <- subset(data, m1i != "n/a")
data <- subset(data, n1i != "n/a")  
#data <- as.numeric(data$m1i) #These columns were of the class character
#data <- as.numeric(data$sd1i) #Cant pass character values to binary operator, must be numeric

#Turning data into a list in order to split dataframe by unique Paper ID
data.list <- split(data, data$ID)

#Defining a function to calculate the effect size and variance
calc.effect <- function(paperone, papertwo) {
  
  if (paperone$Management == "Conventional" && papertwo$Management != "Conventional"){
    n1i <- paperone$n1i
    m1i <- paperone$m1i
    sd1i <- paperone$sd1i
    n2i <- papertwo$n1i
    m2i <- papertwo$m1i
    sd2i <- papertwo$sd1i
    print("Condition met.")
    
    #Calculating effect sizes with assigned variables
    effectsize <- escalc(measure="ROM", 
                         n1i=n1i,
                         n2i=n2i,
                         m1i=m1i,
                         m2i=m2i,
                         sd1i=sd1i,
                         sd2i=sd2i,
                         var.names=c("LRR", "LRR_var"))
    return(effectsize)
  } else {
    print("Condition not met.")
    return()
  }
}

#Creating a matrix of all possible combinations of pairs in order to do pairwise comparisons on all of the sites
pairs = t(combn(nrow(data), m = 2))
#Some more data wrangling
pairs <- as.data.frame(pairs)
colnames(pairs) <- c("PaperOneRowNumber", "PaperTwoRowNumber")
pairs$LRR <- 0
pairs$LRR_var <- 0

#Applying function to whole dataset
for (i in 1:nrow(pairs)) {
    print(i)
    #Assigning Paper IDs to variables
    a <- pairs[i,1]
    b <- pairs[i,2]
    print(a)
    print(b)
    paperone <- data[a,]
    papertwo <- data[b,]
    #print(paperone)
    #print(papertwo)

    #Inputting variables into calc.effect function and saving the output
    effect.size <- calc.effect(paperone, papertwo)
    print(effect.size)
    pairs$LRR[i] <- effect.size$LRR
    pairs$LRR_var[i] <- effect.size$LRR_var
}

#Deciding what data to include in the model
#data_subset <- filter(data, col_to_filter=="name_to_filter_by")

#More data wrangling
pairs[pairs==0] <- NA
pairs2 <- pairs[complete.cases(pairs),]

#Constructing the model
mod1 <- rma.mv(yi=pairs2$LRR,
               V=pairs2$LRR_var,
               random=list(~ 1 | ~ 1 | PaperOneRowNumber, ~ 1 | PaperTwoRowNumber),
               slab=PaperOneRowNumber,
               data=pairs2)

#Making a forest plot of the model
forest(mod1)

#Pooling estimates and variances from the model to see overall effect
estimates <- c(coef(mod1))
variances <- c(vcov(mod1))
forest(estimates, variances, slab="Pooled effect")









