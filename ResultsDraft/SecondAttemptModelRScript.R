#Reading in and preparing the data for analysis
library(tidyverse) # for data manipulation and quick data reading and writing
library(metafor)
library(readxl) # for importing Excel files
library(dplyr)
data <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/TestData.csv")

empty_columns <- c('LRR', 'LRR_var')
data[ , empty_columns] <- NA

#Assigning variables n1i, n2i, m1i, m2i, sd1i, sd2i
pairs = t(combn(nrow(data), m = 2))

for (i in nrow(pairs)) {
  
  #Assignment of variables, comparing every condition to 'Conventional'
  a <- pairs[i,1]
  b <- pairs[i,2]
  paperone <- data[a,]
  papertwo <- data[b,]
  
  if (paperone$Management == "Conventional"){
    n1i <- paperone$n1i
    m1i <- paperone$m1i
    sd1i <- paperone$sd1i
  }
  
  if (papertwo$Management != "Conventional"){
    n2i <- papertwo$n1i
    m2i <- papertwo$m1i
    sd2i <- papertwo$sd1i
  }
  
  #Calculating effect sizes with assigned variables
  output <- escalc(measure="ROM", 
                   n1i=n1i,
                   n2i=n2i,
                   m1i=m1i,
                   m2i=m2i,
                   sd1i=sd1i,
                   sd2i=sd2i,
                   var.names=c("LRR", "LRR_var"))
  
  data$LRR <- cbind(data, output$LRR)
  data$LRR_var <- cbind(data, output$LRR_var)
}

