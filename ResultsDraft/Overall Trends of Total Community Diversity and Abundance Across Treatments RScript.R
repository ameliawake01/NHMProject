#Loading required packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

#Reading in the data for model outputs
abundance.data <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/TotalCommunityResultsAbundance.csv")
colnames(abundance.data) <- c("Treatment", "RR", "CI.lb", "CI.ub")
diversity.data <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/TotalCommunityResultsDiversity.csv")
colnames(diversity.data) <- c("Treatment", "RR", "CI.lb", "CI.ub")

#Plotting the RR and variation to see if treatments
#appear significantly different by eye
ggplot(data=abundance.data, aes(x=RR, y=reorder(Treatment, RR))) +
  geom_point(size=2.5) +
  ggtitle("Total Community Abundance in Non-Conventional Systems \nCompared with Conventional Systems") +
  xlab("Response Ratio") +
  ylab("Non-Conventional Treatment Compared with Conventional Treatment") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25)

ggplot(data=diversity.data, aes(x=RR, y=reorder(Treatment, RR))) +
  geom_point(size=2.5) +
  ggtitle("Total Community Diversity in Non-Conventional Systems \n Compared with Conventional Systems") +
  xlab("Response Ratio") +
  ylab("Non-Conventional Treatment Compared with Conventional Treatment") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25)



