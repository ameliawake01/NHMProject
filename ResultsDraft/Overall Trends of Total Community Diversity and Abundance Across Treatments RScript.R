#Loading required packages
library(ggplot2)

#Reading in the data for model outputs
abundance.data <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/TotalCommunityResultsAbundance.csv")
colnames(abundance.data) <- c("Treatment", "RR", "CI.lb", "CI.ub")
abundance.data2 <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/Fixed+GroupsAbundance.csv")
abundance.data3 <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/CvsNCAbundance.csv")
diversity.data1 <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/TotalCommunityResultsDiversity.csv")
colnames(diversity.data1) <- c("Treatment", "RR", "CI.lb", "CI.ub")
diversity.data2 <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/RandomEffectsDiversityResults.csv")
colnames(diversity.data2) <- c("Group", "NonConventional", "Intermediate", "Organic", "Conservation")
diversity.data3 <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/SoilTypeResponseRatios.csv")
diversity.data4 <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/CvsNCDiversity.csv")
diversity.data5 <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/Fixed+Groups.csv")
diversity.data6 <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/CvsOrgDiversity.csv")

#Plotting the RR and variation to see if treatments
#appear significantly different by eye
ggplot(data=abundance.data, aes(x=RR, y=reorder(Treatment, RR))) +
  geom_point(size=2.5) +
  ggtitle("Total Community Abundance in Non-Conventional Systems \nCompared with Conventional Systems") +
  xlab("Response Ratio") +
  ylab("Non-Conventional Treatment Compared with Conventional Treatment") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25)

ggplot(data=diversity.data1, aes(x=RR, y=reorder(Treatment, RR))) +
  geom_point(size=2.5) +
  ggtitle("Total Community Diversity in Non-Conventional Systems \n Compared with Conventional Systems") +
  xlab("Response Ratio") +
  ylab("Non-Conventional Treatment Compared with Conventional Treatment") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25) +
  geom_vline(xintercept = 0, linetype = "solid", color = "blue", size = 0.5)

#Plotting the RR of each group in each treatment
ggplot(data=diversity.data2, aes(x=reorder(Treatment, ResponseRatio), y=ResponseRatio, color = Group, group = Group)) +
  geom_point(size=3) + 
  #geom_line(linetype = "dotted") +
  ggtitle("Diversity Response Ratios of Different Groups Across \nTreatments") +
  xlab("Treatment") +
  ylab("Response Ratio") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  theme_minimal()

#Plotting the RR of different soil types for total community
ggplot(data=diversity.data3, aes(x=RR, y=reorder(Soil.Type, RR))) +
  geom_point(size=2.5) +
  xlab("Response Ratio") +
  ylab("Soil Type") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25) +
  geom_vline(xintercept = 0, linetype = "solid", color = "blue", size = 0.5)

#Plotting the RR of different microbial groups
ggplot(data=diversity.data4, aes(x=RR, y=reorder(Group, RR))) +
  geom_point(size=2.5) +
  xlab("Response Ratio") +
  ylab("Group") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25) +
  geom_vline(xintercept = 0, linetype = "solid", color = "blue", size = 0.5)

#Plotting the RR of each group in each treatment
ggplot(data=diversity.data5, aes(x=reorder(Climate, ResponseRatio), y=ResponseRatio, color = Group, group = Group)) +
  geom_point(size=3) + 
  #geom_line(linetype = "dotted") +
  ggtitle("Diversity Response Ratios of Microbial Groups Across \nClimate Classifications") +
  xlab("Climate Classification") +
  ylab("Response Ratio") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  theme_minimal()

#Plotting the RR of each group in each treatment
ggplot(data=abundance.data2, aes(x=reorder(Soil.Taxonomy, ResponseRatio), y=ResponseRatio, color = Group, group = Group)) +
  geom_point(size=3) + 
  #geom_line(linetype = "dotted") +
  ggtitle("Abundance Response Ratios of Microbial Groups Across \nSoil Taxonomy") +
  xlab("Soil Taxonomy") +
  ylab("Response Ratio") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  theme_minimal()

#Plotting the RR of different microbial groups
ggplot(data=abundance.data3, aes(x=RR, y=reorder(Group, RR))) +
  geom_point(size=2.5) +
  xlab("Response Ratio") +
  ylab("Group") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25) +
  geom_vline(xintercept = 0, linetype = "solid", color = "blue", size = 0.5)

#Plotting the RR of different microbial groups
ggplot(data=diversity.data6, aes(x=RR, y=reorder(Treatment, RR))) +
  geom_point(size=2.5) +
  xlab("Response Ratio") +
  ylab("Treament") +
  geom_errorbar(aes(xmin=CI.lb, xmax=CI.ub), col = "red", width = 0.25) +
  geom_vline(xintercept = 0, linetype = "solid", color = "blue", size = 0.5)

diversity.data7 <- read.csv("/home/amelia-wake/Documents/NHMProject/ResultsDraft/ProkaryoteExplanatories.csv")
#Plotting the RR of each group in each treatment
ggplot(data=diversity.data7, aes(x=reorder(Explanatory, ResponseRatio), y=ResponseRatio, color = Treatment, group = Treatment)) +
  geom_point(size=3) + 
  #geom_line(linetype = "dotted") +
  ggtitle("Diversity Response Ratios and Explanatory Variables of Prokaryotes Across \nNon-conventional Treatments") +
  xlab("Moderator") +
  ylab("Response Ratio") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  theme_minimal()
