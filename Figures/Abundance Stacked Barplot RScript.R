library(ggplot2)
abundancedata <- read.csv("MeanAbundanceData.csv")
ggplot(abundancedata, aes(fill=Species, y=Value, x=Condition)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Mean Bacterial Abundance Across Decreasing Management Intensities") +
  ylab("Percentage (%)")