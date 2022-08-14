#Circular bar plot
library(tidyverse)
abundancedata <- read.csv("MeanAbundanceData.csv")

#Set a number of 'empty bars' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(abundancedata$Condition), ncol(abundancedata)) )
colnames(to_add) <- colnames(abundancedata)
to_add$Condition <- rep(levels(abundancedata$Condition), each=empty_bar)
abundancedata <- rbind(abundancedata, to_add)
abundancedata <- abundancedata %>% arrange(Condition)
abundancedata$ID <- seq(1, nrow(abundancedata))

#Get the name and the y position of each label
label_data <- abundancedata
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$ID-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

#Prepare a dataframe for base lines
base_data <- abundancedata %>%
  group_by(Condition) %>%
  summarise(start=min(ID), end=max(ID) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

#Prepare a dataframe for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] +1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

#Make the plot
p <- ggplot(abundancedata, aes(x=as.factor(ID), y=Percentage, fill=Condition)) +
  
  geom_bar(aes(x=as.factor(ID), y=Percentage, fill=Condition), stat="identity", alpha=0.5) +
  
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3, inherit.aes = FALSE ) +
  
  annotate("text", x = rep(max(abundancedata$ID),4), y = c(20,40,60,80), label = c("20", "40", "60", "80"), color="grey", size=3, angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(ID), y=Percentage, fill=Condition), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=ID, y=Percentage+10, label=Species, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour="black", alpha=0.8, size=0.6, inherit.aes = FALSE ) +
  geom_text(data=base_data, aes(x = title, y = -18, label=Condition), hjust=c(1,1,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p
