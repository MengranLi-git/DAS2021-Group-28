library(tidyverse)
library(tidymodels)
library(GGally)
library(car)

setwd("~/Desktop/Data Analysis Skills/Group work 2/Datasets-20210710")
Data <- read.csv("dataset28.csv")
#The whole data set has 7 factors and 2000 observations.
#The explanatory variables are:
# country – Country of origin
# price – The cost for a bottle of wine
# province – The province or state the wine is from
# title – The title of the wine review
# variety – The type of grape
# winery – The winery that made the wine
#the response variable y is points – The number of points awarded for the wine on a scale of 1-100 (although reviewsare only posted for ratings 80 and above)

Data <- Data[,-1]
glimpse(Data)
summary(Data)
Data$country <- as.factor(Data$country)
Data$province <- as.factor(Data$province)
Data$title <- as.factor(Data$title)
Data$variety <- as.factor(Data$variety)
Data$winery <- as.factor(Data$winery)

# density plot
Data[,c(2,3)] %>%
  gather(key = "variable", value = "value") %>%
  ggplot() +
  geom_histogram(aes(x=value), fill="#80C687", color="#80C687", alpha=0.8) +
  facet_wrap(~variable, scales = "free")

ggplot(data = data.frame(Data), mapping = aes(x = Data$country, y = ..count..)) + geom_bar(stat = 'count')
ggplot(data = data.frame(Data), mapping = aes(x = Data$province, y = ..count..)) + geom_bar(stat = 'count')












