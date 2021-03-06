library(tidyverse)
library(tidymodels)
library(GGally)
library(car)
library(sjPlot)


setwd("C:\\Users\\Aishwin Tikku\\Documents\\GitHub\\DAS2021-Group-28")
Data <- read.csv("dataset28.csv")
# The whole data set has 7 factors and 2000 observations.
# The explanatory variables are:
# country – Country of origin
# price – The cost for a bottle of wine
# province – The province or state the wine is from
# title – The title of the wine review
# variety – The type of grape
# winery – The winery that made the wine
# the response variable y is points – The number of points awarded for the wine on a scale of 1-100 (although reviewsare only posted for ratings 80 and above)


#### data processing ####
Data <- Data[, -1]
glimpse(Data)
summary(Data)
Data$country <- as.factor(Data$country)
Data$province <- as.factor(Data$province)
Data$title <- as.factor(Data$title)
Data$variety <- as.factor(Data$variety)
Data$winery <- as.factor(Data$winery)

# Group by points, those greater than 90 are pass, others are fail
Data <- Data %>% mutate(score = ifelse(points > 90, "Pass", "Fail"))
Data$score <- as.factor(Data$score)

#### descriptive statistics ####
Data[, c(2, 3)] %>%
  gather(key = "variable", value = "value") %>%
  ggplot() +
  geom_histogram(aes(x = value), fill = "#80C687", color = "#80C687", alpha = 0.8) +
  facet_wrap(~variable, scales = "free")

# boxplot of points by country
ggplot(data = Data) +
  geom_boxplot(aes(points, country))

# cross-table of country and score
count <- table(Data$country, Data$score)
prop.table(count, margin = 1)

# chi square test
chisq.test(Data$country, Data$score)

# density plot by score
ggplot(data = Data) +
  geom_density(aes(price, group = score, fill = score))


#### glm model ####
fit <- glm(score ~ price + country,
  data = Data,
  family = binomial(link = "logit"))
fit %>% summary()

# NewZealand and Italy are significantly different from others

NewZealand <- Data %>% filter(country == "New Zealand")

glm(score ~ price + province + variety,
  data = NewZealand,
  family = binomial(link = "logit")) %>%
  summary()

Italy <- Data %>% filter(country == "Italy")

glm(score ~ price + province + variety,
  data = Italy,
  family = binomial(link = "logit")) %>%
  summary()

# model with only price
fit1 <- glm(score ~ price,
  data = Data,
  family = binomial(link = "logit")) %>%
  summary()
fit1

plot_model(fit1,
  type = "pred", title = "",
  axis.title = c("price", "Probability of pass"))
