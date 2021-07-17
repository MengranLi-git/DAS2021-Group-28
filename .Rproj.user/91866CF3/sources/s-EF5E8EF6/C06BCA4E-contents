library(tidyverse)
library(tidymodels)
library(GGally)
library(car)
library(sjPlot)
library(janitor)

# setwd("~/Desktop/Data Analysis Skills/Group work 2/Datasets-20210710")
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
Data %>%
  tabyl(country,score)%>%
  adorn_percentages()%>%
  adorn_pct_formatting()%>%
  adorn_ns()

# chi square test
chisq.test(Data$country, Data$score)

# density plot by score
ggplot(data = Data) +
  geom_density(aes(price, group = score, fill = score))

Data %>%
  tabyl(variety,score)%>%
  adorn_percentages()%>%
  adorn_pct_formatting()%>%
  adorn_ns()

chisq.test(Data$winery, Data$score)

chisq.test(Data$variety, Data$score)

t <- as.matrix(table(Data$variety))

summary(t)

Data$variety <- as.vector(Data$variety)
Data$variety[which(Data$variety %in% row.names(t)[t<11])] = "other"
Data$variety <- as.factor(Data$variety)

chisq.test(Data$variety, Data$score)

Data %>%
  tabyl(variety,score)%>%
  adorn_percentages()%>%
  adorn_pct_formatting()%>%
  adorn_ns()
#### glm model ####
fit1 <- glm(score ~ price + variety,
  data = Data,
  family = binomial(link = "logit"))
fit1 %>% summary()


fit2 <- glm(score ~ price + country,
            data = Data,
            family = binomial(link = "logit"))
fit2 %>% summary()

#### NewZealand and Italy are significantly different from others ####

Data <- Data %>% mutate(
  country = case_when(
    country == "New Zealand" ~ "New Zealand",
    country == "Italy" ~ "Italy",
    !country %in% c("New Zealand", "Italy") ~ "others"
  )
)

fit3 <- glm(score ~ price + country,
           data = Data,
           family = binomial(link = "logit"))

summary(fit3)

# AIC of fit3 is smaller than fit2
glance(fit2)
glance(fit3)

#### select new zealand and italy to check province ####
#province is not significant
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

fit4 <- glm(score ~ price,
            data = Data,
            family = binomial(link = "logit"))
summary(fit4)
glance(fit4)

plot_model(fit3, show.values = TRUE,
           title = "Odds (Country)", show.p = TRUE)

plot_model(fit4,
  type = "pred", title = "",
  axis.title = c("price", "Probability of pass"))

#### No overdispersion
deviance(fit3)/df.residual(fit3) 

pchisq(summary(fit)$dispersion * fit$df.residual, 
       fit$df.residual, lower = F) 

