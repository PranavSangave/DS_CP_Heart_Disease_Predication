library(dplyr)
library(gtools)
library(gmodels)
library(ggplot2)
library(class)
library(tidyr)
library(lattice)
library(caret)
library(recipes)
library(rmdformats)
library(tidyverse)
heart <- read.csv("G:/College VIT/Module 4/Data Science/CP/heart.csv")
glimpse(heart)
head(heart, 3)
heart <- heart %>%
  #mutate_if(is.integer, as.factor) %>%
  mutate(cp = as.factor(cp),
         restecg = as.factor(restecg),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         sex = factor(sex, levels = c(0,1), labels = c("female", "male")),
         fbs = factor(fbs, levels = c(0,1), labels = c("False", "True")),
         exang = factor(exang, levels = c(0,1), labels = c("No", "Yes")),
         target = factor(target, levels = c(0,1), labels = c("Health", "Not Health")))

glimpse(heart)
library(dplyr)
library(gtools)
library(gmodels)
library(ggplot2)
library(class)
library(tidyr)
library(lattice)
library(caret)
library(recipes)
library(rmdformats)
library(tidyverse)
heart <- read.csv("G:/College VIT/Module 4/Data Science/CP/heart.csv")
glimpse(heart)
head(heart, 3)
heart <- heart %>%
  #mutate_if(is.integer, as.factor) %>%
  mutate(cp = as.factor(cp),
         restecg = as.factor(restecg),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         sex = factor(sex, levels = c(0,1), labels = c("female", "male")),
         fbs = factor(fbs, levels = c(0,1), labels = c("False", "True")),
         exang = factor(exang, levels = c(0,1), labels = c("No", "Yes")),
         target = factor(target, levels = c(0,1), labels = c("Health", "Not Health")))

glimpse(heart)

colSums(is.na(heart))
prop.table(table(heart$target))
table(heart$target)
set.seed(100)
index <- sample(nrow(heart), nrow(heart)*0.7)
train_heart <- heart[index,]
test_heart <- heart[-index,]
prop.table(table(train_heart$target))

# Create a model
model1 <- glm(formula = target ~ sex + cp +  fbs + thal, family = "binomial", data = train_heart)

# Model summary
summary(model1)

# Create a model without predictor
model_none <- glm(target ~ 1, family = "binomial", data = train_heart)

# Create a model with all predictor
model_all <- glm(target ~ ., family = "binomial", data = train_heart)

# Stepwise regression backward
model_back <- step(object = model_all, direction = "backward", trace = F)

# Stepwise regression forward
model_forw <- step(object = model_all, scope = list(lower = model_none, upper = model_all), direction = "forward", trace = F)

# Stepwise regression both
model_both <- step(object = model_all, scope = list(lower = model_none, upper = model_all), direction = "both", trace = F)






