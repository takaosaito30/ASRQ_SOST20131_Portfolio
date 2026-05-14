# If you don't have carData installed yet, uncomment and run the line below
# install.packages(carData)
library(carData)
data(Salaries)
attach(Salaries)
class(sex)
unclass(sex)

options("contrasts")

contrasts(sex)
contrasts(discipline)
contrasts(rank)

# average salary values for each sex group
suppressPackageStartupMessages(library(dplyr))
Salaries %>% 
  select(salary, sex) %>%   
  group_by(sex) %>% 
  summarise(mean=mean(salary))

# regression model 
lm(salary ~  sex)

# If you don't have GGally installed yet, uncomment and run the line below
# install.packages(GGally)
suppressPackageStartupMessages(library(GGally))
ggpairs(Salaries)

# model_1 <- lm(salary ~ yrs.since.phd + yrs.service + discipline + sex + rank, data = Salaries) #long handed way
model_1 <- lm(salary ~ ., data = Salaries) # full stop, . , implies: all other variables in data that do not already appear in the formula
summary(model_1)

#model_1 <- lm(salary ~ yrs.since.phd + yrs.service + discipline + sex + rank, data = Salaries) # long handed method
model_2 <- update(model_1,~. - sex) # refitting by removing the least significant term
summary(model_2)

qt(0.95, 391)

coef(model_2)

model_2_1 <- lm(salary ~  0 + rank + discipline + yrs.since.phd + yrs.service)
summary(model_2_1)
#While this model does differ from model_2, the coefficients for the non-dummy variables are the same,
# although there is now no intercept (intercept = 0), and instead, all 3 dummy variable categories are
# included. The coefficients of the dummy variable categories are adjusted to account for this, with the
# intercept and original coefficient being added to get the new coefficient. While the t-statistics of the
# variables also remain unchanged, the R^2 and F-statistic (the whole model) increase significantly,
# indicating that the new model explains more variance and is more statistically significant as a whole.

#Interpreting the model, the average salary is highest for professors and lowest for assistant professors,
# all else equal. Male teachers also on average earn more than women, and as more time has passed since
# teachers have done their PhD, their salaries increase (around $534.6 more for every additional year), while
# salaries decrease as teachers are in service for longer (this likely indicates multicollinearity).

#This model can likely be simplified even further, as years since PhD and years since service likely have
# strong correlation, and thus induce multicollinearity. This means that we could remove one of the variables
# from the model, without reducing R^2 significantly.

wine = read.csv("https://raw.githubusercontent.com/egarpor/handy/master/datasets/wine.csv")
summary(wine)
ggpairs(wine)

model1 <- lm(Price ~ WinterRain + AGST + HarvestRain + Age + WinterRain * AGST * HarvestRain, data = wine)
summary(model1)

model2 <- update(model1, ~. -WinterRain:AGST:HarvestRain, data =wine)
summary(model2)

model3 <- update(model2, ~. -AGST:HarvestRain, data = wine)
summary(model3)

model4 <- update(model3, ~. -WinterRain:AGST, data = wine)
summary(model4)

model5 <- update(model4, ~. -WinterRain:HarvestRain, data = wine)
summary(model5)
