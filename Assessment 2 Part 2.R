library(ggplot2)
library(GGally)
library(dplyr)

diabetes = read.csv("Diabetes.csv")
View(diabetes)

diabetes$Diabetes = factor(diabetes$Diabetes)
diabetes$HighBP = factor(diabetes$HighBP)
diabetes$HighChol = factor(diabetes$HighChol)
diabetes$Smoker = factor(diabetes$Smoker)
diabetes$Fruits = factor(diabetes$Fruits)


summary(diabetes$Diabetes)
summary(diabetes$HighBP)
summary(diabetes$HighChol)
summary(diabetes$BMI)
summary(diabetes$Smoker)
summary(diabetes$Fruits)
summary(diabetes$Age)
boxplot(diabetes$BMI,  main = "BMI")
boxplot(diabetes$Age,  main = "Age")

ggpairs(diabetes)

tree1 = tree(Diabetes ~ ., data = diabetes)
plot(tree1)
text(tree1, pretty = 0)

#we have to go to further data analysis for all possible explanatory variables, although some are more likely
# than others


#regression:

#binomial logistic regression must be used (binary response variable + typical OLS assumptions e.g.
# normally distributed residuals(?), homoscedascity not met + unbounded)

#considering non-linear terms and interaction terms, polynomial terms are not useful for the many binary
# variables we have. I cannot think of good reason for age to have a non-linear relationship with diabetes.
#BMI polynomial might make sense (upon particular obesity threshold, diabetes becomes much more likely).
#however, we see from ggpairs that the BMI distribution of those with diabetes is largely an overall shift of
# the distribution of those without diabetes, so the effect of BMI can be considered largely linear.
#with interaction terms, we see that BMI*age would be one to look at from the tree we generated, as they
# repeatedly appear after the other, but other interactions are not only weak but also difficult
# to logically reason how they may be related. Thus, we will see whether BMI*age helps prediction of diabetes
# at a statistically significant level, considering that adding an interaction term makes models more complex

#we first look at the baseline model, which will include no interaction terms but all of the provided
# potential explanatory variables, as all of them require further data analysis (none were 0 correlation)
set.seed(1)
split_idx = sample(nrow(diabetes), 120)
diabetes_train = diabetes[split_idx, ]
diabetes_test = diabetes[-split_idx, ]

logmodel1 = glm(Diabetes ~  ., data = diabetes_train, binomial(logit))
summary(logmodel1)

#in making a more parsimonious model, we will start with removing fruits, as it is the least statistically
# significant and we also saw in our expectation that the correlation was not strong
logmodel2 = update(logmodel1,~. - Fruits)
summary(logmodel2)
#we see that the AIC has indeed decreased, which is a good sign that this model is better when considering
# how it is less complex despite the very slight increase in residual deviance (129.37 to 129.63).

#we will next remove highBP, as it is not statistically significant either.
logmodel3 = update(logmodel2,~. - HighBP)
summary(logmodel3)
#again we see AIC drop, as the model becomes simpler but only sees a slight increase in residual deviance

#we will next remove age, as it is still not statistically significant at the 5% level
logmodel4 = update(logmodel3,~. - Age)
summary(logmodel4)

anova(logmodel3, logmodel4, test = "Chisq")
#we see that this actually increased the AIC due to the residual deviance increasing quite a lot
# (129.96 to 133.65). Although exploratory plots suggested only modest differences in age distributions 
# between outcome groups, likelihood-based model comparison indicated that Age contributed to the fitted
# model after adjustment for other risk factors.
#Thus, although the general rule is to remove age as the p-value of its coefficient is >0.05, we will opt to
# keep it in the model as we see the overall model strength is notably stronger with it remained.
#This is also supported by the ANOVA comparing the model with and without age, as it indicates that it is
# close to statistically significant at the 5% level, and thus is reasonable to be kept considering the AIC
# which is our primary focus/evidence.
#now that we have found our baseline appropriate model, we will see whether the interaction term we
# identified to be potentially useful for prediction, age*BMI, helps significantly as well.

logmodel5 = update(logmodel3,~. + BMI*Age)
summary(logmodel5)
#Very high p-value, low Wald z-value, so not statistically significant (cannot reject null hypothesis)
#Furthermore, adding the interaction term greatly increased the standard error of BMI and Age variables
# as well, making these not statistically significant either.
#Although tree-based analysis suggested possible interaction between Age and BMI, formal testing using
# logistic regression showed no evidence that this interaction improved model fit.
#Therefore, we will not keep the interaction term included, and our final model is logmodel3.


prob = predict(logmodel3, diabetes_test, type = "response")

how_well = data.frame(prob, diabetes_test$Diabetes) %>% 
  mutate(result = round(prob) == diabetes_test$Diabetes)
how_well

confusion_matrix = table(diabetes_test$Diabetes, round(prob))
confusion_matrix

accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
accuracy

summary(diabetes_test$Diabetes)
