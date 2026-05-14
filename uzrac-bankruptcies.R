bankrup <- read.csv("Bankruptcies.csv")

set.seed(123)
split_idx = sample(nrow(bankrup), 53)
bankrup_train = bankrup[split_idx, ]
bankrup_test = bankrup[-split_idx, ]

full_model <- glm(Y ~  ., data = bankrup_train, binomial(logit))
summary(full_model)

# Q1) Is this a "good" model worth further consideration?
# H_0:βi=0
# H_1:at least one is different from 0, where i=1,2,3. The decision rule is:
# If Gcalc<Gcrit⇒H0, o.w. H1

G_calc <- full_model$null.deviance - full_model$deviance
Gdf <- full_model$df.null - full_model$df.residual
G_crit <- qchisq(.95, df = Gdf) 
p <- 1 - pchisq(G_calc, Gdf)
G_calc
G_crit
p

# Obtain the output and read the post `R squared in logistic regression`
# https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
# It discusses the McFadden’s R squared as a default ‘pseudo R^2’ 
pscl::pR2(full_model) 

# Q2) How to evaluate individual contribution of the variables used in a logistic regression model? 

anova(full_model, test="Chisq")
# the anova() function compares the following models in sequential order.
# it sequentially compares the smaller model with the next more complex model by adding one variable in each step. 
# Each of those comparisons is done using likelihood ratio test 
# glm(Y~1, binomial(logit), data = bankrup_train) vs. glm(Y~X1, binomial(logit), data = bankrup_train)
# glm(Y~X1, binomial(logit), data = bankrup_train) vs. glm(Y~X1+X2, binomial(logit), data = bankrup_train)
# glm(Y~X1+X2, binomial(logit), data = bankrup_train) vs. glm(Y~X1+X2+X3, binomial(logit), data = bankrup_train)


# model only the intercept
model1 <- glm(Y~1,
            data = bankrup_train, 
            binomial(logit))
# model with intercept + X1
model2 <- glm(Y~X1,              
            data = bankrup_train,
            binomial(logit)) 
# model with intercept + gre + gpa
model3 <- glm(Y ~ X1 + X2,        
            data = bankrup_train,
            binomial(logit)) 
# model containing all variables (full model)
model4 <- glm(Y ~ X1 + X2 + X3,        
            data = bankrup_train,
            binomial(logit)) 

anova(model1, model2, test="LRT")

# the 𝑝-values in the output of summary(full_model) are Wald tests 
# that test the following hypotheses (note that they're interchangeable and the order of the tests does not matter):
#
# H_0: βi=0 (coefficient i is not significant, thus Xi is not important)
# H_1: βi≠0 (coefficient i is significant, thus Xi is important)
#
# X1: glm(Y~X2+X3, data = bankrup_train, binomial(logit)) vs. glm(Y~X1+X2+X3, data = bankrup_train, binomial(logit))
# X2: glm(Y~X1+X3, data = bankrup_train, binomial(logit)) vs. glm(Y~X1+X2+X3, data = bankrup_train, binomial(logit))
# X3: glm(Y~X1+X2, data = bankrup_train, binomial(logit)) vs. glm(Y~X1+X2+X3, data = bankrup_train, binomial(logit))

# Each variable against the full_model containing all variables. 
# Wald tests are an approximation of the likelihood ratio test, and as such 
# we could also do the likelihood ratio tests (LR test).

model1.2f <- glm(Y ~ X1 + X2,
                 data = bankrup_train,
                 binomial(logit)) 
model2.2f <- glm(Y ~ X1 + X3,
                 data = bankrup_train,
                 binomial(logit)) 
model3.2f <- glm(Y ~ X2 + X3,
                 data = bankrup_train,
                 binomial(logit)) 

# LRT test for X3
anova(model1.2f, full_model, test="LRT")
# LRT test for X2
anova(model2.2f, full_model, test="LRT")
# LRT test for X1
anova(model3.2f, full_model, test="LRT")


anova(full_model, test="Chisq")

model_new <- update(full_model, ~. -X3, 
                    data = bankrup_train)
summary(model_new)

exp_X1 <- exp(model_new$coefficients[2])
exp_X2 <- exp(model_new$coefficients[3])

round((exp_X1-1)*100, 2) # A unit increase in the (Retained Earnings / Total Assets) ratio is associated with an increase of 16.25% in the chance of company being solvent
round((exp_X2-1)*100, 2) # A unit increase in the (Earnings Before Interest and Taxes / Total Assets) ratio is associated with an increase of 19.92% in the chance of company being solvent

# assessing the accuracy of the fitted model

library(dplyr)

link_pr <- round(predict(model_new,  bankrup_test, type = "link"), 2)
response_pr <- round(predict(model_new,  bankrup_test, type = "response"), 2)

how_well <- data.frame(response_pr, bankrup_test$Y) %>% 
  mutate(result = round(response_pr) == bankrup_test$Y)
how_well

confusion_matrix <- table(bankrup_test$Y, round(response_pr))
confusion_matrix

# calculate percentage accuracy
accuracy <- function(x){
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

accuracy(confusion_matrix)
