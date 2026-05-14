#Explain what statistical procedures used to investigate the nature of any relationship that may exist
# between a measured response variable and a measured explanatory variable? How is the strength of this
# relationship initially assessed?

#When looking at the relationship between a measured response variable and a measured explanatory variable,
# we can do initial data analysis to compare descriptive statistics. This involves finding a linear
# regression model and considering the R^2 (whether it is 0, 1, or somewhere in between), which measures the
# strength of this relationship (how much of the variance the explanatory variable accounts for).
#Often, it will be necessary to conduct further data analysis, which involves hypothesis testing such as
# the t-test and F-test.


#Explain what role do R2 and R2 adjusted play in validating competing regression models?

#The R2 and adjusted R2 act as measures of fit between a regression model and the real data. Thus, looking
# at the R2 values of two different models (for the same purpose/study), the model with higher R2 is
# generally preferred. However, R2 (as well as adjusted R2) are not absolute measures of how good models are.
# R2 doesn't account for the number of explanatory variables, so adding many variables would typically result
# in a higher R2, at the cost of a complicated model without much extractable meaning. While adjusted R2
# accounts for the number of explanatory variables in the models (the less variables, the better), it still
# does not act as an absolute rule. However, it can be used to assess whether it is worth adding another
# variable or not, as well as other comparative purposes.


#Undertake the Data Analysis for the Supermarket Data Set to investigate the relationships between the
# response variable and from your point of view regarded as worth of attention set of explanatory variables
# that you will use to fit a multiple regression model. Present your points of views about the nature of the
# relationships and give a complete explanation, within the data analysis methodology, of this analysis.

#continuing with the same variables and work from week 5:
supermarket = read.csv("SUPERM.csv")
summary(supermarket)

#exploring the relationship between the amount (measured response variable) and
# age (measured explanatory variable) and income (measured explanatory variable)

summary(supermarket$AMOUNT)
plot(x=supermarket$AGE,y=supermarket$AMOUNT,pch=21)
model = lm(supermarket$AMOUNT ~ supermarket$AGE)
abline(model, lty=2, col=2)
#appears as though there may be heteroscedasticity. slight positive relationship between age and amount.
summary(model)
#F-stat is not statistically significant, so age should likely not be included in model.

plot(x=supermarket$INCOME,y=supermarket$AMOUNT,pch=21)
model1 = lm(supermarket$AMOUNT ~ supermarket$INCOME)
abline(model1, lty=2, col=2)
#appears as though there may be heteroscedasticity. slight positive relationship between age and amount.
summary(model1)
#income is statistically significant at 5% significance level.

#just in case, we will also look at model with both variables included, as potentially there may be
# correlation between the two explanatory variables that make them statistically significant.

model2 = lm(supermarket$AMOUNT ~ supermarket$INCOME + supermarket$AGE)
summary(model2)
#when both variables are included, F-stat indicates that the model cannot be said to be statistically
# significant. Additionally, the explanatory power of this model is very weak as R2<0.01 (less than 1% of variance is explained by
# this model).

#we will now try to include the nchild variable as well, and use the step function to see what the
# statistically significant variables boil down to.
df <- na.omit(supermarket)
model3 = lm(df$AMOUNT ~ df$INCOME + df$AGE + df$NCHILD)
summary(model3)
#model4 = step(model3)
model4 = update(model3,~. - df$AGE)
summary(model4)
model5 = lm(supermarket$AMOUNT ~ supermarket$NCHILD)
summary(model5)
#we see that the number of children is highly statistically significant, with R2 of 0.1125. When we compare
# this to model 3 and 4, we see that while the R2 (and adjusted R2) do increase, the 1-2% increase is not
# worth the increase in the number of explanatory variables (and we see that age is not statistically
# significant at the 5% level).