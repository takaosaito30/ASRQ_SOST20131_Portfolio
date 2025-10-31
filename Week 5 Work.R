#Investigate the nature of the relationship in Share Price Study data for Share_Price vs RD and
# Share_Price vs Turnover

library(tidyverse)
library(ggplot2)

companies = read.csv("SHARE_PRICE.csv")
#all variables measured, so we will show scatter plot and simple linear regression for SP vs RD and SP vs T

plot(x=companies$RD,y=companies$Share_Price,pch=21)
#further data analysis necessary
RD_SP = lm(Share_Price ~ RD,data=companies)
summary(RD_SP)
#there is a relationship between amount spent on R&D and the share price; higher R&D correlates with higher
# share price, with strong statistical significance. R^2 is also strong at 0.3118, meaning that R&D
# explains a large portion of the variance of share prices.

plot(x=companies$Turnover,y=companies$Share_Price,pch=21)
#further data analysis necessary
T_SP = lm(Share_Price ~ Turnover,data=companies)
summary(T_SP)
#there is a relationship between turnover and the share price; higher turnover correlates with higher
# share price, with strong statistical significance. This is slightly unintuitive, as higher turnover
# can imply that employees are not having success at the firm, but may be because high turnover is associated
# with high work standards and pressure to succeed. R^2 is also very strong  at 0.5653, meaning that turnover
# explains a large portion of the variance of share prices.


#Response Variable - Dependent variable, variable that is affected in the study
#Explanatory variable - Independent variable, variable that explains the change in
# the response variable
#Measured Variable - A variable that has numerical values
#Attribute Variable - A variable that has descriptive outcomes

#What is ‘Data Analysis Methodology’, and why is this needed when working with
# sample data?
#Data analysis methodology is comprised of two stages, initial data analysis and
# further data analysis (when necessary), aiming to explore relationships between
# variables. It is necessary as initial data analysis (descriptive statistics)
# does not always yield confident results of relationships.

#What are the statistical concepts used to investigate the relationship between a
# measured response variable and an attribute explanatory variable?
#Statistical concepts used between measured response variables and attribute
# explanatory variables include t-testing and ANOVA (analysis of variance), depending
# on how many levels there are in the attribute variable.

#What are the statistical concepts used to investigate the relationship between a
# measured response variable and a measured explanatory variable?
#Between measured variables, regression models are used, either simple or multiple
# depending on the number of explanatory variables of interest.

#Investigate the relationships between the response variable and from your point of
# view regarded as worth of attention set of explanatory variables.

supermarket = read.csv("SUPERM.csv")
summary(supermarket)

#exploring the relationship between the number of visits (measured response variable) and
# gender (attribute explanatory variable), as well as amount (measured R.V.) and income (measured E.V.)

summary(supermarket$VISITS)
summary(supermarket$VISITS[supermarket$GENDER == 0]) #female
summary(supermarket$VISITS[supermarket$GENDER == 1]) #male
#all quartiles are the same between men and women, but women's mean number of visits is slightly more.
#further data analysis is required as the means are not equal (although they are very similar)

#t-test comparing male and female groups, H0: mean(male) = mean(female), Ha: not equal, 5% sig. level
t.test(supermarket$VISITS[supermarket$GENDER == 0], supermarket$VISITS[supermarket$GENDER == 1])

#p-value = 0.1178 > 0.05, therefore we fail to reject the null hypothesis at 5% sig. level
#thus our conclusion is that we cannot say that there is a relationship between gender and number of visits

plot(x=supermarket$INCOME,y=supermarket$AMOUNT,pch=21)
#no particular relationship can be established from scatter plot, relationship appears weak

#further data analysis, simple regression
reg = lm(AMOUNT ~ INCOME,data=supermarket)
summary(reg)
#although p-value of coefficient of income is < 0.05, the R^2 is only 0.012, meaning that income does not
# explain much of the variance of the amount of money spent.