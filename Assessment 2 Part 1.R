library(ggplot2)
library(GGally)
library(tree)

employee = read.csv("EmployeePerformance.csv")
View(employee)

#expertise must be broken down into dummy variables, as interpretation is difficult
#we will also recode level to dummy variables to ease interpretation, and ensure that type is considered
# a factor (binary) variable by R for when we use it in regression models
class(employee$Type)
class(employee$Level)
class(employee$Expertise)

employee$Type = factor(employee$Type)
employee$Level = factor(employee$Level)
employee$Expertise = factor(employee$Expertise)

#summary of all variables, describe variables and find potential outliers
summary(employee$Sales_Perf)
summary(employee$Creativity)
summary(employee$Mechanical)
summary(employee$Abstract)
summary(employee$Maths)
summary(employee$Type)
summary(employee$Level)
summary(employee$Expertise)
boxplot(employee$Sales_Perf)
boxplot(employee$Creativity)
boxplot(employee$Mechanical)
boxplot(employee$Abstract,  main = "Abstract")
boxplot(employee$Maths)

ggpairs(employee)
#no sparcity issues, as expertise, type, and level are balanced between the different levels.
#from the boxplots, we see that abstract has some outliers on both sides.

#Looking at all of the plots, we see immediately that the mental tests seem to correlate with sales
# performance. Here is what we expect of the coefficients of each variable on sales_performance:
#Creativity +, mechanical +, abstract none? (but might not be any relationship, appears heteroscedastic),
#  maths +, type none?, level +, expertise ? (3 appears quite +, 4 appears quite -, 1 and 2 not much)
#mechanical and level, maths and level, and maths and expertise appear highly correlated based on ggpairs
# as the there is not a lot of overlap between the boxplots
#maths and mechanical
#expertise and level may also have significant correlation, as the graphical contingency tables are quite
# uneven (expertise/level depends on the level/expertise)

#we likely don't need polynomial terms, as the relationships between performance and the variables we have
# appear fairly linear in ggpairs.
#the only interaction term that we will look at is expertise*creativity/mechanical/abstract/maths, as these
# could have compounding effects on performance (e.g. math + logistics good fit, notably high performance)
#we more formally check for these above points using a tree:

tree = tree(Sales_Perf ~ ., data = employee)

plot(tree)
text(tree, pretty = 0)

#the tree indicates that the effect of Maths differs depending on education Level, so interaction term
# possible?
#tree also indicates that creativity only matters when Maths is high, and expertise matters when math is
# high.
#like what we saw earlier, this tree does not give indication that a non-linear (polynomial) term is needed.

#reconciling our logical thought of potential interaction terms and possible interaction terms found through
# the tree, we will only look at Maths*Expertise. this is because I cannot strongly reason why the effect
# of creativity would depend on Maths ability, and although Maths*Level is plausible (a degree isn't useful
# if you do not have the numerical skills that are expected to come with it), we want to avoid overfitting
#considering the small sample size (n=48?). it would be best to focus on as few interactions as possible.

#based on our initial data analysis, none of the possible explanatory variables can be confidently stated to
# not have any correlation to sales performance. Thus, we will take all of the given variables into further
# data analysis, trying different regression models and hypothesis tests.

model1 = lm(Sales_Perf ~  ., data = employee)
summary(model1)
#we immediately see lots of statistically significant variables   
#(Intercept) 83.65770    2.54864  32.824  < 2e-16 ***
#  Creativity  -0.06546    0.07921  -0.826  0.41387    
#Mechanical   0.69435    0.11276   6.158 3.84e-07 ***
#  Abstract    -0.03181    0.16660  -0.191  0.84961    
#Maths        0.26970    0.05717   4.717 3.37e-05 ***
#  Type1        1.61522    0.48689   3.317  0.00205 ** 
#  Level2       4.68240    1.02720   4.558 5.47e-05 ***
#  Level3      10.02595    1.17987   8.498 3.19e-10 ***
#  Expertise2  -0.58234    0.73158  -0.796  0.43110    
#Expertise3   3.07774    0.90468   3.402  0.00162 ** 
#  Expertise4  -1.44663    1.17893  -1.227  0.22755    

#the R2 is 0.9806 and adjusted R2 is 0.9753. These are very very high R2 values, which tells us
# that the model explains almost all of the variance of Sales Performance. The F-stat backs this up, with
# 186.7 >>>>> 4?(check actual number) meaning that our model as a whole is statistically significant.

#however, we can still likely improve this model even further, reducing the number of explanatory variables
# without reducing R2 significantly

#We will start by removing abstract from the model, due to the very high p-value (making it unlikely
# to be statistically different from 0) in two-tailed test.
#It is important to take this step-down one variable at a time, as removing one variable changes coefficients
# and standard errors for the remaining variables as well.

model2 = update(model1,~. - Creativity)
summary(model2)

#we will now remove creativity because there is likely multicollinearity going on. We
# confidently expected creativity to have a positive relationship with performance, but the coefficient is
# negative. Thus without explicitly calculating a one-sided (upper tail?) test, we can say that the p-value
# would be close to 1 and thus not statistically significant.
#edit, not actually THAT confident but still possible multicollinearity and either way weak significance.
model3 = update(model2,~. - Abstract)
summary(model3)

#upon doing these changes, we see that R2 has barely changed (0.9802), and adjusted R2 has actually
# increased (0.9761). This new model predicts just as well as model 1, but with 2 less variables (parsimony).
#we now see that even with one-tailed tests for the appropriate variables (and two-tailed for others), all
# of the other variables remaining are statistically significant at the 5% level. it is to note that
# expertise does not appear to be, but all levels must be kept because expertise 3 is stat. sig.

model4 = update(model3,~. + Maths*Expertise)
summary(model4)

#The interaction adds complexity without clear interpretive gain and the effect appears driven by a single
# level, so I will opt to keep the interaction term out of the model.

#at this point, however, it is important to consider that the sample size is very small. Thus, to avoid
# overfitting, we may have to remove statistically significant variables from our model to ensure that
# the predictive power of our model is preserved, rather than being hyperfocused on the little data we have.
#thus, we attempt to remove expertise and type, which are statistically significant at the 5% level, but
# have lower t-values (and lower p-values) than the other explanatory variables we have left.

model5 = update(model3,~. - Type)
summary(model5)

#nothing changed too notably, so we will continue with what we planned, removing expertise as well.
#we feel alright removing expertise for an additional reason, being that level and expertise may have
# notable correlation, so the slight redundancy makes expertise not worth keeping.

model6 = update(model5,~. - Expertise)
summary(model6)

#now that we have removed type and expertise, we have 4 variables remaining, which the sample size can work
# much more loosely(?) with. we see that notably, the t-value of Level2 has actually decreased, but is still
# statistically significant at the 5% level, as is level3 (very significantly). The R2 has decreased to
# 0.9675, which while sizable, is still very high and is not concerning enough to keep the variables we just
# removed.

#Although we still have some of the pairs of explanatory variables which we noted to potentially have high
# correlation, this is not a particular issue as all of the variables left in the model are statistically
# significant and have coefficients with signs that match our prediction, whereas multicollinearity can
# often lead to wrong signs. This means that despite the overlap of predictive power, the variables have
# enough unique correlation to sales performance to be strong predictors regardless.

#Now that we have our final model, we will go through diagnostics to confirm if assumptions(?) are met for
# the regression to be appropriate.

#normal Q-Q plot to check residual normality
plot(model6, which = 2)
#residuals vs fitted, to check linearity and homoscedasticity
plot(model6, which = 1)

#based on the above plots, assumption of homoscedasticity is generally met, as although not perfect, the
# variance is quite similar across the fitted values, and are centered around 0.
#additionally, residual normality also appears alright, with no systematic curve or extreme deviations.
#A small number of observations lie in the tails of the Q–Q plot; however, these deviations are minor and
# do not indicate serious departures from normality. As such, all observations were retained. This aligns
# with the fact that we did not notice any outliers in our initial data analysis for the remaining variables.


#compare R2, number of variables, etc. (what else should we compare/note of?)

#we must note that because of the quite small sample size, any predictions cannot be held too confidently,
# and we should probably watch out for overfitting