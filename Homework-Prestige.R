# 0) Setup
library(carData)
library(ggplot2)

data("Prestige")
str(Prestige)        # Have a look at variables
summary(Prestige)

# 1) Basic model with measured predictors
m1 <- lm(prestige ~ income + education, data = Prestige)
summary(m1)

# 2) Add a categorical predictor (dummy variables)
# type is a factor with levels "bc", "prof", "wc"
# R will create dummies automatically with treatment coding (reference = first level by default)
Prestige$type <- relevel(Prestige$type, ref = "bc")  # set your preferred reference
m2 <- lm(prestige ~ income + education + type, data = Prestige)
summary(m2)

# 3) Interpreting coefficients with dummy variables
# - Intercept: expected prestige for the reference category (bc) when measured predictors = 0
# - typeprof and typewc: difference from the reference category, holding other variables constant

# 4) Model diagnostics / validation (simple checks)
par(mfrow = c(1,2))
plot(m2, which = 1)   # Residuals vs Fitted (linearity/variance)
plot(m2, which = 2)   # Normal Q-Q (residual normality)
par(mfrow = c(1,1))

# Optional: simple predictive check (train/test split)
set.seed(123)
n <- nrow(Prestige)
idx <- sample(seq_len(n), size = floor(0.7*n))
train <- Prestige[idx, ]
test  <- Prestige[-idx, ]

m3 <- lm(prestige ~ income + education + type, data = train)
pred <- predict(m3, newdata = test)
# RMSE
sqrt(mean((test$prestige - pred)^2))
#this checks for overfitting by regressing on a portion of the sample, and then comparing with the rest of it

# 5) Reporting essentials (template bullets)
# - Briefly justify variable choices (income, education, and type)
# - State reference category and interpret dummy coefficients
# - Comment on model fit (R^2 / adj. R^2) and diagnostics
# - Provide a short, plain-English conclusion

#response variable of interest is prestige
ggpairs(Prestige)
#education and income appear to have strong relationships with prestige; income may be logarithmic
#census is not an explanatory variable of interest, as it does not have any practical meaning
#the means of prestige appear different for the different work types, so this will be included in regression

model1 = lm(prestige ~ income + education + type + I(log(income)) + income*education + I(education^2),
            data = Prestige)
summary(model1)
model2 = update(model1,~. - I(education^2))
summary(model2)
model3 = update(model2,~. - income:education)
summary(model3)
#model4 = update(model3,~. + I(log(income)):education)
#summary(model4)
#tested to see if the interactive term with log(income) was statistically significant, but it wasn't so we
# continue on as it was before
model4 = update(model3,~. - income)
summary(model4)
model5 = update(model4,~. - type)
summary(model5)
#from this regression, we can see that education is positively correlated with prestige, where an extra year
# in education leads to a 4 point increase in prestige (descriptive, not causal). Additionally, income and
# prestige also have a positive relationship, in the form of a linear-log model. The intercept cannot be
# interpreted in this regression, as there are no individuals who have 0 education (in this experiment), and
# prestige cannot be negative. The model has a very high R2, at 0.831, meaning that this model explains 83%
# of the variance in prestige across the workers. With only 2 explanatory variables, the adjusted R2 is very
# similar as well.

par(mfrow = c(1,2))
plot(model5, which = 1)   # Residuals vs Fitted (linearity/variance)
plot(model5, which = 2)   # Normal Q-Q (residual normality)
par(mfrow = c(1,1))
#from this, there appears to generally be homoscedasticity and no problems with regressing
set.seed(126)
n <- nrow(Prestige)
idx <- sample(seq_len(n), size = floor(0.7*n))
train <- Prestige[idx, ]
test  <- Prestige[-idx, ]

model6 <- lm(prestige ~ education + I(log(income)), data = train)
prediction <- predict(model6, newdata = test)
# RMSE
sqrt(mean((test$prestige - prediction)^2))
#obtained roughly 7.5, fair prediction