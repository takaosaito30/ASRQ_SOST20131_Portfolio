stroke <- read.csv("qunpt-stroke.csv")
library(dplyr)
glimpse(stroke)
stroke$bmi <- as.double(stroke$bmi)
glimpse(stroke)

stroke <- stroke[ , 2:12]
head(stroke)

ntrain = .8 * nrow(stroke)
ntrain

set.seed(123)
split_idx = sample(nrow(stroke), ntrain)
stroke_train = stroke[split_idx, ]
stroke_test = stroke[-split_idx, ]

summary(as.factor(stroke_train$stroke))
# summary(as.factor(stroke_train$heart_disease))

model_1 <- glm(stroke ~ ., 
               data = stroke_train, 
               family = binomial(logit))

summary(model_1)

fit <- model_1$fitted
hist(fit)

G_calc <- model_1$null.deviance - model_1$deviance
Gdf <- model_1$df.null - model_1$df.residual
pscl::pR2(model_1)

qchisq(.95, df = Gdf) 
1 - pchisq(G_calc, Gdf)

anova(model_1, test="Chisq")

model_2 <- update(model_1, ~. -bmi, data = stroke_train)
summary(model_2)

anova(model_2, test="Chisq")

model_3 <- update(model_2, ~. -gender, data = stroke_train)
summary(model_3)

anova(model_3, test="Chisq")

model_4 <- update(model_3, ~. -Residence_type, data = stroke_train)
summary(model_4)

anova(model_4, test="Chisq")

model_5 <- update(model_4, ~. -smoking_status, data = stroke_train)
summary(model_5)

anova(model_5, test="Chisq")

model_6 <- update(model_5, ~. -ever_married, data = stroke_train)
summary(model_6)

anova(model_6, test="Chisq")

model_7 <- update(model_6, ~. -work_type, data = stroke_train)
summary(model_7)

anova(model_7, test="Chisq")

model_8 <- update(model_7, ~. -heart_disease, data = stroke_train)
summary(model_8)

anova(model_8, test="Chisq")

  
link_pr <- round(predict(model_8,  stroke_test, type = "link"), 2)
link_pr  
response_pr <- round(predict(model_8,  stroke_test, type = "response"), 2)
stroke_test$stroke


coefficients(model_8)

  
how_well <- data.frame(response_pr, stroke_test$stroke) %>% 
  mutate(result = round(response_pr) == stroke_test$stroke)

how_well

confusion_matrix <- table(stroke_test$stroke, round(response_pr))
confusion_matrix

accuracy <- function(x){
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

accuracy(confusion_matrix)

fit <- model_8$fitted
hist(fit)

library(survey)
caret::varImp(model_1)
car::vif(model_1)


pscl::pR2(model_1)

plotROC(test$default, predicted)