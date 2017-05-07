german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")


colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

View(german_credit)

# orginal response coding 1= good, 2 = bad we need 0 = good, 1 = bad
german_credit$response = german_credit$response - 1 # Run this once
str(german_credit)
german_credit$response = as.factor(german_credit$response)

pcut<-(1/6)
#german_credit$installment_rate <- as.factor(german_credit$installment_rate)
#german_credit$n_people <- as.factor(german_credit$n_people)
#german_credit$n_credits <- as.factor(german_credit$n_credits)
#german_credit$present_resid <- as.factor(german_credit$present_resid)

set.seed(10669365)
subset = sample(nrow(german_credit), nrow(german_credit) * 0.75)
german_credit_train = german_credit[subset, ]
german_credit_test = german_credit[-subset, ]

# 1. Generalized Linear Models (GLM) ####
german_credit_train.glm_Full <- glm(response ~ ., family = binomial(link = "logit"), german_credit_train)
german_credit_train.glm_Null <- glm(response ~ 1, family = binomial(link = "logit"), german_credit_train)

# stepwise Selection Method
german.credit.train.step.aic = step(german_credit_train.glm_Null, 
                                    scope = list(lower = german_credit_train.glm_Null, 
                                                 upper = german_credit_train.glm_Full),direction = "both")

AIC(german.credit.train.step.aic)
BIC(german.credit.train.step.aic)


#### model choose to use
german_credit_model = glm(response ~ chk_acct + duration + purpose + credit_his + saving_acct + 
                            other_debtor + housing + other_install + foreign + installment_rate + 
                            amount + sex + telephone,  family = binomial(link = "logit"), data =  german_credit_train)
summary(german_credit_model)

# assymmetric cost 
creditcost <- function(r, pi) {
  weight1 = 5
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

# Predict on insample Training Data
german_credit.glm.prob.insample <- predict(german_credit_model, type = "response")
german_credit.glm.pred.insample  <- (german_credit.glm.prob.insample > (1/6) ) * 1
table(german_credit_train$response, german_credit.glm.pred.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(german_credit_train$response != german_credit.glm.pred.insample, 1, 0))
creditcost(german_credit_train$response, german_credit.glm.pred.insample)

library("verification")
roc.plot(german_credit_train$response == "1", german_credit.glm.prob.insample)

roc.plot(german_credit_train$response == "1", german_credit.glm.prob.insample)$roc.vol


## Predict on outsample Testing Data
german_credit.glm.prob.outsample <- predict(german_credit_model, german_credit_test, type = "response")
german_credit.glm.pred.outsample  <- (german_credit.glm.prob.outsample > (1/6) ) * 1
table(german_credit_test$response, german_credit.glm.pred.outsample, dnn = c("Truth", "Predicted"))
#mis-classification rate
mean(ifelse(german_credit_test$response != german_credit.glm.pred.outsample, 1, 0))
# cost
creditcost(german_credit_test$response, german_credit.glm.pred.outsample)


library("verification")
roc.plot(german_credit_test$response == "1", german_credit.glm.prob.outsample)

roc.plot(german_credit_test$response == "1", german_credit.glm.prob.outsample)$roc.vol

pred_test <- prediction(german_credit.glm.pred.outsample, german_credit_test$response)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, colorize = TRUE)
as.numeric(performance(pred_test, 'auc')@y.values)

# Cross Validation
library(boot)
# whole data set
german_credit_cv <- glm(response ~ chk_acct + duration + purpose + credit_his + saving_acct + 
                          other_debtor + housing + other_install + foreign + installment_rate + 
                          amount + sex + telephone, family = binomial, german_credit)
cv.result = cv.glm(german_credit, german_credit_cv, creditcost, 10)
cv.result$delta

# Classification Tree ####
library(rpart)
credit.rpart1 <- rpart(formula = response ~ ., data = german_credit_train, method = "class", 
                       parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)))

plotcp(credit.rpart1)
printcp(credit.rpart1)

credit.rpart <- prune(credit.rpart1, cp = 0.012)
plot(credit.rpart)
text(credit.rpart, cex=0.7) # text and plot need to be one after one another
text(credit.rpart)

summary(credit.rpart)

install.packages("tree")
library(tree)
tree1 <- tree(response ~ ., data = german_credit_train)
m1 = prune.misclass(tree1, best = 17)
summary(m1)

#InSample
credit.train.prob.tree1 = predict(credit.rpart, german_credit_train, type = "prob")
credit.train.pred.tree1 = predict(credit.rpart, german_credit_train, type = "class")
table(german_credit_train$response, credit.train.pred.tree1, dnn = c("Truth", "Predicted"))

creditcost(german_credit_train$response, credit.train.prob.tree1)
mean(ifelse(german_credit_train$response != credit.train.pred.tree1, 1, 0))

pred_train_rpart <- prediction(credit.train.prob.tree1[, 2], german_credit_train$response)
perf_train_rpart <- performance(pred_train_rpart, "tpr", "fpr")
plot(perf_train_rpart, colorize = TRUE, main = "ROC Curve: Training Data")
as.numeric(performance(pred_train_rpart, 'auc')@y.values)

#OutSample
credit.test.prob.tree1 = predict(credit.rpart, german_credit_test, type = "prob")
credit.test.pred.tree1 = predict(credit.rpart, german_credit_test, type = "class")
table(german_credit_test$response, credit.test.pred.tree1, dnn = c("Truth", "Predicted"))

creditcost(german_credit_test$response, credit.test.prob.tree1)
mean(ifelse(german_credit_test$response != credit.test.pred.tree1, 1, 0))

pred_test_rpart <- prediction(credit.test.prob.tree1[, 2], german_credit_test$response)
perf_test_rpart <- performance(pred_test_rpart, "tpr", "fpr")
plot(perf_test_rpart, colorize = TRUE, main = "ROC Curve: Testing Data")
as.numeric(performance(pred_test_rpart, 'auc')@y.values)



library("verification")
roc.plot(german_credit_test$response == "1", credit.test.prob.tree1)

roc.plot(german_credit_test$response == "1", credit.test.prob.tree1)$roc.vol

# 2. Generalized Additive Models (GAM) ####

library(mgcv)
credit.gam <- gam(response ~ chk_acct+s(duration)+credit_his+purpose+s(amount)+saving_acct+present_emp+
                                    (installment_rate)+sex+other_debtor+(present_resid)+property+(age)+other_install+
                    housing+(n_credits)+job+(n_people)+telephone+foreign, family = binomial, data = german_credit_train)
summary(credit.gam)
credit.gam$deviance/credit.gam$df.residual
AIC(credit.gam)
BIC(credit.gam)
par(mfrow=c(1,3))
plot(credit.gam, shade = TRUE, seWithMean = TRUE, scale = 0)
par(mfrow=c(1,1))
pcut.gam <- 1/6
prob.gam.in <- predict(credit.gam, german_credit_train, type = "response")
pred.gam.in <- (prob.gam.in >= pcut.gam) * 1
table(german_credit_train$response, pred.gam.in, dnn = c("Observation", "Prediction"))
mean(ifelse(german_credit_train$response != pred.gam.in, 1, 0))
creditcost(german_credit_train$response, pred.gam.in)


library("verification")
roc.plot(german_credit_train$response == "1", prob.gam.in)

roc.plot(german_credit_train$response == "1", prob.gam.in)$roc.vol



#out sample prediction
pcut <- result.gam[index.min, 1]
prob.gam.out <- predict(credit.gam, german_credit_test, type = "response")
pred.gam.out <- (prob.gam.out >= pcut) * 1
table(german_credit_test$response, pred.gam.out, dnn = c("Observation", "Prediction"))
mean(ifelse(german_credit_test$response != pred.gam.out, 1, 0))
creditcost(german_credit_test$response, pred.gam.out)


library("verification")
roc.plot(german_credit_test$response == "1", prob.gam.out)

roc.plot(german_credit_test$response == "1", prob.gam.out)$roc.vol

# 3. Linear Discriminant Analysis (LDA)####
library(MASS)
german_credit_train$response = as.factor(german_credit_train$response)
credit.lda <- lda(response ~ ., data = german_credit_train)
summary(credit.lda)

#In sample
prob.lda.in <- predict(credit.lda, data = credit.train)
pred.lda.in <- (prob.lda.in$posterior[, 2] >= 1/6) * 1
table(german_credit_train$response, pred.lda.in, dnn = c("Obs", "Pred"))
creditcost(german_credit_train$response, pred.lda.in)
mean(ifelse(german_credit_train$response != pred.lda.in, 1, 0))

pred_train_lda <- prediction(prob.lda.in$posterior[, 2], german_credit_train$response)
perf_train_lda <- performance(pred_train_lda, "tpr", "fpr")
plot(perf_train_lda, colorize = TRUE, main = "ROC Curve: Training Data")
as.numeric(performance(pred_train_lda, 'auc')@y.values)

#OutSample
prob.lda.out <- predict(credit.lda, newdata = german_credit_test)
pred.lda.out <- as.numeric((prob.lda.out$posterior[, 2] >= 1/6))
table(german_credit_test$response, pred.lda.out, dnn = c("Obs", "Pred"))
creditcost(german_credit_test$response, pred.lda.out)
mean(ifelse(german_credit_test$response != pred.lda.out, 1, 0))

pred_test_lda <- prediction(prob.lda.out$posterior[, 2], german_credit_test$response)
perf_test_lda <- performance(pred_test_lda, "tpr", "fpr")
plot(perf_test_lda, colorize = TRUE, main = "ROC Curve: Testing Data")
as.numeric(performance(pred_test_lda, 'auc')@y.values)