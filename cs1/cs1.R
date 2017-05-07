library(MASS)
data(Boston); #this data is in MASS package
colnames(Boston) 
str(Boston)
set.seed(10669365)

subset = sample(nrow(Boston), nrow(Boston) * 0.75)
Boston_train = Boston[subset, ]
Boston_test = Boston[-subset, ]

lm.nullmodel = lm(medv ~ 1, data = Boston_train)
lm.fullmodel = lm(medv ~ ., data = Boston_train)

model.lm = step(lm.nullmodel, scope = list(lower = lm.nullmodel, upper = lm.fullmodel), 
                direction = "both")
AIC(model.lm)

BIC(model.lm)

model_summary = summary(model.lm)
(model_summary$sigma)^2



glm.pi=predict(model.lm)
mean((glm.pi - Boston_train$medv)^2)

glm.pi=predict(model.lm,Boston_test)
mean((glm.pi - Boston_test$medv)^2)


library(boot)
model_2 = glm(medv ~ lstat + rm + ptratio + chas + black + dis + 
                nox + zn + crim + rad + tax, data = Boston)
cv.glm(data = Boston, glmfit = model_2, K = 3)$delta[2]


#tree
library(rpart)
boston.largetree <- rpart(formula = medv ~ ., data = Boston_train, cp = 0.001)
plot(boston.largetree)
plotcp(boston.largetree)
printcp(boston.largetree)
pruned=prune(boston.largetree, cp = 0.006)
plot(pruned)
text(pruned)
mean((predict(boston.largetree) - Boston_train$medv)^2)
mean((predict(pruned) - Boston_train$medv)^2)

mean((predict(boston.largetree, Boston_test) - Boston_test$medv)^2)
mean((predict(pruned, Boston_test) - Boston_test$medv)^2)


#gam
str(Boston)


# GAM model
library(mgcv)
Boston.gam <- gam(medv ~
                    s(crim)+s(zn)+s(indus)+chas+s(nox)+s(rm)+s(age)
                  +s(dis)+rad+s(tax)+s(ptratio)+s(black)+s(lstat),
                  data=Boston_train)

summary(Boston.gam)
par(mfrow=c(4,3))
plot(Boston.gam, shade = TRUE, , seWithMean = TRUE, scale = 0)
par(mfrow=c(1,1))

model.gam <- gam(medv ~
                   s(crim)+zn+s(indus)+chas+s(nox)+s(rm)+age
                 +s(dis)+rad+s(tax)+ptratio+s(black)+s(lstat),
                 data=Boston_train)
gam_summary=summary(model.gam)

AIC(model.gam)
BIC(model.gam)


Boston.gam.mse.train <-
  model.gam$dev/model.gam$df.res


gam.pi=predict(model.gam)
mean((gam.pi - Boston_train$medv)^2)
mean(residuals(model.gam)^2)


gam.pi=predict(model.gam, Boston_test)
mean((gam.pi - Boston_test$medv)^2)


##neural network
library(stats);
library(nnet);



Boston_sd<-data.frame((Boston$crim-mean(Boston$crim))/sd(Boston$crim))
names(Boston_sd)[names(Boston_sd) == 'X.Boston.crim...mean.Boston.crim...sd.Boston.crim.'] <- 'crim'
Boston_sd$zn<-(Boston$zn-mean(Boston$zn))/sd(Boston$zn);
Boston_sd$indus<-(Boston$indus-mean(Boston$indus))/sd(Boston$indus);
Boston_sd$chas<-(Boston$chas-mean(Boston$chas))/sd(Boston$chas);
Boston_sd$nox<-(Boston$nox-mean(Boston$nox))/sd(Boston$nox);
Boston_sd$rm<-(Boston$rm-mean(Boston$rm))/sd(Boston$rm);
Boston_sd$age<-(Boston$age-mean(Boston$age))/sd(Boston$age);
Boston_sd$dis<-(Boston$dis-mean(Boston$dis))/sd(Boston$dis);
Boston_sd$rad<-(Boston$rad-mean(Boston$rad))/sd(Boston$rad);
Boston_sd$tax<-(Boston$tax-mean(Boston$tax))/sd(Boston$tax);
Boston_sd$ptratio<-(Boston$ptratio-mean(Boston$ptratio))/sd(Boston$ptratio);
Boston_sd$black<-(Boston$black-mean(Boston$black))/sd(Boston$black);
Boston_sd$lstat<-(Boston$lstat-mean(Boston$lstat))/sd(Boston$lstat);
Boston_sd$medv<-Boston$medv



set.seed(10669365);
subset = sample(nrow(Boston_sd), nrow(Boston_sd) * 0.75)
Boston_sd_train = Boston_sd[subset, ]
Boston_sd_validation = Boston_sd[-subset, ]

##
set.seed(10669365);
subset = sample(nrow(Boston_sd_train), nrow(Boston_sd_train) * 0.75)
Boston_sd_training = Boston_sd_train[subset, ]
Boston_sd_test = Boston_sd_train[-subset, ]

test_mse<-0
train_mse<-0
layer_size<-0
# train networks with sizes of hidden units ranging from 0 to 20
for (n in 0:20)
{

  train_predict<-0;
  test_predict<-0;
  # for each size, train 10 networks with different random starting points
  for(i in 1:10)
  {
    set.seed(i);
    net<-nnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,size = n, data=Boston_sd_training, rang = 0.00001, 
              linout = TRUE, maxit = 10000, decay = 0, skip = TRUE);
    train_predict<-train_predict+predict(net, Boston_sd_training);
    test_predict<-test_predict+predict(net, Boston_sd_test);
  }
  # average outcomes of 10 networks
  Boston_sd_training$pred_medv<-train_predict/i;
  Boston_sd_test$pred_medv<-test_predict/i;
  # calculate the sum of squared residuals for training and validation sets, SSEs
  test_mse<-rbind(test_mse, mean((Boston_sd_test$medv-Boston_sd_test$pred_medv)^2));
  train_mse<-rbind(train_mse, mean((Boston_sd_training$medv-Boston_sd_training$pred_medv)^2));
  layer_size<-rbind(layer_size,n);
  train_predict<-0;
  test_predict<-0;
}

result<-data.frame(test_mse,train_mse,layer_size)
result = result[result$test_mse > 0,]

View(result)


dev.off()
library(ggplot2)
library(reshape2)
d <- melt(result, id.vars="layer_size")

# Everything on the same plot
ggplot(d, aes(layer_size,value, col=variable)) + 
  geom_point() + 
  geom_line()



test_sse<-0
train_sse<-0
layer_size<-0
decay_value<-0
# train networks with sizes of hidden units ranging from 0 to 20
for (n in 8:12)
{
  
  for (w in c(0, 0.01, 0.001, 0.0001))
  {
    train_predict<-0;
    test_predict<-0;
    # for each size, train 10 networks with different random starting points
    for(i in 1:10)
    {
      set.seed(i);
      net<-nnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,size = n, data=Boston_sd_training, rang = 0.00001, 
                linout = TRUE, maxit = 10000, decay = w, skip = TRUE);
      train_predict<-train_predict+predict(net, Boston_sd_training);
      test_predict<-test_predict+predict(net, Boston_sd_test);
    }
    
    # average outcomes of 10 networks
    Boston_sd_training$pred_medv<-train_predict/i;
    Boston_sd_test$pred_medv<-test_predict/i;
    # calculate the sum of squared residuals for training and validation sets, SSEs
    test_sse<-rbind(test_sse, mean((Boston_sd_test$medv-Boston_sd_test$pred_medv)^2));
    train_sse<-rbind(train_sse, mean((Boston_sd_training$medv-Boston_sd_training$pred_medv)^2));
    layer_size<-rbind(layer_size,n);
    decay_value<-rbind(decay_value,w);
    train_predict<-0;
    test_predict<-0;
  }
}


result2<-data.frame(test_sse,layer_size,decay_value)

result2$decay_value<-as.factor(result2$decay_value)

result2 = result2[result2$layer_size > 0,]

d2 <- melt(result2, id.vars="layer_size")

# Everything on the same plot
ggplot(result2, aes(layer_size,test_sse,group=decay_value, col=decay_value)) + 
  geom_point() + 
  geom_line()



#final model
n<-9;
decay_value<-0.01


net<-nnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,size = n, data=Boston_sd_train, rang = 0.00001, 
          linout = TRUE, maxit = 10000, decay = decay_value, skip = TRUE);

summary(net)
#in sample mse
pred<-predict(net, Boston_sd_train)
mean((Boston_sd_train$medv-pred)^2)

#out sample mse
pred<-predict(net, Boston_sd_validation)
mean((Boston_sd_validation$medv-pred)^2)

