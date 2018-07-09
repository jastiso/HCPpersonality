# Fit random forest models
library(ranger)
library(dplyr)
library(randomForest)

# load data
data = read.csv("allData_ic149_fullCor_n810.csv")

# we will first select 100 observations for tuning
set.seed(1)
idx = sample(1:810, size=100)

param_data = data[idx,]
param_data = select(param_data, -Subject)
param_data2 = param_data[ , -grep( "NEO" , names( param_data ) ) ]
param_data2$NEOFAC_C = param_data$NEOFAC_C

test_data = na.omit(data[-idx,])
test_data2 = test_data[ , -grep( "NEO" , names( test_data ) ) ]
test_data2 = na.omit(test_data2)
test_data_C = test_data2
test_data_C$NEOFAC_C = test_data$NEOFAC_C
test_data_A = test_data2
test_data_A$NEOFAC_A = test_data$NEOFAC_A
test_data_O = test_data2
test_data_O$NEOFAC_O = test_data$NEOFAC_O
test_data_N = test_data2
test_data_N$NEOFAC_N = test_data$NEOFAC_N
test_data_E = test_data2
test_data_E$NEOFAC_E = test_data$NEOFAC_E

save(test_data, file='test_data.RData')

# tune B
fit.B = randomForest(NEOFAC_C~., data = param_data2, ntree = 10000, mtry = 3990) # set mtry to p/3 for now
plot(fit.B)
B = 1000
save(fit.B, file='fit_full_B.RData')
# tune mtry# Find Parameters for random forest model

par(mfrow=c(3,1))

ps = seq(from = 1, to = 9000, by = 250)
rf.error.p <- numeric(length(ps))  # set up a vector 
cnt = 0;
for (p in ps)  # repeat the following code inside { } 19 times
{
  print(p)
  cnt = cnt + 1
  fit.rf <-ranger(NEOFAC_C~., data = param_data2, num.trees = B, mtry = p) # set mtry to p/3 for now 
  #plot(fit.rf, col= p, lwd = 3)
  rf.error.p[cnt] <- fit.rf$prediction.error  # collecting oob mse based on B trees
}

plot(ps, rf.error.p, pch=16,
     xlab="mtry",
     ylab="mse of mtry") 
mtry = 4000;

# make models
fit.FAC_C <-ranger(NEOFAC_C~., data = test_data_C, num.trees = B, mtry = mtry) # set mtry to p/3 for now
fit.FAC_C$prediction.error
save(fit.FAC_C, file = 'fit_rf_c.RData')

fit.FAC_A <-ranger(NEOFAC_A~., data = test_data_A, num.trees = B, mtry = mtry) # set mtry to p/3 for now 
fit.FAC_A$prediction.error
save(fit.FAC_A, file = 'fit_rf_a.RData')

fit.FAC_O <-ranger(NEOFAC_O~., data = test_data_O, num.trees = B, mtry = mtry) # set mtry to p/3 for now 
fit.FAC_O$prediction.error
save(fit.FAC_O, file = 'fit_rf_O.RData')

fit.FAC_N <-ranger(NEOFAC_N~., data = test_data_N, num.trees = B, mtry = mtry) # set mtry to p/3 for now 
fit.FAC_N$prediction.error
save(fit.FAC_N, file = 'fit_rf_n.RData')

fit.FAC_E <-ranger(NEOFAC_E~., data = test_data_E, num.trees = B, mtry = mtry) # set mtry to p/3 for now 
fit.FAC_E$prediction.error
save(fit.FAC_E, file = 'fit_rf_e.RData')

