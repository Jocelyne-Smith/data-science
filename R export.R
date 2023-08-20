library(ggplot2)
library(dplyr)
library(class)
library(MASS)
library(caret)
library(devtools)
library(countreg)
library(forcats)
library(insuranceData)
library(Hmisc)
install.packages("countreg", repos="http://R-Forge.R-project.org")
#Attaching data for modeling
data(dataCar)
data1 <- dataCar

write.csv(data1, "data1.csv")
#Data Cleaning & Pre-processing
data1$veh_value_cat <- as.numeric(cut2(data1$veh_value, g=5))
data2 <- unique(data1)
data3 <- data2[data2$veh_value > quantile(data2$veh_value, 0.0001), ] 
#data4 <- data3[data3$veh_value < quantile(data3$veh_value, 0.999), ]
#Regrouping vehicle categories
top9 <-c('SEDAN','HBACK','STNWG','UTE','TRUCK','HDTOP','COUPE','PANVN','MIBUS')
data3$veh_body <- fct_other(data3$veh_body, keep = top9, other_level = 'other')
#Converting catagorical variables into factors
names <- c('veh_body' ,'veh_age','gender','area','agecat','veh_value_cat')
data3[,names] <- lapply(data3[,names] , factor)
str(data3)
# based on variable values
newdata <- subset(data3, clm ==1)
df <- newdata[newdata$claimcst0 < quantile(newdata$claimcst0, 0.99), ]
##data partition - original data
data_partition <- createDataPartition(df$claimcst0, times = 1,p = 0.8,list = FALSE)
str(data_partition)
train <- df[data_partition,]
test  <- df[-data_partition,]
#models - Gaussian
model_gauss <- glm(claimcst0 ~ veh_value+veh_body+veh_age+gender+area+agecat,
                  data = train,offset = log(numclaims),family=gaussian(link="log"))
summary(model_gauss)
plot(model_gauss)
test$pred <- predict(model_gauss, newdata=test, type="response")
sqrt(mean((test$pred - test$claimcst0)^2))
write.csv(test,"test_gauss.csv")
# Models - Gamma
model_gamma <- glm(claimcst0 ~ veh_value+veh_body+veh_age+gender+area+agecat,
                  data = train,offset = log(numclaims),family=Gamma(link="log"))
summary(model_gamma)
plot(model_gamma)
test$pred <- predict(model_gamma, newdata=test, type="response")
sqrt(mean((test$pred - test$claimcst0)^2))
write.csv(test,"test_gamma.csv")