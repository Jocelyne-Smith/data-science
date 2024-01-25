reg_data<-warpbreaks  
print(head(reg_data))  

library(dplyr)
count(reg_data)

output_result <-glm(formula = breaks ~ wool+tension, data = warpbreaks,family = poisson)  
summary(output_result)
sqrt(mean((output_result$actual - output_result$predicted)^2))

poisson_model <- glm(breaks ~ wool + tension, data = warpbreaks, family = quasipoisson(link = "log"))
summary(poisson_model)

pchisq(poisson_model$deviance, df=poisson_model$df.residual, lower.tail=FALSE)

sqrt(mean((poisson_model$actual - poisson_model$predicted)^2))

as.factor(warpbreaks$wool)
as.factor(warpbreaks$tension)
as.factor(warpbreaks$breaks)

library(MASS)

bin_model <- glm.nb(breaks ~ wool + tension, data = warpbreaks)
summary(bin_model)
sqrt(mean((bin_model$actual - bin_model$predicted)^2))

# Install the package jtools if not already installed
library("jtools")

plot_summs(output_result, poisson_model,bin_model, scale = TRUE, exp = TRUE)

library(AER)
dispersiontest(output_result, trafo=1)