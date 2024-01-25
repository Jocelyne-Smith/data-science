# Firstly, I call the appropriate packages
library(ggplot2)
library(hrbrthemes)
library(MASS)


y <- read.csv("C:\\Users\\jocel\\Downloads\\gamma-arrivals.csv", header = F, col.names = "time") 
n <- length(y)

MLE<-function(p){
  MLE <- (1/mean(arrival.times[,1]))
}
# using MLE to find the estimated value
out<-nlm(MLE, p=c(0.5), hessian = TRUE)
lambda_hat_MLE <- out$estimate 

# Next, we show how to do the sampling and redo estimation. Basically this will use a for loop.
B <- 2000 # we will sample 2000 samples and estimate 2000 times
# we will save the bootstrap results in this array, and this array is initialized as 0 vector
lambda_hat_MLE_Bootstrap1 <- rep(0,B)
for(i in 1:B) # for loop, we will repeat the experiment B times
{
  # get a sample of size n, with parameter lambda_hat_MLE
  y <- rgamma(n, lambda_hat_MLE)
  # for this new sample, find the MLE
  out<-nlm(MLE, p=c(0.5), hessian=TRUE)
  # save the estimation result of the i-th iteration
  lambda_hat_MLE_Bootstrap1 [i]<- out$estimate
}

hist(lambda_hat_MLE_Bootstrap1, main="sampling distribution of Bootrapped MLE", xlab = "estimated value", breaks=50, prob=TRUE)
curve(dnorm(x, mean=mean(lambda_hat_MLE_Bootstrap1), sd=
              sd(lambda_hat_MLE_Bootstrap1)),col='blue',add=TRUE) 





# using MOM to find estimated value
MOM<-function(p){
  MOM <- mean(arrival.times[,1])/var(arrival.times[,1])
}
out<-nlm(MOM, p=c(0.5), hessian = TRUE)
lambda_hat_MOM <- out$estimate 

# Next, we show how to do the sampling and redo estimation. Basically this will use a for loop.
B <- 2000 # we will sample 2000 samples and estimate 2000 times
# we will save the bootstrap results in this array, and this array is initialized as 0 vector
lambda_hat_MOM_Bootstrap1 <- rep(0,B)
for(i in 1:B) # for loop, we will repeat the experiment B times
{
  # get a sample of size n, with parameter lambda_hat_MOM
  y <- rgamma(n, lambda_hat_MOM)
  # for this new sample, find the MOM
  out<-nlm(MOM, p=c(0.5), hessian=TRUE)
  # save the estimation result of the i-th iteration
  lambda_hat_MOM_Bootstrap1 [i]<- out$estimate
}

hist(lambda_hat_MOM_Bootstrap1, main="sampling distribution of Bootrapped MOM", xlab = "estimated value", breaks=50, prob=TRUE)
curve(dnorm(x, mean=mean(lambda_hat_MLE_Bootstrap1), sd=
              sd(lambda_hat_MLE_Bootstrap1)),col='blue',add=TRUE) 






# MLE confidence interval
# find the 2.5% and 97.5% quantiles
delta <- quantile(lambda_hat_MLE_Bootstrap1, probs=c(0.025,0.975), names=FALSE)
# find the upper and lower quantiles
delta_up <- delta [2]
delta_down<- delta [1]
hist(lambda_hat_MLE_Bootstrap1, main="sampling distribution of boot_lambda_hat_MLE1 - lambda_hat_MLE", xlab
     = "difference", breaks=50, prob=TRUE)
abline(v= delta_down, col = "blue")
abline(v= delta_up, col = "red")
# calculate the upper and lower confidence bound for the CI
L <- lambda_hat_MLE - delta_up
U <- lambda_hat_MLE - delta_down
# the output:
L
U




# MOM confidence interval
diff <-lambda_hat_MLE_Bootstrap1 - lambda_hat_MLE
# find the 2.5% and 97.5% quantiles
delta <- quantile(diff, probs=c(0.025,0.975), names=FALSE)
# find the upper and lower quantiles
delta_up <- delta [2]
delta_down<- delta [1]
hist(diff, main="sampling distribution of boot_lambda_hat_MOM1 - lambda_hat_MOM", xlab
     = "difference", breaks=50, prob=TRUE)
abline(v= delta_down, col = "blue")
abline(v= delta_up, col = "red")
# calculate the upper and lower confidence bound for the CI
L <- lambda_hat_MLE - delta_up
U <- lambda_hat_MLE - delta_down
# the output:
L
U

