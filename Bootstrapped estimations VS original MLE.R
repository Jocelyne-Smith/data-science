y <- c(rep(0,14), rep(1,30), rep(2,36), rep(3,68), rep(4, 43), rep(5,43), rep(6, 30), rep(7,14),
       rep(8,10), rep(9, 6), rep(10,4), rep(11,1), rep(12,1))
n <- length(y)

NegLogLike<-function(p){
  NegLogLike <- -(mean(y*log(p))-p-mean(log(factorial(y))))
}
# using MLE to find the estimated value
out<-nlm(NegLogLike, p=c(0.5), hessian = TRUE)
lambda_hat_MLE <- out$estimate 

# Next, we show how to do the sampling and redo estimation. Basically this will use a for loop.
B <- 2000 # we will sample 2000 samples and estimate 2000 times
# we will save the bootstrap results in this array, and this array is initialized as 0 vector
lambda_hat_MLE_Bootstrap1 <- rep(0,B)
for(i in 1:B) # for loop, we will repeat the experiment B times
{
  # get a sample of size n, with parameter lambda_hat_MLE
  y <- rpois(n, lambda_hat_MLE)
  # for this new sample, find the MLE
  out<-nlm(NegLogLike, p=c(0.5), hessian=TRUE)
  # save the estimation result of the i-th iteration
  lambda_hat_MLE_Bootstrap1 [i]<- out$estimate
}
hist(lambda_hat_MLE_Bootstrap1, xlab = "estimated value", breaks=50, prob=TRUE, col = "red")
curve(dnorm(x, mean=mean(lambda_hat_MLE_Bootstrap1), sd=
              sd(lambda_hat_MLE_Bootstrap1)),col='blue',add=TRUE) 


# the difference of the bootstrapped estimations and the original MLE
diff <-lambda_hat_MLE_Bootstrap1 - lambda_hat_MLE
# find the 2.5% and 97.5% quantiles
delta <- quantile(diff, probs=c(0.025,0.975), names=FALSE)
# find the upper and lower quantiles
delta_up <- delta [2]
delta_down<- delta [1]
hist(diff, xlab
     = "difference", breaks=50, prob=TRUE, col = "red")
abline(v= delta_down, col = "blue")
abline(v= delta_up, col = "green")
# calculate the upper and lower confidence bound for the CI
L <- lambda_hat_MLE - delta_up
U <- lambda_hat_MLE - delta_down
# the output:
L
U

###################################################################################

NegLogLike<-function(p){
  NegLogLike <- -(mean(y*log(p))-p-mean(log(factorial(y))))
}
# using MLE to find the estimated value
out<-nlm(NegLogLike, p=c(0.5), hessian = TRUE)
lambda_hat_MLE <- out$estimate 

# Next, we show how to do the sampling and redo estimation. Basically this will use a for loop.
B <- 2000 # we will sample 2000 samples and estimate 2000 times
# we will save the bootstrap results in this array, and this array is initialized as 0 vector
lambda_hat_MLE_Bootstrap1 <- rep(0,B)
for(i in 1:B) # for loop, we will repeat the experiment B times
{
  # get a sample of size n, with parameter lambda_hat_MLE
  y <- rpois(n, lambda_hat_MLE)
  # for this new sample, find the MLE
  out<-nlm(NegLogLike, p=c(0.5), hessian=TRUE)
  # save the estimation result of the i-th iteration
  lambda_hat_MLE_Bootstrap1 [i]<- out$estimate
}
hist(lambda_hat_MLE_Bootstrap1, xlab = "estimated value", breaks=50, prob=TRUE, col = "red")
curve(dnorm(x, mean=mean(lambda_hat_MLE_Bootstrap1), sd=
              sd(lambda_hat_MLE_Bootstrap1)),col='blue',add=TRUE) 


# the difference of the bootstrapped estimations and the original MLE
diff <-lambda_hat_MLE_Bootstrap1 - lambda_hat_MLE
# find the 2.5% and 97.5% quantiles
delta <- quantile(diff, probs=c(0.025,0.975), names=FALSE)
# find the upper and lower quantiles
delta_up <- delta [2]
delta_down<- delta [1]
hist(diff, xlab
     = "difference", breaks=50, prob=TRUE, col = "red")
abline(v= delta_down, col = "blue")
abline(v= delta_up, col = "green")
# calculate the upper and lower confidence bound for the CI
L <- lambda_hat_MLE - delta_up
U <- lambda_hat_MLE - delta_down
# the output:
L
U
