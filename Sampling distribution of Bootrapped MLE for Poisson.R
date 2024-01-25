library(ggplot)
# Secondly, I read the data from the exel file into R and save it in the variable, 'arrival.times'
arrival.times = read.csv("C:\\Users\\jocel\\Downloads\\gamma-arrivals.csv", header = F, col.names = "time") 

xbar = mean(arrival.times$time)
xbar

lamda_hat = 1/xbar
lamda_hat

ggplot(arrival.times) + 
  geom_histogram(aes(x = time, y = ..density..), binwidth = 20, fill = "red", col = "black") + theme_classic()


x <- seq(1,600, 200)
fit.distribution <- data.frame(x = x, f = lamda_hat^x/factorial(x)*exp(-lamda_hat))

# plot the histogram and the fitted line on top
ggplot(arrival.times) +
  geom_histogram(aes(x = time, y = ..density..), binwidth = 20, col = "black", fill = "red") +
 

hist(arrival.times, main="sampling distribution of Bootrapped MLE for Poisson", xlab = "estimated value", breaks=50, prob=TRUE)
curve(dpois(x, lambda = lamda_hat),col='blue',add=TRUE) 


warnings()
