# I instantiate a value that will be used to store the sample mean values
sample_means = numeric(0)
sample_vars = numeric(0)

for (i in 1:5000) {

  sample = rnorm(10,0,1)
  
  sample_means = c(sample_means, mean(sample))
  sample_vars = c(sample_vars, var(sample))
}

library(rcompanion)

hist(sample_means, col =  "Red")
hist(sample_vars, col =  "Blue")