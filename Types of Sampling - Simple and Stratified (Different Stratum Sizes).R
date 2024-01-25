# I construct the population of 100 from 1 to 100
population = 1:100

# I instantiate a value that will be used to store the sample mean values
sample_means = numeric(0)

# I use a for loop to draw the 100 samples
for (i in 1:100) {
  # I take a simple random sample of size 12 from the population without replacement
  sample = sample(population, 12, replace = F, prob = NULL)
  
  # I then find the mean value of the sample and round it off to 2 decimal places
  mean_value = round(mean(sample),2)
  
  # I add the new mean to the list of existing means
  sample_means = c(sample_means, mean_value)
}

# Using a additional package, I construct a histogram showing its normal curve
library(rcompanion)

plotNormalHistogram(sample_means, linecol = "Blue", col =  "Red")


###############################################


# I construct the population of 100 from 1 to 100
population = 1:100

# Stratum 1
S1 = 1:50
# Stratum 2
S2 = 51:100

# I instantiate a value that will be used to store the sample mean values
stratified_sample_means = numeric(0)

for (i in 1:100) {
  # I take a simple random sample of size 6 from stratum 1 and 2, without replacement
  sample1 = sample(S1, 6, replace = F, prob = NULL)
  sample2 = sample(S2, 6, replace = F, prob = NULL)
  
  # I then find the mean value of the two samples, add them together and divide by 2 to find their mean together
  # And then I round it off to 2 decimal places
  mean_value = round((mean(sample1)+mean(sample2)/2),2)
  
  # I add the new mean to the list of existing means
  stratified_sample_means = c(stratified_sample_means, mean_value)
}

# Using a additional package, I construct a histogram showing its normal curve
library(rcompanion)

plotNormalHistogram(stratified_sample_means, linecol = "Blue", col =  "Red")

##################################

# I construct the population of 100 from 1 to 100
population = 1:100

# Stratum 1
S1 = 1:25
# Stratum 2
S2 = 26:50
# Stratum 3
S3 = 51:75
# Stratum 4
S4 = 76:100

# I instantiate a value that will be used to store the sample mean values
stratified_sample_means = numeric(0)

for (i in 1:100) {
  # I take a simple random sample of size 3 from stratum 1, 2, 3 and 4, without replacement
  sample1 = sample(S1, 3, replace = F, prob = NULL)
  sample2 = sample(S2, 3, replace = F, prob = NULL)
  sample3 = sample(S2, 3, replace = F, prob = NULL)
  sample4 = sample(S2, 3, replace = F, prob = NULL)
  
  # I then find the mean value of the four samples, add them together and divide by 4 to find their mean together
  # And then I round it off to 2 decimal places
  mean_value = round((mean(sample1)+mean(sample2)+mean(sample3)+mean(sample4)/4),2)
  
  # I add the new mean to the list of existing means
  stratified_sample_means = c(stratified_sample_means, mean_value)
}

# Using a additional package, I construct a histogram showing its normal curve
library(rcompanion)

plotNormalHistogram(stratified_sample_means, linecol = "Blue", col =  "Red")
