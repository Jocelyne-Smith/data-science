# I use the built in dataset called 'iris'

# Firstly we view the first entries of the dataset
head(iris)

# For this example, I want to estiamte the corrolation bwtween Petal Length and Petal Width

# Steps to Compute the Bootstrap CI in R:
# 1. Import the boot library for calculation of bootstrap CI and ggplot2 for plotting. 

library(boot) # Import library for bootstrap methods

library(ggplot2) # Import library for plotting

# 2. Create a function that computes the statistic we want to use such as mean, median, correlation, etc. 

corr.fun <- function(data, idx) # Custom function to find correlation between the Petal Length and Width
{
  df = data[idx, ]
  
  c(cor(df[, 3], df[, 4], method = 'spearman')) # In this example we need to find the spearman correlation between the 3rd and 4th columns of dataset
}

# 3. Using the boot function to find the R bootstrap of the statistic. 

set.seed(42) # I set the seed in order to replicate the exact results

bootstrap = boot(iris, corr.fun, R = 1000) # I call the boot function with the dataset and my function and number of loops

bootstrap # I display the result of boot function

# 4. We can plot the generated bootstrap distribution using the plot command with calculated bootstrap.

plot(bootstrap) # I plot the bootstrap sampling distribution using ggplot

# 5. Using the boot.ci() function to get the confidence intervals. 

boot.ci(boot.out = bootstrap,     # I use the function to find the bootstrap Confidence Intervals
        type = c("norm", "basic",
                 "perc", "bca"))


