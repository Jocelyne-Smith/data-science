# ANOVA

#One-way ANOVA - consider the maximum size of 4 fish each from 3 populations (n=12)

size = c(3,4,5,6,4,5,6,7,7,8,9,10)
pop = c("A","A","A","A","B","B","B","B","C","C","C","C")

boxplot(size ~ pop, data.frame(pop), xlab = "Population",
        ylab = "Size", main = "Fish data")

lm.model = lm(size ~ pop)
summary(lm.model)

# p-value > 0.05 suggests no group means differ from each other and 
# you may be done with that model. If the p-value < 0.05 , then you have at 
# least one group that differs from the other(s) and additional steps need 
# to be taken to quantify those difference

aov.model <- aov(size ~ pop)
summary(aov.model)

# It is worth noting that your categorical variable in the aov() needs to be a factor.
# For example, you may have categorical groups labeled 1-10, but of those labels are 
# numeric or integeter in the eyes of R, then they won't work in aov(). Fortunately,
# the as.factor() wrapper usually does the trick.

# We may want the ANOVA coefficients, which are not included in the summary. 
# Fortunatly, those can easily be had by subsetting the model object.
aov.model$coefficients