#Another one-way ANOVA

crop.data = read.csv("C:\\Users\\jocel\\Downloads\\crop.data_.anova_\\crop.data.csv")

#effect of fertilizer type on crop yield
one.way = aov(yield ~ factor(fertilizer), data = crop.data)
summary(one.way)

TukeyHSD(one.way)

two.way = aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)

interaction = aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)
# In the output table, the 'fertilizer:density' variable has a low 
# sum-of-squares value and a high p value, which means there is 
# not much variation that can be explained by the interaction between 
# fertilizer and planting density.

blocking = aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking)
# The 'block' variable has a low sum-of-squares value (0.486) and 
# a high p value (p = 0.48), so it's probably not adding much information 
# to the model. It also doesn't change the sum of squares for the 
# two independent variables, which means that it's not affecting 
# how much variation in the dependent variable they explain.
