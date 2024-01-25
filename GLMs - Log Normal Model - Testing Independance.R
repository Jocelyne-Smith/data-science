library(MASS)
## Independence model of hair and eye color and sex.  
model_A <- loglm(~Hair+Eye+Sex, data=HairEyeColor)
model_A

## Conditional independence
model_B <- loglm(~(Hair + Eye) * Sex, data=HairEyeColor)
model_B

## Joint independence model.  
model_C <- loglm(~Hair*Eye + Sex, data=HairEyeColor)
model_C

anova(model_A, model_B, model_C)