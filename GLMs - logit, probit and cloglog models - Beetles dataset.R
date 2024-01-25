## data
data("BeetleMortality", package = "glmx")

summary(model)

model_logit = glm(cbind(died, n - died) ~ dose, data = BeetleMortality, family = binomial(link = "logit"))
summary(model_logit)

model_probit = glm(cbind(died, n - died) ~ dose, data = BeetleMortality, family = binomial(link = "probit"))
summary(model_probit)

model_cloglog = glm(cbind(died, n - died) ~ dose, data = BeetleMortality, family = binomial(link = "cloglog"))
summary(model_cloglog)


## various standard binary response models
m <- lapply(c("logit", "probit", "cloglog"), function(type)
  glm(cbind(died, n - died) ~ dose, data = BeetleMortality, family = binomial(link = type)))

summary(m)
## visualization
plot(I(died/n) ~ dose, data = BeetleMortality)
lines(fitted(m[[1]]) ~ dose, data = BeetleMortality, col = 2)
lines(fitted(m[[2]]) ~ dose, data = BeetleMortality, col = 3)
lines(fitted(m[[3]]) ~ dose, data = BeetleMortality, col = 4)