library(MASS)

data(mammals)
mammals 

plot(mammals$body,mammals$brain,log="xy") 

mammals.lgnml<-glm(log(brain)~log(body),data=mammals, family="gaussian") 
summary(mammals.lgnml)


mammals.gamma<-glm(brain~log(body),Gamma(log),mammals)
summary(mammals.gamma) 

gamma.dispersion(mammals.gamma) 
gamma.shape(mammals.gamma)