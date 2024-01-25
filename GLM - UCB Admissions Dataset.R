UCBAdmissions  

berk.data = as.data.frame(UCBAdmissions)
berk.data$Gender = relevel(berk.data$Gender, ref='Female')
berk.data$Dept = relevel(berk.data$Dept, ref='F')
berk.sat = glm(Freq~Admit+Gender+Dept, family=poisson(), data=berk.data)
summary(berk.sat)