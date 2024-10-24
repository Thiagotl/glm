# Atividade GLM 
data<-read.table("Dados_aula_4.txt",h=T)
attach(data)
fit1<- glm(resp~cov1+cov2+cov3, family=poisson(link="log"))
summary(fit1)

fit2 <- glm(resp~cov1+cov3, family=poisson(link="log"))
summary(fit2)
