# ESTUDO DE CASO DOSE-RESPOSTA 

dados<-data.frame(
  dose<-c(0, 2.6, 3.8, 5.1, 7.7, 10.2),
  m<-c(49, 50, 48, 46,49, 50),
  y<-c(0, 6, 16, 24, 42, 44),
  p<-c(0, 0.12, 0.33, 0.52, 0.86, 0.88))


plot(dados$dose, dados$p, main = 'relacao dose X sucesso')

glm_model <- glm(p ~ dose, family = binomial(link = "logit"), weights = m, data = dados)
summary(glm_model)

glm_model1 <- glm(p ~ dose, family = binomial(link = "probit"), weights = m, data = dados)
summary(glm_model1)



#No caso, pude observar que o deviance foi igual tanto para funlik probit e logit
#porem o AIC no modelo da logit foi menor
