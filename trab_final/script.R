library(readxl)
library(tidyverse)
library(MASS)

# teste para qualidade do AR

banco <- read_excel("trab_final/Dados gerais das estações 2023.xlsx")
View(banco)
attach(banco)
glimpse(banco)
banco$pm10<-as.double(banco$pm10)
banco$co<-as.double(banco$co)


banco<-banco |> slice(-1)

banco<-banco |> 
  separate(col = data, into = c("data", "hora"), sep = " ")


cal_media_diaria<-function(df, data_col, vars) {
  banco |> 
    group_by({{ data_col }}) |> 
    summarize(across(all_of(vars), mean, na.rm = TRUE), .groups = "drop")
}

media_diaria<-cal_media_diaria(banco, data, c("pm10", "so2", "no2", "o3","co"))

glimpse(banco)





# media_teste<- banco |> 
#   group_by(data) |> 
#   summarize(varaivel_media = mean(variavel,na.rm = TRUE))


# devido a quantidade de NA a variável pm10 será excluida
media_diaria<-media_diaria |> select(-pm10)

# será removido a linha 58 devido a presença de NA

media_diaria<-media_diaria |> slice(-58)


# No caso da variavel o3, há apenas um valor Na, esse será substituido pela média geral

media_diaria$o3[is.na(media_diaria$o3)]<-mean(media_diaria$o3, na.rm = TRUE)


# verificar o valores minimos estabelecidos para criar a variável 0 e 1 para qualidade do ar



limites_conama<-c(so2=40, no2=200, o3=100 , co=9)



media_diaria<-media_diaria |> 
  mutate(
    aceitavel = if_else(
      rowSums(across(names(limites_conama),~.<=limites_conama[cur_column()])) == length(limites_conama),
      1,0
    )
  )

 # todos os dias estão dentro do limite aceitável - o banco será descartado.
# https://aqicn.org/city/sao-paulo/pt/

# Your token is f6cd930c4f6acd42e01a1e1bc3ff03a33c913d7b
# https://aqicn.org/json-api/doc/ - documentacao API

# https://www.timeseriesclassification.com/dataset.php
# https://archive.ics.uci.edu/datasets

# https://www.kaggle.com/datasets/rukenmissonnier/research-on-algae-growth-in-the-laboratory





#https://catalog.data.gov/dataset/river-cyanobacteria-datasets


## DADOS COM CIANOS BACTERIAS


ciano <- read_excel("trab_final/ciano.xlsx")
View(ciano)
attach(ciano)
ciano$chlorophyll_a<-as.double(ciano$chlorophyll_a)


ciano$chlorophyll_a[is.na(ciano$chlorophyll_a)]<-round(mean(ciano$chlorophyll_a, na.rm = TRUE), 2)

media_diaria$o3[is.na(media_diaria$o3)]<-mean(media_diaria$o3, na.rm = TRUE)


corr<-cor(ciano)

corrplot::corrplot(corr, tl.cex = 0.8)

hist(ciano$chlorophyll_a)


plot(ciano$total_phosphorus)

summary(ciano)

fit <- glm(chlorophyll_a ~ ., family = Gamma(link = "log"), data = ciano)

summary(fit)

qchisq(0.95,44)



step(fit)

#stepAIC(fit)

fit2<-glm(formula = chlorophyll_a ~ total_nitrogen + total_phosphorus + 
            dissolved_oxigen + pH_water + carbon_dioxide_water, family = Gamma(link = "log"), 
          data = ciano)
summary(fit2)





#### diagnostico para o FIT2

## Graficos diagnosticos

# [G1] resíduos deviance vs indices
r<- residuals(fit2,type="deviance")
plot(r,pch="+") # plota os graficos com simbolo + para os pontos
abline(h=c(-2,0,2),lty=3) # inlcui linhas horizontais para melhorar visualizado 

# [G2] envelope simulado
library(hnp) # pacote util para envelope simulado
# note que eh preciso definir o residuo para o envelope. As opcoes sao:
# "deviance", "pearson", "response", "working", "simple", "student", or "standard"
hnp(fit2, resid.type="deviance", halfnormal = F) # melhor. Nao eh half (metade)

# [G3] distacia de Cook
d <- cooks.distance(fit2) # salva distacia de cook em d
plot(d,pch="+") 

# [G4] alavancagem
h<-hatvalues(fit2)
plot(h,pch="+") 

# Varias medidas de influencia:
influence.measures(fit2)



library(fmsb) # para computo do R2 de Nagelkerke (1991)
NagelkerkeR2(fit2) # ruim

library(modEvA)
RsqGLM(fit2) # computo de varios pseudo-R2






# 
# library(auditor)
# 
# lm_audit <- audit(fit2, data = ciano, y = ciano$chlorophyll_a)
# 
# hn_lm <- model_halfnormal(lm_audit)
# 
# plot_halfnormal(hn_lm)
# 
# 
# cd_lm <- model_cooksdistance(lm_audit)
# 
# # plot results
# plot_cooksdistance(cd_lm)













