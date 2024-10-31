library(readxl)
library(tidyverse)

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













