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

