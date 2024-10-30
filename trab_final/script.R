library(readxl)
library(tidyverse)

# teste para qualidade do AR

banco <- read_excel("trab_final/Dados gerais das estações 2023.xlsx")
View(banco)

banco<-banco |> slice(-1)

banco<-banco |> 
  separate(col = data, into = c("data", "hora"), sep = " ")


cal_media_diaria<-function(df, data_col, vars) {
  banco |> 
    group_by({{data_col}}) |> 
    summarize(across(all_of(vars), mean, na.rm = TRUE), .groups = "drop")
}

media_diaria<-cal_media_diaria(banco, data, c("pm10", "so2", "no2", "o3","co"))

View(media_diaria)





