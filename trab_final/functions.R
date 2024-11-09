library(ggplot2)
library(dplyr)
library(tidyr)

boxplot<-function(data, vars, title = ""){
  # if(!all(vars %in% names(data))){
  #   stop("Há Variável não presente no banco")
  # }
  
  data_long<-data |> 
    select(all_of(vars)) |> 
    pivot_longer(cols = everything(), names_to = "Variável", values_to = "Valor")
  
  p <-ggplot(data_long, aes(x=Variável, y= Valor)) +
               geom_boxplot(fill= "lightblue", color = "black") +
               theme_minimal() +
               labs(
                 title = title,
                 x = "Variáveis",
                 y = "Valores") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
             
             return(p)
}



#Embrace Syntax ({{ }})

boxplot(ciano, c("chlorophyll_a", "total_nitrogen"))

hist(ciano$chlorophyll_a)


#numero de bins
bins_sug = nclass.Sturges(ciano$chlorophyll_a)


ggplot(ciano, aes(x=chlorophyll_a))+
  geom_histogram(bins=bins_sug, fill= "#55c53e", color="black")+
  labs(title = "Histograma", x="Conc. Clorofila A", y='Frequência')+
  theme_minimal()
  







