library(data.table)
library(dplyr)

dados = fread('train.csv')
head(dados)

#O que se está tentando prever é o Demanda_uni_equil (unidades de cada produto que são 
#realmente necessárias por semana), o qual é resultado da subtração 
#Venta_uni_hoy - Dev_uni_proxima. Portanto, não acho pertinente manter essas duas variáveis
dados[,c('Venta_uni_hoy','Dev_uni_proxima')] = NULL

#Venta_hoy e Dev_proxima são a multiplicação das unidades pelos preços, mas
#nesse estudo os preços não são úteis, por isso vou remover eles também
dados[,c('Venta_hoy','Dev_proxima')] = NULL

#Como há poucas semanas, não consigo considerar o efeito da sazonalidade, então
#a variável Semana só enviesaria os resultados, por isso vou removê-la
dados[,'Semana'] = NULL

str(dados)
#Todos os inteiros, exceto o Demanda_uni_equil são na verdade categorias codificadas
#(fatores)
dados = dados %>% mutate_at(c(1:5),as.factor) 


