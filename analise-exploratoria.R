library(data.table)
library(dplyr)
library(ggplot2)

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

#Agrupando os dados por produto, somando a demanda total até hoje e ordenando de forma
#decrescente
#TOTAL DE UNIDADES DEMANDADAS ATÉ HOJE E QUANTIDADE MEDIANA DE UNIDADES POR PEDIDO:
agrupa =  dados %>% group_by(Producto_ID) %>% summarise(total=sum(Demanda_uni_equil),
                                                    mediana=median(Demanda_uni_equil)) %>%
  arrange(desc(total)) %>% mutate(total_p=round(prop.table(total)*100,2))

plot(agrupa$total_p,main='Decaimento das unidades demandadas por produto')
#As unidades demandadas por produto, da maior para a menor, vão caindo aos poucos, em um
#formato de exponencial negativa

sum(agrupa[1:10,'total_p'])
#Em termos percentuais, os 10 primeiros produtos representam 27.32% de todas as unidades
#da demanda. São eles, em forma gráfica:

ggplot(data=agrupa[1:10,]) + geom_bar(aes(reorder(Producto_ID,-total_p),total_p,fill=Producto_ID),
                    stat='identity') + ggtitle('Top 10 produtos mais vendidos (27.32% do total)')+
  theme(plot.title = element_text(size=20,hjust=0.5)) + ylab('Percentual do total de unidades') +
  xlab('ID do produto')


#Agrupando os dados por Cliente, somando a demanda total até hoje e ordenando de forma
#decrescente
#TOTAL DE UNIDADES POR CLIENTE:
agrupa =  dados %>% group_by(Cliente_ID) %>% summarise(total=sum(Demanda_uni_equil)) %>%
  arrange(desc(total)) %>% mutate(total_p=round(prop.table(total)*100,5))

#As unidades demandadas por cliente estão muito dispersas, o cliente que pede mais unidades
#é o 653378, com 3.33% do total, que é muito maior que o segundo colocado, com 0.16%

sum(agrupa[1:10,'total_p'])
#Em termos percentuais, os 10 primeiros clientes compram 3.81% de todas as unidades
#da demanda. São eles, em forma gráfica:
ggplot() + geom_bar(data=agrupa[1:10,],aes(reorder(Cliente_ID,-total_p),total_p,fill=Cliente_ID),
                    stat='identity') + ggtitle('Top 10 clientes que compram mais unidades (3.81% do total)')+
  theme(plot.title = element_text(size=20,hjust=0.5))+ ylab('Percentual do total de unidades') +
  xlab('ID do cliente')


#Agrupando os dados por Ruta_SAK, somando a demanda total até hoje e ordenando de forma
#decrescente
#TOTAL DE UNIDADES POR ROTA:
agrupa =  dados %>% group_by(Ruta_SAK) %>% summarise(total=sum(Demanda_uni_equil)) %>%
  arrange(desc(total)) %>% mutate(total_p=round(prop.table(total)*100,5))

plot(agrupa$total_p,main='Decaimento das unidades demandadas por cada rota')
#As unidades demandadas por rota, da maior para a menor, vão caindo aos poucos, em um
#formato de exponencial negativa

sum(agrupa[1:10,'total_p'])
#Em termos percentuais, as 10 primeiras rotas mais usadas representam 4.5% de todas as unidades
#da demanda. São eles, em forma gráfica:
ggplot() + geom_bar(data=agrupa[1:10,],aes(reorder(Ruta_SAK,-total_p),total_p,fill=Ruta_SAK),
                    stat='identity') + ggtitle('Top 10 rotas que geram mais demanda (4.5% do total)')+
  theme(plot.title = element_text(size=20,hjust=0.5))+ ylab('Percentual do total de unidades') +
  xlab('ID da rota')

#Agrupando os dados por Canal_ID, somando a demanda total até hoje e ordenando de forma
#decrescente
#TOTAL DE UNIDADES POR CANAL:
agrupa =  dados %>% group_by(Canal_ID) %>% summarise(total=sum(Demanda_uni_equil)) %>%
  arrange(desc(total)) %>% mutate(total_p=round(prop.table(total)*100,5))

#As unidades vendidas se concentram quase totalmente no canal 1, com 72.1% das unidades

#Em forma gráfica:
ggplot() + geom_bar(data=agrupa,aes(reorder(Canal_ID,-total_p),total_p,fill=Canal_ID),
                    stat='identity') + ggtitle('Unidades demandadas por canal')+
  theme(plot.title = element_text(size=20,hjust=0.5))+ ylab('Percentual do total de unidades') +
  xlab('ID do canal')

#Agrupando os dados por Agencia_ID, somando a demanda total até hoje e ordenando de forma
#decrescente
#TOTAL DE UNIDADES POR AGENCIA:
agrupa =  dados %>% group_by(Agencia_ID) %>% summarise(total=sum(Demanda_uni_equil)) %>%
  arrange(desc(total)) %>% mutate(total_p=round(prop.table(total)*100,5))

plot(agrupa$total_p,main='Decaimento das unidades demandadas por agencia')
#As unidades demandadas por agencia, da maior para a menor, vão caindo aos poucos, em um
#formato de exponencial negativa

sum(agrupa[1:10,'total_p'])
#Em termos percentuais, as 10 primeiras agencias representam 7.22% de todas as unidades
#da demanda. São eles, em forma gráfica:
ggplot() + geom_bar(data=agrupa[1:10,],aes(reorder(Agencia_ID,-total_p),total_p,fill=Agencia_ID),
                    stat='identity') + ggtitle('Top 10 agencias que geram mais demanda (7.22% do total)')+
  theme(plot.title = element_text(size=20,hjust=0.5))+ ylab('Percentual do total de unidades') +
  xlab('ID da agencia')