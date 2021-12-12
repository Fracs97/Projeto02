library(dplyr)
library(data.table)
library(caret)
library(rpart)
library(mltools)
library(tidyr)

set.seed(100)

#DATA MUNGING
dados = fread('train_enxuto.csv')

#Removendo duplicatas
dados = distinct(dados)

#De acordo com a feature selection feita com o VarImp, a variável Canal_ID não tem importância no modelo
dados$Canal_ID = NULL

#Criando variáveis com as médias de demanda para cada coluna
medias_cliente = dados %>% group_by(NombreCliente) %>% summarise(media_cliente=mean(Demanda_uni_equil))
medias_agencia = dados %>% group_by(endereco) %>% summarise(media_agencia=median(Demanda_uni_equil))

dados = dados %>% left_join(medias_cliente)
dados = dados %>% left_join(medias_agencia)

#Ajustando os tipos das variáveis
dados = dados %>% mutate_at(-c(3,6),as.factor)

cor(dados[,c(3,6)])
#media_cliente e Demanda_uni_equil tem uma correlação de 0.56

#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#MODELAGEM
#Como a base de dados é muito grande, para ser possível fazer o treinamento eu criei um 
#modelo para cada endereço através de um subset dos dados
sub_end = function(end){
  dados_end = subset(dados, endereco==end,-5)
}

#LISTA DE FREQUENCIAS DE CADA ENDEREÇO
data.frame(table(dados$endereco)) %>% arrange(Freq) %>% View()
dados_end = sub_end('2562 MEXICALI PLAZA, BAJA CALIFORNIA NORTE')

#Separando em treino e teste
tt = createDataPartition(dados_end$Demanda_uni_equil,p=0.7,list = F)
treino = dados_end[tt,]
teste = dados_end[-tt,]

sprintf('Treino: %d, teste: %d',dim(treino)[1],dim(teste)[1])

#Função que automatiza a avaliação do algoritmo
avalia=function(modelo){
  previsoes = predict(modelo,teste[,-3])
  compara = data.frame(real = teste$Demanda_uni_equil,previsto=previsoes)
  cat(sprintf('#MAE: %g',MAE(previsoes,teste$Demanda_uni_equil)))
  cat('\n')
  cat(sprintf('#Resíduos: %g',sum(compara$real-compara$previsto)))
  cat('\n')
  cat(sprintf('#R²: %g',cor(compara$real,compara$previsto)**2))
  return(compara)
}

#RPART
modelo_rpart = rpart(Demanda_uni_equil~.,data=treino)
df_rpart = avalia(modelo_rpart)
rmsle(df_rpart$previsto,df_rpart$real)

#MAE: 6.37349
#Resíduos: -5950.51
#R²: 0.602505

#TUNING

