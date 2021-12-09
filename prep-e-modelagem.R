library(dplyr)
library(data.table)
library(caret)
library(rpart)
library(tidyr)

set.seed(100)

#Os códigos dos clientes são por ordem de chegada, e não tem valor algum por si só, essa variável só possui alguma
#utilidade quando substituída pelos nomes reais dos clientes, que estão no arquivo cliente_tabla.csv
clientes = fread('cliente_tabla.csv')

dados = fread('train_enxuto.csv')

#Removendo as duplicatas para ser possível realizar o join (limitação de ram)
dados = distinct(dados)

dados = dados %>% left_join(clientes)

str(dados)

#As variáveis preditoras são ID's, e por isso são fatores
dados = dados %>% mutate_at(c(1,2,3,4,5,7),as.factor)

#A coluna Cliente_ID não é mais necessária
dados$Cliente_ID = NULL

#Como haviam ID's diferentes para um mesmo cliente, é preciso remover as novas duplicatas
dados = distinct(dados)

#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#Deixando 
#MODELAGEM
#Como a base de dados é muito grande, para ser possível fazer o treinamento eu criei um 
#modelo para cada rota através de um subset dos dados
sub_rota = function(rota){
  dados_rota = subset(dados, Ruta_SAK==rota,-3)
}

#LISTA DE FREQUENCIAS DE CADA ROTA
data.frame(table(dados$Ruta_SAK)) %>% arrange(Freq) %>% View()
dados_rota = sub_rota('2106')

#Separando em treino e teste
tt = createDataPartition(dados_rota$Demanda_uni_equil,p=0.7,list = F)
treino = dados_rota[tt,]
teste = dados_rota[-tt,]

sprintf('Treino: %d, teste: %d',dim(treino)[1],dim(teste)[1])

#Função que automatiza a avaliação do algoritmo
avalia=function(modelo){
  previsoes = predict(modelo,teste[,-4])
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
#Separando por produto
#MAE: 7.71428
#Resíduos: 476.56
#R²: 0.185365

#Separando por rota
#MAE: 13.4776
#Resíduos: 1082.91
#R²: 0.350835

#MAE: 3.27299
#Resíduos: -217.24
#R²: 0.359853

#CUBIST (Regressão linear)
modelo_cub = cubist(treino[,1:3],treino$Demanda_uni_equil,committees = 3)
df_cub = avalia(modelo_cub)
#MAE: 11.2576
#Resíduos: 89179.2
#R²: 0.297063

#SVM
modelo_svm = ksvm(Demanda_uni_equil~.,data=treino,kernel='rbfdot')
df_svm = avalia(modelo_svm)
#EPS-BSVR/RBFDOT
#MAE: 0.970225
#Resíduos: 1937.87
#R²: 0.129979
