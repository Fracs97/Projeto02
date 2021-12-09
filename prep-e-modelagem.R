library(dplyr)
library(data.table)
library(caret)
library(rpart)
library(kernlab)
library(randomForest)

set.seed(100)

dados = fread('train_enxuto.csv')

str(dados)

#Há fatores demais (880 mil)
dados$Cliente_ID = NULL

#As variáveis preditoras são ID's, e por isso são fatores
dados = dados %>% mutate_at(c(1:4),as.factor)

#Aqui há um grande problema, pois uma mesma combinação de variáveis de entrada gera
#saídas diferentes
sum(duplicated(dados[,c(1:5)]))
#Isso é racionalmente aceitável, pois um mesmo cliente pode fazer pedidos de dimensões 
#diferentes usando as mesmas opções de compra, pois a demanda não é constante.
#O problema é que, na matemática, um x não pode gerar dois y diferentes

#Para resolver isso, vou estimar que uma dada condição de compra sempre gera uma demanda
#constante
dados = distinct_at(dados,c(1:4),.keep_all=T)

dados = distinct(dados)
#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#MODELAGEM

#Como a base de dados é muito grande, para ser possível fazer o treinamento eu criei um 
#modelo para cada produto através de um subset dos dados
sub_prod = function(produto){
  dados_prod = subset(dados, Producto_ID==produto,-4)
}

#LISTA DE FREQUENCIAS DE CADA PRODUTO
data.frame(table(dados$Producto_ID)) %>% arrange(Freq) %>% View()
dados_prod = sub_prod('41843')
dados_prod = sub_prod('5337')
#Separando em treino e teste
tt = createDataPartition(dados_prod$Demanda_uni_equil,p=0.7,list = F)
treino = dados_prod[tt,]
teste = dados_prod[-tt,]
#Para poder usar essa variável na função de avaliação (tornando ela global)

sprintf('Treino: %d, teste: %d',dim(treino)[1],dim(teste)[1])

#Função que automatiza a avaliação do algoritmo
avalia=function(modelo){
  previsoes = predict(modelo,teste[,1:(ncol(teste)-1)])
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
#MAE: 7.71428
#Resíduos: 476.56
#R²: 0.185365

#CUBIST (Regressão linear)
modelo_cub = cubist(treino[,1:3],treino$Demanda_uni_equil,committees = 10)
df_cub = avalia(modelo_cub)
#MAE: 21.7591
#Resíduos: 9030.1
#R²: 0.208888
#Usando neighbors=9

#SVM
modelo_svm = ksvm(Demanda_uni_equil~.,data=treino,kernel='rbfdot')
df_svm = avalia(modelo_svm)
#EPS-BSVR/RBFDOT
#MAE: 0.970225
#Resíduos: 1937.87
#R²: 0.129979

modelo_svm2 = svm(Demanda_uni_equil~.,data=treino,kernel='linear')
df_svm2 = avalia(modelo_svm2)
