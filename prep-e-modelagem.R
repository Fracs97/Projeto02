library(dplyr)
library(data.table)
library(caret)
library(rpart)

dados = fread('train_enxuto.csv')

str(dados)

#As variáveis preditoras são ID's, e por isso são fatores
dados = dados %>% mutate_at(c(1:5),as.factor)

#Aqui há um grande problema, pois uma mesma combinação de variáveis de entrada gera
#saídas diferentes
sum(duplicated(dados[,c(1:5)]))
#Isso é racionalmente aceitável, pois um mesmo cliente pode fazer pedidos de dimensões 
#diferentes usando as mesmas opções de compra, pois a demanda não é constante
#O problema é que, na matemática, um x não pode gerar dois y diferentes

#Para resolver isso, vou estimar que uma dada condição de compra sempre gera uma demanda
#constante
dados = distinct_at(dados,c(1:5),.keep_all=T)

#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#MODELAGEM

#Separando em treino e teste
sub_prod = function(produto){
  dados_prod = subset(dados, Producto_ID==produto,-5)
}

dados_prod = sub_prod('40985')
tt = createDataPartition(dados_prod$Demanda_uni_equil,p=0.7,list = F)
treino = dados_prod[tt,]
teste = dados_prod[-tt,]
sprintf('Treino: %d, teste: %d',dim(treino)[1],dim(teste)[1])
#tc = trainControl(verboseIter = T)
#modelo = train(Demanda_uni_equil~.,data=treino,method='rpart',trControl=tc)
modelo = rpart(Demanda_uni_equil~.,data=treino)

previsoes = ifelse(previsoes%%1>0.5,round(previsoes,0),previsoes%/%1)
previsoes = predict(modelo,teste[,1:(ncol(teste)-1)])

compara = data.frame(real = teste$Demanda_uni_equil,previsto=previsoes)

plot(compara$real)
points(compara$previsto,col='red')
MAE(previsoes,teste$Demanda_uni_equil)

round(5.5,0)
