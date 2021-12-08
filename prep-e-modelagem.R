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
#diferentes usando as mesmas opções de compra, pois a demanda não é constante.
#O problema é que, na matemática, um x não pode gerar dois y diferentes

#Para resolver isso, vou estimar que uma dada condição de compra sempre gera uma demanda
#constante
dados = distinct_at(dados,c(1:5),.keep_all=T)

#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#MODELAGEM


sub_prod = function(produto){
  dados_prod = subset(dados, Producto_ID==produto,-5)
}

#Como a base de dados é muito grande, é preciso criar um modelo para cada produto
#através de um subset dos dados
dados_prod = sub_prod('40985')

#Separando em treino e teste
tt = createDataPartition(dados_prod$Demanda_uni_equil,p=0.7,list = F)
treino = dados_prod[tt,]
teste = dados_prod[-tt,]
#Para poder usar essa variável na função de avaliação (tornando ela global)
attach(teste)
sprintf('Treino: %d, teste: %d',dim(treino)[1],dim(teste)[1])
tc = trainControl(verboseIter = T)

#Função que automatiza a avaliação do algoritmo
avalia=function(modelo){
  previsoes = predict(modelo,teste[,1:(ncol(teste)-1)])
  compara = data.frame(real = teste$Demanda_uni_equil,previsto=previsoes)
  print(sprintf('MAE: %g',MAE(previsoes,teste$Demanda_uni_equil)))
  print(sprintf('Resíduos: %g',sum(compara$real-compara$previsto)))
  return(compara)
}

#RPART
modelo_rpart = rpart(Demanda_uni_equil~.,data=treino)
df_rpart = avalia(modelo)
#MAE=13.66


