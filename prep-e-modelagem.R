library(dplyr)
library(data.table)
library(caret)
library(rpart)
library(tidyr)

set.seed(100)

#DATA MUNGING
dados = fread('train_enxuto.csv')

#Os códigos dos clientes são por ordem de chegada, e não tem valor algum por si só, essa variável só possui alguma
#utilidade quando substituída pelos nomes reais dos clientes, que estão no arquivo cliente_tabla.csv
clientes = read.csv('cliente_tabla.csv',fileEncoding ="UTF-8")

#Removendo as duplicatas para ser possível realizar o join (limitação de ram)
dados = distinct(dados)

dados = dados %>% left_join(clientes)

str(dados)

#A coluna Cliente_ID não é mais necessária
dados$Cliente_ID = NULL

#Como haviam ID's diferentes para um mesmo cliente, é preciso remover as novas duplicatas
dados = distinct(dados)

#É preciso realizar o processo de substituição dos códigos das agências pelos endereços, pois uma mesma agência 
#tem mais de um ID, conforme town_state.csv
agencias = read.csv('town_state.csv',fileEncoding ="UTF-8")

#Os endereços estão divididos entre duas colunas, é preciso unir elas
agencia_mod = unite(agencias[,c(2,3)],'endereco',sep=', ')

#Recuperando a coluna de ID, que foi perdida
agencia_mod$Agencia_ID = agencias$Agencia_ID

#Fazendo o join
dados = dados %>% left_join(agencia_mod)

#A coluna Agencia_ID não é mais necessária
dados$Agencia_ID = NULL

#As variáveis preditoras são todas fatores
dados = dados %>% mutate_at(-4,as.factor)

#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#MODELAGEM
#Como a base de dados é muito grande, para ser possível fazer o treinamento eu criei um 
#modelo para cada rota através de um subset dos dados
sub_rota = function(rota){
  dados_rota = subset(dados, Ruta_SAK==rota,-2)
}

sub_end = function(end){
  dados_end = subset(dados, endereco==end,-6)
}

sub_prod = function(prod){
  dados_prod = subset(dados, Producto_ID==prod,-3)
}

sub_canal = function(canal){
  dados_canal = subset(dados, Canal_ID==canal,-1)
}

#LISTA DE FREQUENCIAS DE CADA ROTA
data.frame(table(dados$Ruta_SAK)) %>% arrange(Freq) %>% View()
dados_rota = sub_rota('2001')

data.frame(table(dados$endereco)) %>% arrange(Freq) %>% View()
dados_end = sub_end('2388 CHETUMAL, QUINTANA ROO')

data.frame(table(dados$Producto_ID)) %>% arrange(Freq) %>% View()
dados_prod = sub_prod('43209')

data.frame(table(dados$Canal_ID)) %>% arrange(Freq) %>% View()
dados_canal = sub_canal('5')

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
#MAE: 2.98414
#Resíduos: -3529.56
#R²: 0.48967
#RMSLE: 0.6061118

#Separando por rota
#MAE: 3.44798
#Resíduos: 1208.88
#R²: 0.484126
#RMSLE: 0.5918016

#Separando por endereco
#MAE: 4.10843
#Resíduos: -279.615
#R²: 0.673654

#Separando por canal
#MAE: 66.1614
#Resíduos: 6305.5
#R²: 0.555059
#RMSLE: 1.17

#CUBIST (Regressão linear)
modelo_cub = cubist(treino[,1:3],treino$Demanda_uni_equil,committees = 3)
df_cub = avalia(modelo_cub)
#MAE: 11.2576
#Resíduos: 89179.2
#R²: 0.297063

#SVM NÃO RODOU DEVIDO À FALTA DE MEMÓRIA