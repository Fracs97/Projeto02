library(dplyr)
library(data.table)
library(caret)
library(rpart)
library(mltools)
library(tidyr)
library(tidymodels)

set.seed(100)

#DATA MUNGING
dados = fread('train_enxuto.csv')

dados = sample_n(dados,0.8*nrow(dados))
#De acordo com a feature selection feita com o VarImp, a variável Canal_ID não tem importância no modelo
dados$Canal_ID = NULL

#Removendo duplicatas
dados = distinct(dados)

#Criando variáveis com as médias de demanda para cada coluna
medias_cliente = dados %>% group_by(NombreCliente) %>% summarise(media_cliente=mean(Demanda_uni_equil))
medias_agencia = dados %>% group_by(endereco) %>% summarise(media_agencia=median(Demanda_uni_equil))

dados = dados %>% left_join(medias_cliente)
dados = dados %>% left_join(medias_agencia)

#Ajustando os tipos das variáveis
dados = dados %>% mutate_at(-3,as.factor)

str(dados)
#PRÉ-PROCESSAMENTO
#Os fatores tem levels demais, é preciso substituir todos os menos frequentes por "Other" para aumentar e muito
#a velocidade de treinamento
receita = recipe(Demanda_uni_equil~.,data=dados) %>% step_other(Ruta_SAK,threshold = 0.0002) %>%
  step_other(Producto_ID,threshold=3e-4) %>% step_other(NombreCliente,threshold = 4e-5) %>%
  step_other(endereco,threshold = 2.9e-3)

dados = prep(receita) %>% juice()

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
tt = createDataPartition(dados$Demanda_uni_equil,p=0.7,list = F)
treino = dados[tt,]
teste = dados[-tt,]

sprintf('Treino: %d, teste: %d',dim(treino)[1],dim(teste)[1])

#Função que automatiza a avaliação do algoritmo
avalia=function(modelo){
  previsoes = predict(modelo,teste[,-5])
  compara = data.frame(real = teste$Demanda_uni_equil,previsto=previsoes)
  cat(sprintf('#MAE: %g',MAE(previsoes,teste$Demanda_uni_equil)))
  cat('\n')
  cat(sprintf('#Resíduos: %g',sum(compara$real-compara$previsto)))
  cat('\n')
  cat(sprintf('#R²: %g',cor(compara$real,compara$previsto)**2))
  return(compara)
}

#RPART
modelo_rpart = rpart(Demanda_uni_equil~.,data=treino,control = rpart.control(cp=0.00000316,
                                                                maxdepth = 15,min_n=30))
df_rpart = avalia(modelo_rpart)
rmsle(df_rpart$previsto,df_rpart$real)

#MAE: 5.5387
#Resíduos: 86175.2
#R²: 0.412905

#TUNING
#Definindo os hiperparâmetros que vão ser testados
tune_spec = decision_tree(cost_complexity=tune(),tree_depth=tune(),min_n=tune(),mode='regression', engine='rpart')
#Os nomes dos hiperparâmetros equivalentes a esses, no pacote rpart são:
#cost_complexity = cp, tree_depth = max_depth, min_n = minsplit

#Gerando combinações dos hiperparâmetros com valores arbitrários
tree_grid = grid_regular(cost_complexity(),tree_depth(),min_n(),levels=4)
#125 combinações diferentes foram geradas

#O treinamento dos hiperparâmetros não é feito na base de dados de treino inteira, e sim com pedaços gerados por
#validação cruzada
vf = vfold_cv(treino)

#Criando o workflow, que une o modelo de tuning criado com a fórmula de treinamento
tree_wf = workflow() %>% add_model(tune_spec) %>% add_formula(Demanda_uni_equil~.)

#Realizando o treinamento dos hiperparâmetros
tree_res = tree_wf %>% tune_grid(resamples=vf,grid=tree_grid)

#TESTAR FAZER O TUNING PARA TODOS OS DADOS, MAS SE NÃO FUNCIONAR, SEPARAR POR ENDEREÇO. FAZER UMA FUNÇÃO QUE
#BUSCA O ENDEREÇO DA AGENCIA NOS DADOS DE TREINO, E SE NÃO ENCONTRAR, SUBSTITUIR POR OTHER

#Extraindo as métricas e seus resultados
tree_res %>% collect_metrics() %>% select(cost_complexity,tree_depth,.metric,mean,min_n) %>% arrange(mean) %>% View()

tree_res %>% select_best('rsq')

