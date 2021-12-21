library(dplyr)
library(data.table)
library(caret)
library(rpart)
library(tidymodels)
library(doParallel)
library(tidyr)

set.seed(100)

#DATA MUNGING
dados = fread('train_enxuto.csv')

#Ajustando os tipos das variáveis
dados = dados %>% mutate_at(-4,as.factor)

#Removendo duplicatas
dados = distinct(dados)

#Não há nenhum nulo
sapply(dados,function(x)sum(is.na(x)))

#MODELAGEM
#Como a base de dados é muito grande, para ser possível fazer o treinamento eu criei um 
#modelo para cada endereço através de um subset dos dados
sub_end = function(end){
  dados_end = subset(dados, endereco==end,-6)
}

#LISTA DE FREQUENCIAS DE CADA ENDEREÇO DE AGENCIA
data.frame(table(dados$endereco)) %>% arrange(Freq) %>% View()
#Escolhendo o endereço da agência
dados_end = sub_end('2001 AG. ATIZAPAN, ESTADO DE MÉXICO')

str(dados_end)
#PRÉ-PROCESSAMENTO
#Os fatores tem levels demais, é preciso substituir todos os menos frequentes por "Other" para aumentar e muito
#a velocidade de treinamento
receita = recipe(Demanda_uni_equil~.,data=dados_end)  %>% step_other(Ruta_SAK,threshold = 2e-6) %>% 
  step_other(NombreCliente,threshold = 1e-5)

#Efetuando a mudança dos levels dos fatores
dados_end = prep(receita) %>% juice()

dados_end = distinct(dados_end)

#Separando em treino e teste
tt = initial_split(dados_end,0.7,strata = Demanda_uni_equil)
treino = training(tt)
teste = testing(tt)

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

#ANTES DO TUNING
#RPART
modelo_rpart = rpart(Demanda_uni_equil~.,data=treino)
df_rpart = avalia(modelo_rpart)

#TUNING
#Definindo os hiperparâmetros que vão ser testados
tune_spec = decision_tree(cost_complexity=tune(),tree_depth=tune(),min_n=tune(),mode='regression', engine='rpart')
#Os nomes dos hiperparâmetros equivalentes a esses, no pacote rpart são:
#cost_complexity = cp, tree_depth = max_depth, min_n = minsplit

#Gerando combinações dos hiperparâmetros com valores arbitrários
tree_grid = grid_regular(cost_complexity(),tree_depth(),min_n(),levels=5)
#125 combinações diferentes foram geradas

#O treinamento dos hiperparâmetros não é feito na base de dados de treino inteira, e sim com pedaços gerados por
#validação cruzada
vf = vfold_cv(treino)

#Criando o workflow, que une o modelo de tuning criado com a fórmula de treinamento
tree_wf = workflow() %>% add_model(tune_spec) %>% add_formula(Demanda_uni_equil~.)

#Realizando o treinamento dos hiperparâmetros
tree_res = tree_wf %>% tune_grid(resamples=vf,grid=tree_grid)

#Extraindo as métricas e seus resultados
tree_res %>% collect_metrics() %>% select(cost_complexity,tree_depth,.metric,mean,min_n) %>% arrange(mean) %>% View()

#Extraindo o melhor modelo
best_rsq = tree_res %>% select_best('rsq')

#Criando o modelo com os parâmetros que geram o maior coeficiente de determinação R²
final_rf = finalize_model(tune_spec, best_rsq)

#Aplicando os melhores parâmetros para o modelo rpart
final_wf = workflow() %>% add_formula(Demanda_uni_equil~.) %>% add_model(final_rf) %>% last_fit(tt,metrics=metric_set(rsq,mae))

#AVALIAÇÃO
#R² (rsq) E MAE DO MODELO PARA O ENDEREÇO ESCOLHIDO
final_wf %>% collect_metrics() %>% select(.metric,.estimate)

#MAE: 3
#R²: 0.525

#Coletando previsões
final_wf %>% collect_predictions() %>% select(c('.pred','Demanda_uni_equil')) %>% View()
