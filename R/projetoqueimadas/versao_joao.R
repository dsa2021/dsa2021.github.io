
rm(list=ls(all=TRUE))

library(mice)#pacote para imputação
library(dplyr)
library(caret)
library(gains)
library(pROC)
library(ROCR)
library(ROSE)
library(e1071)
library(psych)
library(dplyr)
library(randomForest)
library(tree)
library(car)
library(knitr)
library(MASS)
library(plyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(scales)
library(dplyr)


#Importando a base de dados
dados_queimadas_v1 <- read.csv("Focos_2021-06-26_2021-06-27.csv")

#Analisando os tipos dos dados
View(dados_queimadas_v1)

str(dados_queimadas_v1)

summary(dados_queimadas_v1)

dados_queimadas_v1[1:5,]

#Verificamos que existem dados faltantes e outliers

# An?lise dias sem chuva
hist(dados_queimadas_v1$diasemchuva,
     main = "Histograma dias sem chuva",
     xlab = "Quantidade de Dias",
     ylab = "Frequ?ncia",
     col = "#6600cc",
     labels = TRUE)


# Análise precipitação
hist(dados_queimadas_v1$precipitacao,
     main = "Histograma precipitacao",
     xlab = "Quantidade de Dias",
     ylab = "Frequência",
     col = "#6600cc",
     labels = TRUE)


# An?lise riscofogo
hist(dados_queimadas_v1$riscofogo,
     main = "Histograma Riscofogo",
     xlab = "Quantidade de Dias",
     ylab = "Frequência",
     col = "#6600cc",
     labels = TRUE)


# Diagrama de caixa para an?lise de distribui??o e de valores discrepantes (outliers)  
boxplot(dados_queimadas_v1$diasemchuva)

boxplot(dados_queimadas_v1$riscofogo)

# Removendo outliers negativos 

dados_queimadas_v2 =filter(dados_queimadas_v1, diasemchuva >=0 | is.na(diasemchuva),riscofogo >=0 | is.na(riscofogo))

summary(dados_queimadas_v2)
View(dados_queimadas_v1)
View(dados_queimadas_v2)


# An?lise distribui??o da vari?vel dias sem chuva com histograma antes e depois do filtro
hist(dados_queimadas_v1$diasemchuva)
hist(dados_queimadas_v2$diasemchuva)
     
par(mfrow=c(1,2))

hist(dados_queimadas_v1$diasemchuva)
hist(dados_queimadas_v2$diasemchuva)
par(mfrow=c(1,1))

split.screen(figs=c(1,2))

screen(1)
hist(dados_queimadas_v1$diasemchuva, 
     main = "Histograma da vari?vel Dias sem Chuva (sem filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'red')
screen(2)
hist(dados_queimadas_v2$diasemchuva, 
     main = "Histograma da vari?vel Dias sem Chuva (com filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'blue')
close.screen(all=TRUE)

# An?lise distribui??o da vari?vel risco de fogo com histograma antes e depois do filtro
hist(dados_queimadas_v1$riscofogo)
hist(dados_queimadas_v2$riscofogo)

par(mfrow=c(1,2))

hist(dados_queimadas_v1$riscofogo)
hist(dados_queimadas_v2$riscofogo)
par(mfrow=c(1,1))

split.screen(figs=c(1,2))

screen(1)
hist(dados_queimadas_v1$riscofogo, 
     main = "Histograma da vari?vel Risco de Fogo (sem filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'red')
screen(2)
hist(dados_queimadas_v2$riscofogo, 
     main = "Histograma da vari?vel Risco de Fogo (com filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'blue')
close.screen(all=TRUE)


# Diagrama de caixa da vari?vel dias sem chuva antes e depois da aplica??o do filtro  
 

boxplot(dados_queimadas_v1$diasemchuva)
boxplot(dados_queimadas_v2$diasemchuva)

par(mfrow=c(1,2))

boxplot(dados_queimadas_v1$diasemchuva)
boxplot(dados_queimadas_v2$diasemchuva)
par(mfrow=c(1,1))


split.screen(figs=c(1,2))

screen(1)
boxplot(dados_queimadas_v1$diasemchuva, col = 'red')
screen(2)
boxplot(dados_queimadas_v2$diasemchuva, col = 'blue')
close.screen(all=TRUE)

# Diagrama de caixa da vari?vel risco de fogo antes e depois da aplica??o do filtro  

boxplot(dados_queimadas_v1$riscofogo)
boxplot(dados_queimadas_v2$riscofogo)

boxplot(dados_queimadas_v1$riscofogo)
boxplot(dados_queimadas_v2$riscofogo)

par(mfrow=c(1,2))

boxplot(dados_queimadas_v1$riscofogo)
boxplot(dados_queimadas_v2$riscofogo)
par(mfrow=c(1,1))

split.screen(figs=c(1,2))

screen(1)
boxplot(dados_queimadas_v1$riscofogo, col = 'red')
screen(2)
boxplot(dados_queimadas_v2$riscofogo, col = 'blue')
close.screen(all=TRUE)


#Contando os valores NA(185 linhas e 2220 informações)
sapply(dados_queimadas_v2, function(x)sum(is.na(x)))
sum(is.na(dados_queimadas_v2))


##### Aplicando Imputa??o em Valores Missing Usando M?todo PMM (Predictive Mean Matching)


# 1? Encontrar as vari?veis com dados do tipo caracter e pegar o nome das colunas

chr_col <- as.integer(0)
chrnames <- names(Filter(is.character, dados_queimadas_v2))
chrnames
k = 1

#2? Encontrando o ?ndice dessas colunas
for(i in chrnames){
        while (k <= 6){ #nesse dataset temos apenas 6
                grep(i, colnames(dados_queimadas_v2))
                chr_col[k] <- grep(i, colnames(dados_queimadas_v2))
                k = k + 1
                break 
        }
}

# Colunas que s?o do tipo caracter
chr_col

# Imputa??o

# Definindo a regra de imputa??o

regra_imputacao <- mice((dados_queimadas_v2[,-c(chr_col)]), 
                        m = 1, 
                        maxit = 50, 
                        meth = 'pmm', 
                        )

# Aplicando a regra de imputação
total_data <- complete(regra_imputacao, 1)
View(total_data)
sum(is.na(total_data))

# Juntar novamente as vari?veis categ?ricas ao dataset
dados_queimadas_v2_final <- cbind(dados_queimadas_v2[,c(chr_col)],total_data )#incluindo as colunas com fator o c esta sem o sinal de negativo
View(dados_queimadas_v2_final)
View(dados_queimadas_v2)
sum(is.na(dados_queimadas_v2_final))

summary(dados_queimadas_v2_final)


# An?lise das vari?veis dias sem chuva em tr?s momentos: 
# (a) hitograma data frame original; 
# (b) hitograma data frame com filtro; 
# (c) hitograma data frame com filtro e com imputa??o

hist(dados_queimadas_v1$diasemchuva)
hist(dados_queimadas_v2$diasemchuva)
hist(dados_queimadas_v2_final$diasemchuva)
par(mfrow=c(1,2))

hist(dados_queimadas_v1$diasemchuva)
hist(dados_queimadas_v2$diasemchuva)
hist(dados_queimadas_v2_final$diasemchuva)
par(mfrow=c(1,1))

split.screen(figs=c(2,2))

screen(1)
hist(dados_queimadas_v1$diasemchuva, 
     main = "Histograma da vari?vel Dias sem Chuva 
(sem filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'red')
screen(2)
hist(dados_queimadas_v2$diasemchuva, 
     main = "Histograma da vari?vel Dias sem Chuva 
(com filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'blue')
screen(3)
hist(dados_queimadas_v2_final$diasemchuva, 
     main = "Histograma da vari?vel Dias sem Chuva 
(com filtro e imputa??o)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'green')
close.screen(all=TRUE)


# An?lise das vari?veis risco de fogo em tr?s momentos: 
# (a) hitograma data frame original; 
# (b) hitograma data frame com filtro; 
# (c) hitograma data frame com filtro e com imputa??o

hist(dados_queimadas_v1$riscofogo)
hist(dados_queimadas_v2$riscofogo)
hist(dados_queimadas_v2_final$riscofogo)
          
par(mfrow=c(1,2))
          
hist(dados_queimadas_v1$riscofogo)
hist(dados_queimadas_v2$riscofogo)
hist(dados_queimadas_v2_final$riscofogo)
par(mfrow=c(1,1))
          
split.screen(figs=c(2,2))
          
screen(1)
hist(dados_queimadas_v1$riscofogo,
    main = "Histograma da vari?vel Risco de Fogo (sem filtro)", 
    xlab = "Quantidade de dias", 
    ylab = "Frequ?ncia", 
    col = 'red')
screen(2)
hist(dados_queimadas_v2$riscofogo, 
     main = "Histograma da vari?vel Risco de Fogo (com filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'blue')
screen(3)
hist(dados_queimadas_v2_final$riscofogo, 
     main = "Histograma da vari?vel Risco de Fogo (com filtro e imputa??o)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'green')
close.screen(all=TRUE)
          

# An?lise das vari?veis precipita??o em tr?s momentos: 
# (a) hitograma data frame original; 
# (b) hitograma data frame com filtro; 
# (c) hitograma data frame com filtro e com imputa??o

hist(dados_queimadas_v1$precipitacao)
hist(dados_queimadas_v2$precipitacao)
hist(dados_queimadas_v2_final$precipitacao)

par(mfrow=c(1,2))

hist(dados_queimadas_v1$precipitacao)
hist(dados_queimadas_v2$precipitacao)
hist(dados_queimadas_v2_final$precipitacao)
par(mfrow=c(1,1))

split.screen(figs=c(2,2))

screen(1)
hist(dados_queimadas_v1$precipitacao,
     main = "Histograma da vari?vel Precipita??o 
  (sem filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'red')
screen(2)
hist(dados_queimadas_v2$precipitacao, 
     main = "Histograma da vari?vel Precipita??o 
  (com filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'blue')
screen(3)
hist(dados_queimadas_v2_final$precipitacao, 
     main = "Histograma da vari?vel Precipita??o 
  (com filtro e imputa??o)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'green')
close.screen(all=TRUE)


# An?lise das vari?veis frp em tr?s momentos: 
# (a) hitograma data frame original; 
# (b) hitograma data frame com filtro; 
# (c) hitograma data frame com filtro e com imputa??o

hist(dados_queimadas_v1$frp)
hist(dados_queimadas_v2$frp)
hist(dados_queimadas_v2_final$frp)

par(mfrow=c(1,2))

hist(dados_queimadas_v1$frp)
hist(dados_queimadas_v2$frp)
hist(dados_queimadas_v2_final$frp)
par(mfrow=c(1,1))

split.screen(figs=c(2,2))

screen(1)
hist(dados_queimadas_v1$frp,
     main = "Histograma da vari?vel Energia Radiativa 
do Fogo (sem filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'red')
screen(2)
hist(dados_queimadas_v2$frp,
     main = "Histograma da vari?vel Energia Radiativa 
do Fogo (com filtro)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'blue')
screen(3)
hist(dados_queimadas_v2_final$frp,
     main = "Histograma da vari?vel Energia Radiativa 
do Fogo (com filtro e imputa??o)", 
     xlab = "Quantidade de dias", 
     ylab = "Frequ?ncia", 
     col = 'green')
close.screen(all=TRUE)



# BoxPlot 
boxplot(dados_queimadas_v2_final$diasemchuva, main = "Boxplot da vari?vel Dias sem Chuva")
boxplot(dados_queimadas_v2_final$precipitacao, main = "Boxplot da vari?vel Precipita??o")
boxplot(dados_queimadas_v2_final$riscofogo, main = "Boxplot da vari?vel Risco de Fogo")
boxplot(dados_queimadas_v2_final$frp, main = "Boxplot da vari?vel Frequ?ncia Radiativa do Fogo")


#calculando a m?dia de dias sem chuva por munic?pio  
diaschuva_municipio_antes<-aggregate(data = dados_queimadas_v2, diasemchuva ~ municipio, mean)
diaschuva_municipio_antes<-aggregate(data = dados_queimadas_v2_final, diasemchuva ~ municipio, mean)

#calculando a média de dias sem chuva por estado
diaschuva_estado_antes<- aggregate(data = dados_queimadas_v2, diasemchuva ~ estado, mean)
diaschuva_estado_depois<- aggregate(data = dados_queimadas_v2_final, diasemchuva ~ estado, mean)

#calculando v?rios juntos 
aggregate(data = dados_queimadas_v2_final, cbind(diasemchuva,precipitacao,riscofogo,frp) ~ estado, mean)
aggregate(data = dados_queimadas_v2_final, cbind(diasemchuva,precipitacao,riscofogo,frp) ~ municipio, mean)

#Variancia 

var(dados_queimadas_v2_final$riscofogo)
var(dados_queimadas_v2_final$diasemchuva)
var(dados_queimadas_v2_final$precipitacao)
var(dados_queimadas_v2_final$frp)

#Desvio Padr?o 
sd(dados_queimadas_v2_final$riscofogo)
sd(dados_queimadas_v2_final$diasemchuva)
sd(dados_queimadas_v2_final$precipitacao)
sd(dados_queimadas_v2_final$frp)

#Coeficiente de Assimetria
skewness(dados_queimadas_v2_final$diasemchuva)
skewness(dados_queimadas_v2_final$precipitacao)
skewness(dados_queimadas_v2_final$riscofogo)
skewness(dados_queimadas_v2_final$frp)

#Curtose grau de achatamento em relação a normal Padrão
kurtosis(dados_queimadas_v2_final$diasemchuva)
kurtosis(dados_queimadas_v2_final$riscofogo)
kurtosis(dados_queimadas_v2_final$precipitacao )
kurtosis(dados_queimadas_v2_final$frp)

#diasemchuva X riscofogo(correlação fraca positiva)
x = dados_queimadas_v2_final$diasemchuva
y = dados_queimadas_v2_final$riscofogo
cor(x,y)

#precipitacao X riscofogo (correlação fraca negativa)
x = dados_queimadas_v2_final$precipitacao
y = dados_queimadas_v2_final$riscofogo
cor(x,y)

#frp X riscofogo (correlação fraca negativa)
x = dados_queimadas_v2_final$frp
y = dados_queimadas_v2_final$riscofogo
cor(x,y)


#Correlacao

cor(dados_queimadas_v2_final[c( "diasemchuva", "precipitacao", "riscofogo", "latitude","longitude","frp" )])
pairs.panels(dados_queimadas_v2_final[c( "diasemchuva", "precipitacao", "riscofogo", "latitude","longitude","frp" )])


# Checando se a variável alvo está balanceada (*******colocar o indicador de queimada e ou tipo vegetação, região, etc)

#Muita concentra??o em 1
prop.table(table(dados_queimadas_v2_final$riscofogo)) * 100


prop.table(table(dados_queimadas_v2_final$diasemchuva)) * 100

#Muita concentra??o em 0
prop.table(table(dados_queimadas_v2_final$precipitacao)) * 100


# Antes e depois da imputa??o

as.data.frame(table(dados_queimadas_v2$riscofogo))

as.data.frame(table(dados_queimadas_v2_final$riscofogo))



#Transformando dados que est?o como character mas podem ter comportamento de fatores Bioma

dados_queimadas_v3_final <-data.frame(dados_queimadas_v2_final)
dados_queimadas_v3_final$bioma <- as.factor(dados_queimadas_v3_final$bioma)
dados_queimadas_v3_final$diasemchuva<- as.numeric(dados_queimadas_v3_final$diasemchuva)

str(dados_queimadas_v3_final)
View(dados_queimadas_v3_final)

#Dividindo os dados em treino 60% e 30% teste e ( balancear dados de treino ??)

indice_divisao_dos_dados <- sample(x = nrow(dados_queimadas_v3_final),
                              size = 0.6 * nrow(dados_queimadas_v3_final),
                              replace = FALSE)#amostra sem reposição


# Separando os dados
dados_treino <- dados_queimadas_v3_final[indice_divisao_dos_dados ,]
dados_teste <- dados_queimadas_v3_final[-indice_divisao_dos_dados ,]


# Treinamento do modelo de regress?o linear  N?1

model_v1 <- lm(riscofogo ~ diasemchuva +precipitacao+ latitude + longitude + frp, data = dados_treino[,-c(1:6)] )
previsao_treino_v1 <- predict(model_v1)

#Testando o modelo N?1
teste_v1 <- dados_teste[,-c(1:6,9)] #retirando colunas de character e target
View(dados_teste)
View(teste_v1)
previsao_teste_v1 <- predict(model_v1,teste_v1)
View(previsao_teste)


summary(model_v1)


#Detectando a colinearidade: quando duas ou mais variaveis preditivas são altamente correlacionadas
#aumenta o erro padrão obtendo estimativas instáveis
#avaliar valores maiores que 5
kable(vif(model_v1 ),align='c')

#Fazendo seleção de atributos com o método Akaike(AIC)
?stepAIC
step<-stepAIC(model_v1, direction='both', trace=FALSE)
summary(step)

#Incluindo uma coluna com os valores previstos no dataset
dados_teste_com_previsoes <- cbind(dados_teste, previsao_teste_1)

#Colocando a variavel Target ao Lado dos valores Previstos 

dados_teste_com_previsoes<- dados_teste_com_previsoes %>%
        select(datahora, satelite, pais, estado, municipio,bioma,diasemchuva,precipitacao,latitude,longitude,frp,riscofogo,previsao_teste_1)
View(dados_teste_com_previsoes)



# Treinamento do modelo de regressão linear  N?2 
# Transformando o bioma para fator e incluindo no modelo

model_v2 <- lm(riscofogo ~bioma  + diasemchuva + precipitacao + latitude + longitude + frp, data = dados_treino)
previsao_treino_v2 <- predict(model_v2)

#Testando o modelo Nº2
teste_v2 <- dados_teste[,-c(1:5,9)] #retirando coluna de character e target
View(dados_teste)
View(teste_v2)
previsao_teste_v2 <- predict(model_v2,teste_v2)
View(previsao_teste)


summary(model_v2)
#Resultado: Adjusted R-squared:  0.5785 

# Treinamento do modelo de regress?o linear  N?3 retirando o FRP


model_v3 <- lm(riscofogo ~diasemchuva + precipitacao + latitude + longitude , data = dados_treino)
previsao_treino_v3 <- predict(model_v3)

#Testando o modelo Nº3
teste_v3 <- dados_teste[,-c(1:6,9,12)]
View(dados_teste)
View(teste_v3)
previsao_teste_v3 <- predict(model_v3,teste_v3)
View(previsao_teste_v3)

summary(model_v3)
#Resultado: Adjusted R-squared:  0.5255 


# Treinamento do modelo de regressão linear  N?4 
# Elevando latitude e longitude ao quadrado

dados_treino_v2 <- dados_treino
dados_treino_v2$latitude <- dados_treino$latitude^2
dados_treino_v2$longitude <- dados_treino$longitude^2
dados_teste_v2 <- dados_teste
dados_teste_v2$latitude <- dados_teste$latitude^2
dados_teste_v2$longitude <- dados_teste$longitude^2

model_v4 <- lm(riscofogo ~bioma  + diasemchuva + precipitacao + latitude + longitude + frp, data = dados_treino_v2)
previsao_treino_v4 <- predict(model_v4)

#Testando o modelo N?4
teste_v4 <- dados_teste_v2[,-c(1:5,9)] #retirando coluna de character e target
View(dados_teste_v2)
View(teste_v4)
previsao_teste_v4 <- predict(model_v4,teste_v4)
View(previsao_teste)


summary(model_v4)
#Resultado: Adjusted R-squared:  0.5507 


#Testando o modelo Nº5 (Random Forest)
teste_v5 <- dados_teste[,-c(1:5,9)] #retirando coluna de character e target
treino_v5 <- dados_treino[,-c(1:5,9)] #retirando coluna de character e target

View(treino_v5)
View(dados_treino[9])
View(teste_v5)
View(dados_teste[9])

length(treino_v5)

rf <- randomForest(x=treino_v5,
                   y=dados_treino[, 9],
                   xtest=teste_v5,
                   ytest=dados_teste[,9],
                   ntree=200)
rf                   
varImpPlot(rf)
plot(rf)
text(rf, pretty=0)



#Testando o modelo Nº6(Decision Tree)
teste_v6 <- dados_teste[,-c(1:5,9)] #retirando coluna de character e target
treino_v6 <- dados_treino[,-c(1:5)] #retirando coluna de character 

?tree
tree_1<-tree(riscofogo ~ bioma  + diasemchuva + precipitacao + latitude + longitude + frp, data = treino_v6)

summary(tree_1)
plot(tree_1)
text(tree_1)

#valida??o cruzada
cv.tree1<- cv.tree(tree_1)
plot(cv.tree1$size, cv.tree1$dev, type ="b", col="blue")


#teste
riscofogo_teste <- predict(tree_1,teste_v6  )
riscofogo_original <-dados_teste[,9]

teste1 <- data.frame(obs=riscofogo_original, pred=riscofogo_teste)
str(teste1)
?defaultSummary
defaultSummary(teste1)
# Checando se a variável alvo está balanceada (*******colocar o indicador de queimada e ou tipo vegetação, região, etc)

#Muita concentração em 1
prop.table(table(dados_treino$riscofogo)) * 100

dados_treino$precipitacao = as.factor (dados_treino$precipitacao)
dados_treino$riscofogo = as.factor (dados_treino$riscofogo)
dados_treino$latitude  = as.factor (dados_treino$latitude )
dados_treino$longitude  = as.factor (dados_treino$longitude)
dados_treino$frp  = as.factor (dados_treino$frp )

View(dados_treino)

#aplicando balanceamento com SMOTE(não deu certo)
##dados_treino_balanceados <- SMOTE(riscofogo ~ ., dados_treino, perc.over = 600, perc.under = 100)

##str(dados_treino)



###Bases para Dashboard shiny

##Agrupando os dados em tabelas

##Risco Fogo m?dio X Estado+Município
# Agrupa os dados pela m?dia de riscofogo 
RiscoFogoMedioEstadoMunicipio <- aggregate(riscofogo ~ estado+municipio, data = dados_queimadas_v3_final, mean)
colnames(RiscoFogoMedioEstadoMunicipio)[3]  <- "Riscofogo_medio"

##Risco Fogo médio X Estado
# Agrupa os dados pela média de riscofogo 
RiscoFogoMedioEstado <- aggregate(riscofogo ~ estado, data = dados_queimadas_v3_final, mean)
colnames(RiscoFogoMedioEstado)[2]  <- "Riscofogo_medio"

##Risco Fogo m?dio X Bioma
# Agrupa os dados pela média de riscofogo 
RiscoFogoMedioBioma <- aggregate(riscofogo ~ bioma, data = dados_queimadas_v3_final, mean)
colnames(RiscoFogoMedioBioma)[2]  <- "Riscofogo_medio"

##Dias sem Chuva médio X Bioma
# Agrupa os dados pela média de diasemchuva
DiasSemChuvaMedioBioma <- aggregate(diasemchuva ~ bioma, data = dados_queimadas_v3_final, mean)
colnames(DiasSemChuvaMedioBioma)[2]  <- "Diasemchuva_medio"

##Dias sem Chuva médio X Estado
# Agrupa os dados pela média de diasemchuva
DiasSemChuvaMedioEstado <- aggregate(diasemchuva ~ estado, data = dados_queimadas_v3_final, mean)
colnames(DiasSemChuvaMedioEstado)[2]  <- "Diasemchuva_medio"

##Dias sem Chuva médio X Munic?pio
# Agrupa os dados pela média de diasemchuva
DiasSemChuvaMedioMunicipio <- aggregate(diasemchuva ~ municipio, data = dados_queimadas_v3_final, mean)
colnames(DiasSemChuvaMedioMunicipio)[2]  <- "Diasemchuva_medio"


#Tabela Estado
TabelaEstado <- left_join(RiscoFogoMedioEstado,DiasSemChuvaMedioEstado, by = c("estado"="estado"))

#Tabela Munic?pio
TabelaMunicipio <- left_join(RiscoFogoMedioEstadoMunicipio,DiasSemChuvaMedioMunicipio, by = c("municipio"="municipio"))

#Tabela Bioma
TabelaBioma <- left_join(RiscoFogoMedioBioma,DiasSemChuvaMedioBioma, by = c("bioma"="bioma"))

