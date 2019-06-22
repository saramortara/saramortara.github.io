############################################################
###### Código para as analises da aula 2.1 distribuicoes ###
############################################################

# elaborado por Sara Mortara para disciplina de modelagem estatistica
# dezembro de 2018

# define o diretorio de trabalho
### ATENCAO! mude para o SEU diretorio de trabalho
setwd("~/Dropbox/posdoc/disciplina_modelagem/atividadesR/teoricas")

##############################
# PARTE 1: lendo os dados ####
##############################

# lendo os dados da idade da população que usa fraldas
fraldas <- read.csv("../dados/idade_fraldas_NA.csv")

# checando os dados
# checando as primeiras 6 linhas da planilha
head(fraldas)
# checando as ultimas 6 linhas da planilha
tail(fraldas)

# estrutura dos dados
str(fraldas)

# sumario dos dados
summary(fraldas)


#################################
# PARTE 2: encontrando os NA ####
#################################

# teste logico para saber onde tem NA na coluna idade do objeto fraldas
is.na(fraldas$idade)

# vendo quais elementos tem NA na coluna idade do objeto fraldas
which(is.na(fraldas$idade))

# selecionandando as linhas que tem NA no objeto fraldas
fraldas[c(2,17),] # c(2,17) seleciona as linhas 2 e 17 do objeto fraldas

# na verdade, o NA seria 0
## vamos substituir o NA por 0
## ve quem tem NA
fraldas$idade[is.na(fraldas$idade)]
## atribui 0 aos elementos que eram NA
fraldas$idade[is.na(fraldas$idade)] <- 0
## checando a substituicao
# vendo os valores
fraldas$idade[is.na(fraldas$idade)]
# fazendo o teste logico com a funcao is.na
is.na(fraldas$idade)
# como os valores de TRUE e FALSE são 0 e 1 no R, podemos somar para checar se tem algum verdadeiro 
sum(is.na(fraldas$idade))

#################################
# PARTE 3: checando os zeros  ###
#################################

# teste logico para saber se tem zero nos dados de idade
fraldas$idade==0

# fazendo a soma para quantificar quantos zeros
sum(fraldas$idade==0)

# muitos bebes usam fralda, zero é ok neste caso talkey?

###################################
# PARTE 4: entendendo os dados ####
###################################

# sumario dos valores de idade
summary(fraldas$idade)

# boxplot dos valores de idade
boxplot(fraldas$idade)

# histograma dos valores de idade
hist(fraldas$idade)

## na verdade, temos dois grupos: bebes e vovxs
# vamos separar os dados em dois grupos

# criando planilha com a idade de bebes
bb <- fraldas[fraldas$idade<10,] 
# criando planilha com a idade de vovx
vv <- fraldas[fraldas$idade>10,]

#######################################################
# PARTE 5: visualizando os novos dados em graficos ####
########################################################

### 5.1. boxplot
# comando para criar duas janelas graficas, 1 linha e 2 colunas
par(mfrow=c(1,2))
# boxplot de bebes
boxplot(bb$idade)
# boxplor de vovxs
boxplot(vv$idade)
par(mfrow=c(1,1)) # retorna a configuracao inicial

### 5.2. histogramas
par(mfrow=c(1,2)) # cria 2 janelas graficas, 1 linha e 2 colunas
# histograma de bebes 
hist(bb$idade)
# histograma de vovxs
hist(vv$idade)
par(mfrow=c(1,1)) # retorna a configuracao inicial

### 5.2.2. classes de histogramas
par(mfrow=c(1,2)) # cria 2 janelas graficas, 1 linha e 2 colunas
# histograma de frequencia de bebes
hist(bb$idade)
# histograma de densidade de bebes
hist(bb$idade, probability = TRUE)
par(mfrow=c(1,1)) # retorna a configuracao inicial

### 5.2.3. numero de barras nos histogramas
par(mfrow=c(1,3)) # cria 2 janelas graficas, 1 linha e 3 colunas
# hitograma com tres classes
hist(bb$idade, breaks=seq(0, max(bb$idade), length=3))
# histograma com 5 classes
hist(bb$idade,  breaks=seq(0, max(bb$idade), length=5))
# histograma padrao 9 classes (neste caso!)
hist(bb$idade)
par(mfrow=c(1,1)) # retorna a configuracao inicial

### 5.3 inspecionando a distribuicao dos dados
# guardando um objeto com a idade maxima
bb.max <- max(bb$idade)
# guardando um objeto com a media de idade (= parametro lambda da Posson)
bb.med <- mean(bb$idade)

# fazendo o grafico
## histograma de densidade probalilistica
hist(bb$idade, probability = TRUE)
## pontos com a distribuicao Poisson estimada a partir do dado
points(dpois(0:bb.max, bb.med), col="blue") # comando que cria os pontos
lines(dpois(0:bb.max, bb.med), col="blue") # comando que cria a linha


