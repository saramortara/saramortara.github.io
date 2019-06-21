###############################################################
###### Código para as analises da aula 2.2 regressao linear ###
###############################################################

# elaborado por Sara Mortara para disciplina de modelagem estatistica
# dezembro de 2018

# define o diretorio de trabalho
### ATENCAO! mude para o SEU diretorio de trabalho
setwd("~/Dropbox/posdoc/disciplina_modelagem/atividadesR/teoricas")

#####################################
# PARTE 1: uma regressao qualquer ###
#####################################

# vamos ler os dados x1 e y1 usados na aula
xy <- read.csv("../dados/exemplo_xy.csv")

# inspecionando os dados
head(xy)

# sumario dos dados
summary(xy)

# para facilitar, vamos usar o comando attach
attach(xy) # cria um objeto chamado x1 e outro x2 


# PASSO 1: variavel resposta é normal?

# visualizando o dado num histograma
hist(y1, probability = TRUE, ylab="densidade")
# adicionando a curva da distribuicao normal teorica
curve(dnorm(x, mean(y1), sd(y1)), add=TRUE, lwd=3)

# PASSO 2: qual a relacao entre a preditora e a resposta?
plot(y1 ~ x1, data=xy, las=1, bty="l")

#########################################################################
# INTERROMPEMOS O CODIGO PARA A EXPLICACAO DA ESTIMATIVA DOS PARAMETROS #
#########################################################################

## o ponto de fulcro
plot(y1 ~ x1, data=xy, xlab="preditora", ylab="resposta", pch=19, bty="l", las=1)
# plotando o ponto de fulcro
# ponto menor e preenchido
points(x=mean(x1), y=mean(y1), pch=19, cex=.6)
# ponto maior
points(x=mean(x1), y=mean(y1), pch=1, cex=1.2)
# texto
text("fulcro", x=mean(x1)+.37, y=mean(y1), cex=.4)
## PRIMEIRA tentativa
abline(a=16.8 , b=-1)

## segunda tentativa, vamos plotar o grafico de novo
## o ponto de fulcro
plot(y1 ~ x1, data=xy, xlab="preditora", ylab="resposta", pch=19, bty="l", las=1)
# plotando o ponto de fulcro
# ponto menor e preenchido
points(x=mean(x1), y=mean(y1), pch=19, cex=.6)
# ponto maior
points(x=mean(x1), y=mean(y1), pch=1, cex=1.2)
# texto
text("fulcro", x=mean(x1)+.37, y=mean(y1), cex=.4)
## SEGUNDA tentativa
abline(a=-10 , b=8.07)

## terceira tentativa, vamos plotar o grafico de novo
## o ponto de fulcro
plot(y1 ~ x1, data=xy, xlab="preditora", ylab="resposta", pch=19, bty="l", las=1)
# plotando o ponto de fulcro
# ponto menor e preenchido
points(x=mean(x1), y=mean(y1), pch=19, cex=.6)
# ponto maior
points(x=mean(x1), y=mean(y1), pch=1, cex=1.2)
# texto
text("fulcro", x=mean(x1)+.37, y=mean(y1), cex=.4)
## TERCEIRA tentativa, agora com os coeficientes da regressao
abline(lm(y1 ~ x1))

## agora vamos visualizar os residuos no grafico
# primeiro precisamos calcular os valores previstos com a funcao predict 
prev <- predict(lm(y1 ~ x1))
# agora adicionamos os segmentos do valor esperado ate a reta prevista 
segments(x0=x1, x1=x1, y0=y1, y1=prev, lty=2)

# FIM da interrupcao ###################################################
#########################################################################

# PASSO 3: construindo o modelo linear

# salvamos o modelo linear num objeto chamado 'mod'
mod <- lm(y1 ~ x1)

# inspecionando a saida do modelo
## o que voce entende da saida?
summary(mod)

# extraindo apenas os valores dos parametros
coef(mod)

# extraindo o intervalo de confianca da estimativa
confint(mod)

# PASSO 4: visualizando o ajuste do modelo aos dados
plot(y1 ~ x1, data=xy, las=1, bty="l", cex=.7, 
     xlab="preditora", ylab="resposta", pch=19)
abline(mod, lwd=2, col='red')

# PASSO 5: entendendo a partição da variancia no modelo linear 
# SQtot = SQmod + SQerro

## desvio quadratico total
# SQtot = somatorio(y-ymed)^2

sq.tot <- sum((y1 - mean(y1))^2)
sq.tot

## desvio quadratico do residuo
# SQerro = somatorio(y-ymed)^2
sq.er <- sum(residuals(mod)^2)
sq.er

## desvio quadratico do modelo
# se SQtot = SQmod + SQerro
# SQmod = SQtot-SQerro
sq.mod <- sq.tot - sq.er 
sq.mod

# conferindo os calculos com a tabela de anova
anova(mod)

## calculando o R2
# SQmod/SQtot

meuR2 <- sq.mod/sq.tot

meuR2

# conferindo com o sumario do modelo
summary(mod)

# PASSO 6: diagnostico do mo modelo
## vamos olhar os residuos

par(mfrow=c(1,2)) # cria janela grafica com 1 linha e 2 colunas
hist(residuals(mod), main="", probability=TRUE, xlab="resíduo", ylab="densidade")
boxplot(residuals(mod), ylab="resíduo") # volta a configuracao original
par(mfrow=c(1,1)) # cria janela grafica com 1 linha e 2 colunas

## o diagnostico do modelo
plot(mod)

## para finalizar, vamos olhar novamente o ajuste do modelo aos dados
plot(y1 ~ x1, data=xy, las=1, bty="l", 
     xlab="preditora", ylab="resposta", pch=19)
abline(lm(y1 ~ x1), lwd=2)


