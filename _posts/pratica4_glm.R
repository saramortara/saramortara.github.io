## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,2))
curve(3-x, xaxt='n', yaxt='n', col='navy', lwd=2, 
      las=1, bty='l', ylab="N de doentes", xlab="Tempo")
curve(3.2-x, ylim=c(-2,0), add=TRUE, col='tomato', lwd=2)
#legend("topright", c("com vacina", "sem vacina"), lwd=2, 
#       col=c('navy', 'tomato'))
mtext("1.", 3, adj=0)
curve(3-x, xaxt='n', yaxt='n', col='navy', lwd=2, 
      las=1, bty='l', ylab="N de doentes", xlab="Tempo")
curve(2.98-x, ylim=c(-2,0), add=TRUE, col='tomato', lwd=2)
legend("topright", c("com vacina", "sem vacina"), lwd=2, 
       col=c('navy', 'tomato'))
mtext("2.", 3, adj=0)
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
## defina o diretorio de trabalho O SEU! e não o meu
setwd("~/Dropbox/posdoc/disciplina_modelagem/atividadesR/praticas")
## lendo os dados
### ajuste para o seu caminho
doentes <- read.csv("../dados/doentes.csv")

## ------------------------------------------------------------------------
hist(doentes$N, probability = TRUE)

## ------------------------------------------------------------------------
summary(doentes)

## ------------------------------------------------------------------------
y.lim <- range(doentes$N) # padronizando o limite do eixo y
plot(N ~ dias, data=doentes[doentes$local=="A",], 
     col='tomato', ylim=y.lim, 
     ylab="N de doentes", las=1, bty='l')
points(N ~ dias, data=doentes[doentes$local=="B",], col='navy')
legend("topleft", c("com vacina", "sem vacina"), 
       col=c("navy", "tomato"), pch=1)

## ------------------------------------------------------------------------
#hipótese 1 vacina funciona
p01 <- glm(N ~ dias + local, data=doentes, family='poisson')
# hipótese 2 vacina não funciona
p02 <- glm(N ~ dias, data=doentes, family='poisson')
# hipótese 3
p03 <- glm(N ~ 1, data=doentes, family='poisson')

## ---- results='hide'-----------------------------------------------------
summary(p01)
summary(p02)
summary(p03)

## ------------------------------------------------------------------------
library(bbmle)
AICtab(p01, p02, p03)

## ------------------------------------------------------------------------
# primeiro crimos uma sequência para o eixo x
xv<-1:15
# vamos criar um vetor com 15x a letra "A" do primeiro tratamento
tA<-rep("A",15)
# calculando os valores previstos para o tratamento A
yA<-predict(p01,list(local=factor(tA),dias=xv),type="response")
# vamos também criar um vetor com 15x a letra "B"  para o segundo tratamento
tB<-rep("B",15)
# calculando os valores previstos para o tratamento B
yB<-predict(p01,list(local=factor(tB),dias=xv),type="response")

## ------------------------------------------------------------------------
# plot dos dados do tratamento A
plot(N ~ dias, data=doentes[doentes$local=="A",], 
     col='tomato', ylim=y.lim, 
     ylab="N de doentes", las=1, bty='l')
# adicionando os dados do tratamento B
points(N ~ dias, data=doentes[doentes$local=="B",], col='navy')
# incluindo a legenda
legend("bottomleft", c("com vacina", "sem vacina"), 
       col=c("navy", "tomato"), pch=1)
# linha dos previstos para o tratamento A
lines(xv,yA, col="tomato")
# linha dos previstos para o tratamento B
lines(xv,yB, col="navy")


## ------------------------------------------------------------------------
flor <- read.table("../dados/flowering.txt", header=TRUE, sep='\t')

## ------------------------------------------------------------------------
summary(flor)

## ------------------------------------------------------------------------
flor$prop <- flor$flowered/flor$number

## ------------------------------------------------------------------------
hist(flor$prop)

## ------------------------------------------------------------------------
## instale o pacote para a paleta de cores usando o comando:
# install.packages("wesanderson")
## carregue o pacote 
library(wesanderson)
# crie um vetor com as cores do filme Rushmore de Wes Anderson
cor <- wes_palette("Rushmore1")

## ------------------------------------------------------------------------
## use a funcao rainbow para ecolher 5 cores diferentes
# cor <- rainbow(5)

## ------------------------------------------------------------------------
plot(prop ~ dose, data=flor, type='n', las=1, bty='l') # este comando faz com que nenhum ponto apareça no gráfico. por que?
# aqui, dependendo da paleta de cores que você escolheu seu gráfico ficará maravilhoso, ou não :P
points(prop ~ dose, data=flor[flor$variety=="A",], pch=19,
       col=cor[1])
points(prop ~ dose, data=flor[flor$variety=="B",], pch=19,
       col=cor[2])
points(prop ~ dose, data=flor[flor$variety=="C",], pch=19, 
       col=cor[3])
points(prop ~ dose, data=flor[flor$variety=="D",], pch=19,
       col=cor[4])
points(prop ~ dose, data=flor[flor$variety=="E",], pch=19,
       col=cor[5])
legend("topright", c("A", "B", "C", "D", "E"), 
       col=cor, pch=19)

## ------------------------------------------------------------------------
# primeiro criamos um vetor com a variável resposta
flor$y1 <- flor$flowered
flor$y2 <- flor$number-flor$flowered

## hipótese 1
b01 <- glm(cbind(y1,y2) ~ dose, data=flor, family="binomial")

## hipótese 2
b02 <- glm(cbind(y1,y2) ~ dose*variety, data=flor, family="binomial")

## hipótese 3
b03 <- glm(cbind(y1,y2) ~ 1, data=flor, family="binomial")

## ---- results='hide'-----------------------------------------------------
summary(b01)
summary(b02)
summary(b03)

## ------------------------------------------------------------------------
AICtab(b01, b02, b03)

## ------------------------------------------------------------------------
# variedade A
A <- glm(cbind(y1,y2) ~ dose, data=flor[flor$variety=="A",],
         family="binomial")
# variedade B
B <- glm(cbind(y1,y2) ~ dose, data=flor[flor$variety=="B",],
         family="binomial")
# variedade C
C <- glm(cbind(y1,y2) ~ dose, data=flor[flor$variety=="C",],
         family="binomial")
# variedade D
D <- glm(cbind(y1,y2) ~ dose, data=flor[flor$variety=="D",],
         family="binomial")
# variedade E
E <- glm(cbind(y1,y2) ~ dose, data=flor[flor$variety=="E",],
         family="binomial")

## ------------------------------------------------------------------------
# guardando em um objeto a variável que será o eixo x
xv<-0:30
prevA <- predict(A,list(dose=xv),type="response")
prevB <- predict(B,list(dose=xv),type="response")
prevC <- predict(C,list(dose=xv),type="response")
prevD <- predict(D,list(dose=xv),type="response")
prevE <- predict(E,list(dose=xv),type="response")

## ------------------------------------------------------------------------

### plot que já fizemos antes ###
plot(prop ~ dose, data=flor, type='n', las=1, bty='l') # este comando faz com que nenhum ponto apareça no gráfico. por que?
# aqui, dependendo da paleta de cores que você escolheu seu gráfico ficará maravilhoso, ou não :P
points(prop ~ dose, data=flor[flor$variety=="A",], pch=19,
       col=cor[1])
points(prop ~ dose, data=flor[flor$variety=="B",], pch=19,
       col=cor[2])
points(prop ~ dose, data=flor[flor$variety=="C",], pch=19, 
       col=cor[3])
points(prop ~ dose, data=flor[flor$variety=="D",], pch=19,
       col=cor[4])
points(prop ~ dose, data=flor[flor$variety=="E",], pch=19,
       col=cor[5])
legend("topleft", c("A", "B", "C", "D", "E"), 
       col=cor, pch=19)
lines(xv,prevA, col=cor[1], lwd=2)
lines(xv,prevB, col=cor[2], lwd=2)
lines(xv,prevC, col=cor[3], lwd=2)
lines(xv,prevD, col=cor[4], lwd=2)
lines(xv,prevE, col=cor[5], lwd=2)

