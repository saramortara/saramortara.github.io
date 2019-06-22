##########################################
## A primeira visão do R #################
##########################################

# Codigo elaborado por Sara Mortara
# apresentacao do R para disciplina de modelagem estatistica

# definindo o diretorio de trabalho
setwd("~/Dropbox/posdoc/disciplina_modelagem/teoricas")
# para quem usa Windows: 
## precisa separar os diretorios por duas barras
#setwd("~//Fulano//uso_windows//preciso_de_duas_barras")
## ou separar por uma barra invertida
#setwd("~\Fulano\uso_windows\preciso_barra_invertida")

# checando o diretorio de trabalho
getwd()

# como citar o R
## usando a funcao citation
citation()

## lista objetos na area de trabalho 
ls()

# o R é uma calculadora
# somando
2+3

# log
log(10)

# raiz quadrada
sqrt(12)

# 1. usando as funcoes que existem no r
## funcoes sao objetos que existem dentro do R

# concatenar números
c(0, 7, 3, 4, 5)

# concatenar letras
c("a", "b", "c", "d", "e")

# funcao media
mean(c(0, 7, 3, 4, 5))

# funcao desvio padrao
sd(c(0, 7, 3, 4, 5))

# funcao variancia
var(c(0, 7, 3, 4, 5))

# 2. carregando um objeto que existe dentro do R
# vetor de letras maiusculas e minusculas
letters
LETTERS

# outra forma de criar o vetor de a a e
letters[1:5]

# carregando um conjunto de dados que existe no R
data(cars)

cars

# 3. usando a funcao plot para visualizar um dado
plot(dist ~ speed, data=cars)

cars$dist

# outra forma de fazer o mesmo
plot(cars$dist ~ cars$speed, main="Título", xlab="velocidade", 
     ylab="distância")

# 4. criando objetos dentro do r
x <- c(0, 7, 3, 4, 5)

x

# usando o objeto no R
# media do x
mean(x) # media do x
var(x)
sd(x)

# 5. lendo um conjunto de dados no R

## lendo um arquivo csv
meucsv <- read.csv("meudado.csv", dec=".")

meucsv

# checando o arquivo de dados
head(meucsv)
summary(meucsv)

meucsv

## lendo um arquivo txt
meutxt <- read.table("meudado.txt", sep="\t", header=TRUE) # \t é o simbolo de tabulacao

meutxt

# checando o arquivo de dados
head(meutxt)
summary(meutxt)

# listando o que existe na area de trabalho

ls()

# 6. instalando e carregando pacotes
# vamos usar o pacote bbmle 

# se voce nao tem o pacote instalado
install.packages("bbmle")

# para carregar um pacote
library(bbmle)
