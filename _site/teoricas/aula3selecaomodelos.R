################################################################
###### Código para as analises da aula 3 selecao de modelos ###
################################################################

# elaborado por Sara Mortara para disciplina de modelagem estatistica
# dezembro de 2018

# define o diretorio de trabalho
### ATENCAO! mude para o SEU diretorio de trabalho
setwd("~/Dropbox/posdoc/disciplina_modelagem/atividadesR/teoricas")

# carregando pacotes necessarios
## ATENCAO! 
## nesta pratica precisaremos do pacote bbmle
## se voce nao tem instalado, precisa antes rodar o comando:
## install.packages("bbmle")
## se já tiver instalado, apenas carregue o pacote
library(bbmle)

##############################
# PARTE 1: lendo os dados ####
##############################

# lendo os dados de salario e experiencia de homens e mulheres
sal <- read.csv("../dados/salarioHM.csv")

# primeiras seis linhas dos dados
head(sal)

# estrutra dos dados
str(sal)

# sumario dos dados
summary(sal)

###################################
# Passo 2: visualizando os dados ##
###################################

# boxplot do salario por sexo
boxplot(salario ~ sexo, data=sal, ylab="salário")

# boxplor com cores diferentes por sexo
## tomate para homens e marinho para mulheres 
boxplot(salario ~ sexo, data=sal, ylab="salário", col=c("tomato", "navy"))

# grafico de dispersao salario ~ experiencia
plot(salario ~ experiencia, data=sal, 
     xlab="experiência (anos)", 
     ylab="salário (reais)")

# grafico de dispersao salario ~ experiencia
## agora incluindo cores diferentes para cada grupo
plot(salario ~ experiencia, data=sal, 
     col=ifelse(sal$sexo=="H", "tomato", "navy"), # homens cor tomate e mulheres marinho
     xlab="experiência (anos)", 
     ylab="salário (reais)")


#####################################
# Passo 3:  construcao dos modelos ##
#####################################

# 3.1 pessoas com mais experiência ganham mais independente do sexo
h01 <- lm(salario ~ experiencia, data=sal)

# sumario da hipotese 1
summary(h01)

# 3.2 homens ganham mais do que mulheres independente da experiencia
h02 <- lm(salario ~ sexo, data=sal)

# sumario da hipotese 2
summary(h02)

# 3.3 homens ganham mais do que mulheres com a mesma experiencia
h03 <- lm(salario ~ experiencia + sexo, data=sal)

# sumario da hipotese 3
summary(h03)

# 3.4 ausencia de efeito da experiencia e sexo no salario
h00 <- lm(salario ~ 1, data=sal)

# sumario da hipotese 4
summary(h00)

###########################################
# Passo 4:  selecao dos modelos com AIC ##
##########################################

# extraindo o valor de AIC de cada modelo
AIC(h01)
AIC(h02)
AIC(h03)
AIC(h00)

# construindo a tabela de AIC 
## aqui usamos a funcao AICtab do pacote bbmle 
AICtab(h01, h02, h03, h00, weights=TRUE, base=TRUE)

# vamos checar os residuos do modelo selecionado
plot(h03)

#########################################################
# Passo 5: visualizacao do ajuste do modelo aos dados ##
#########################################################

# vamos criar um objeto com os coeficientes do melhor modelo
h03.coef <- coef(h03)

# primeiro o grafico com os dados
plot(salario ~ experiencia, data=sal, 
     col=ifelse(sal$sexo=="H", "tomato", "navy"), # homens cor tomate e mulheres marinho
     xlab="experiência (anos)", 
     ylab="salário (reais)")
# linha do ajuste para os homens
abline(h03.coef[1], h03.coef[2], col="tomato", lwd=2)
abline(h03.coef[1]+h03.coef[3], h03.coef[2], col="navy", lwd=2)

