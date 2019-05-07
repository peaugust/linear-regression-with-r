#SCRIPT PARA O TRABALHO DE PREDIÇÃO
#AUTOR: PEDRO FREDDI - <pedro.freddi@grad.ufsc.br>
#-----------------PREPARAÇÃO PARA A LEITURA DOS DADOS-----------------
#REMOVE OS OBJETOS DA MEMÓRIA
#A FUNÇÃO rm() TEM COMO PARÂMETRO UMA LISTA DE OBJETOS
#NESSE CASO USO A FUNÇÃO ls() QUE LISTA OS OBJETOS CRIADOS
rm(list=ls())

#MOSTRA O DIRETÓRIO DE TRABALHO DO R
getwd()

#ALTERA O DIRETÓRIO DE TRABALHO DO R PARA OUTRO ENDEREÇO
#INSIRA ABAIXO O ENDEREÇO RELATIVO ÀS PASTAS DO SEU COMPUTADOR
setwd("/home/pedro/Documents/Predicao")

#O OBJETO cigarettesTable RECEBE UM ARQUIVO .csv
cigarettesTable <- read.csv("CigarettesSW.csv", header = T)

#ANEXO A TABELA AO CAMINHO DE PESQUISA DO R PARA USAR OS NOMES DO CABEÇALHO DA TABELA 
attach(cigarettesTable)
#-------------ANALISE EXPLORATÓRIA DOS DADOS-------------------
#ESTATÍSTICAS DESCRITIVAS DAS VARIÁVEIS E VARIABILIDADE
summary(taxs[0:50])
var(taxs[0:50])
sd(taxs[0:50])

summary(price[0:50])
var(price[0:50])
sd(price[0:50])
#-------------DIAGRAMA DE DISPERSÃO------------------
#ANÁLISE VISUAL DA DISPERSÃO DOS PONTOS
plot(taxs[0:50],price[0:50], main = "Taxs X Price", xlab = "TAXS", ylab = "PRICE")
#-------------ANALISE DE CORRELAÇÃO LINEAR------------------
#ANALISO A CORRELAÇÃO DAS VARIÁVEIS TAXS E PRICE
cor(taxs[0:50], price[0:50])

#FAÇO O TESTE DE ASSOCIAÇÃO/CORREÇÃO ENTRE AS VARIÁVEIS
cor.test(taxs[0:50], price[0:50])

#-------------REGRESSÃO LINEAR SIMPLES-----------------
#AJUSTO O MODELO E COLOCO NO OBJETO O MODELO AJUSTADO
modeloAjustado <- lm(price[0:50]~taxs[0:50])
modeloAjustado
summary(modeloAjustado)
#GERO O RESUMO DO OBJETO
summary(modeloAjustado)

#ESBOÇO DA RETA DE MÍNIMOS QUADRADOS AJUSTADA
abline(lm(price[0:50]~taxs[0:50]))

#---------------ANALISE ADICIONAL----------------------
#REMOVENDO PONTO DISCREPANTE
priceAjustado <- price[0:48]
priceAjustado <- c(priceAjustado, price[50])
taxsAjustado <- taxs[0:48]
taxsAjustado <- c(taxsAjustado, taxs[50])

#ANALISO A CORRELAÇÃO DAS VARIÁVEIS TAXS E PRICE
cor(taxsAjustado, priceAjustado)

#USO O PLOT PARA COLOCAR NO GRÁFICO E FAZER A ANÁLISE VISUAL
plot(taxsAjustado, priceAjustado, main = "Taxs X Price Adjusted", xlab = "TAXS", ylab = "PRICE")

#FAÇO O TESTE DE ASSOCIAÇÃO/CORREÇÃO ENTRE AS VARIÁVEIS
cor.test(taxsAjustado, priceAjustado)

#AJUSTO O MODELO E COLOCO NO OBJETO O MODELO AJUSTADO
modeloAjustado2 <- lm(priceAjustado~taxsAjustado)
modeloAjustado2

#GERO O RESUMO DO OBJETO
summary(modeloAjustado2)

#FAÇO UM PLOT DO GRÁFICO COM A LINHA DE MÍNIMOS QUADRADOS
plot(taxsAjustado, priceAjustado, main = "Taxs X Price", xlab = "TAXS", ylab = "PRICE")
abline(lm(priceAjustado~taxsAjustado))
