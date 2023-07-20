
setwd("C:/Users/Nathan/Downloads/livros economia/estatistica/puc/econometria 4")
library(stats)
library(readxl)
library(ggplot2)
library(tidyverse)
library(GGally)
#a
returns<-read_excel("returns.xlsx")
returns_pca<-returns[,-(1:17)]
returns_pca_pad<-scale(returns[,-(1:17)])
returns_pca_result <- prcomp(returns_pca)
returns_pca_pad_result <- prcomp(returns_pca_pad)
plot(returns_pca_result$x[,1], returns_pca_result$x[,2], xlab = "PC1", ylab = "PC2")


#otimo
#Informal Way

prop_var <- (returns_pca_result$sdev^2) / sum(returns_pca_result$sdev^2)
prop_var_acum <- cumsum(prop_var)
plot(prop_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

prop_pad_var <- (returns_pca_pad_result$sdev^2) / sum(returns_pca_pad_result$sdev^2)
prop_pad_var_acum <- cumsum(prop_pad_var)
plot(prop_pad_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

######Rule of Thumb

for(i in 2:length(prop_var_acum)){
  if(prop_var_acum[i] -prop_var_acum[i-1]<0.03){
    thumb<-i-1
    stop()
  }

}
thumb
for(i in 2:length(prop_var_acum)){
  if(prop_pad_var_acum[i] -prop_pad_var_acum[i-1]<0.03){
    thumb_pad<-i-1
    stop()
  }
  
}
thumb_pad

# Pela regra de bolso de adicionar os componentes que incrementam a variância explicada
# em pelo menos 3% foram selecionados 2 componentes.

#########Biggest Drop
autovalores<-returns_pca_result$sdev
autovalores1<<-c(autovalores[-1],1)
biggest_drop<-which.max(autovalores/autovalores1)
biggest_drop

autovalores_pad<-returns_pca_pad_result$sdev
autovalores1_pad<<-c(autovalores_pad[-1],1)
biggest_drop_pad<-which.max(autovalores_pad/autovalores1_pad)
biggest_drop_pad

#Pelo biggest drop foram selecionados 325 componentes.
############

# Pela regra de bolso foram selecionados os 2 primeiros componentes. Usando as variáveis padronizadas, os 2
# primeiros componentes explicam:
summary_returns_pca <- summary(returns_pca_pad_result)
print(summary_returns_pca$importance["Cumulative Proportion","PC2"])
##### 0.09503 da variância.

## b)

# Computando a relação do primeiro componente com as anomalias:

pc1_pad<-returns_pca_pad_result$x[,1]
pc1<-returns_pca_result$x[,1]

#
returns_reg<-(cbind(pc1_pad,returns[2:17]))
reg1<-lm( pc1_pad~MKT + HML + SMB + MOM1 +  MOM36 + ACC + BETA + CFP+
            DY + EP + IDIOVOL + CMA + UMD + RMW + RETVOL + CHCSHO, data= returns_reg)

# Extrair os coeficientes e intervalos de confiança do modelo
coeficientes <- coef(reg1)[-1]
intervalos_confianca <- confint(reg1)[-1,]

# Converter os coeficientes e intervalos de confiança em um data frame
df_coeficientes <- data.frame(Coeficiente = names(coeficientes),
                              Valor = coeficientes,
                              IC_Inferior = intervalos_confianca[,1],
                              IC_Superior = intervalos_confianca[,2])

# Criar o gráfico de boxplot com os coeficientes e intervalos de confiança
ggplot(df_coeficientes, aes(x = Coeficiente, y = Valor)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior),
                width = 0.2, color = "red") +
  xlab("Coeficiente") +
  ylab("Valor") +
  ggtitle("Boxplot dos Coeficientes da Regressão Múltipla") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(reg1)

# Computando a relação do segundo componente com as anomalias:

pc2_pad<-returns_pca_pad_result$x[,2]
pc2<-returns_pca_result$x[,2]

returns_reg2<-(cbind(pc2_pad,returns[2:17]))
reg2<-lm( pc2_pad~MKT + HML + SMB + MOM1 +  MOM36 + ACC + BETA + CFP+
            DY + EP + IDIOVOL + CMA + UMD + RMW + RETVOL + CHCSHO, data= returns_reg2)

# Extrair os coeficientes e intervalos de confiança do modelo
coeficientes2 <- coef(reg2)[-1]
intervalos_confianca2 <- confint(reg2)[-1,]

# Converter os coeficientes e intervalos de confiança em um data frame
df_coeficientes2 <- data.frame(Coeficiente2 = names(coeficiente2),
                              Valor2 = coeficientes2,
                              IC_Inferior2 = intervalos_confianca2[,1],
                              IC_Superior2 = intervalos_confianca2[,2])

# Criar o gráfico de boxplot com os coeficientes e intervalos de confiança
ggplot(df_coeficientes2, aes(x = Coeficiente2, y = Valor2)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = IC_Inferior2, ymax = IC_Superior2),
                width = 0.2, color = "red") +
  xlab("Coeficiente") +
  ylab("Valor") +
  ggtitle("Boxplot dos Coeficientes da Regressão Múltipla") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(reg2)

## Como eles se relacionam? O R² e o R² ajustado da regressão com o PC1 foram 0.954 e 0.9516, e para a regressão
# com o PC2 foram 0.6353 e 0.6164. Então os fatores anômalos contain quase toda a informação do PC1.
# Os R²s são menores na regressão com o PC2, mas ainda consideravelmente altos.
# A única anomalia que tem p-valor menor que 0.001 em ambas as regressões é o fator de mercado (MKT).
# Isso pode ser interpretado pelo fato dos componentes principais serem combinações lineares dos retornos do mercado, 
# logo faz sentido que o MKT faça um bom fit.

## c) 


pca16<-returns[,(2:17)]
pca16_pad<-scale(returns[,(2:17)])
pca16_result <- prcomp(pca16)
pca16_pad_result <- prcomp(pca16_pad)
plot(pca16_result$x[,1], pca16_result$x[,2], xlab = "PC1", ylab = "PC2")

#otimo
#Informal Way

prop_var <- (pca16_result$sdev^2) / sum(pca16_result$sdev^2)
prop_var_acum <- cumsum(prop_var)
plot(prop_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

prop_pad_var <- (pca16_pad_result$sdev^2) / sum(pca16_pad_result$sdev^2)
prop_pad_var_acum <- cumsum(prop_pad_var)
plot(prop_pad_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

######Rule of Thumb

for(i in 2:length(prop_var_acum)){
  if(prop_var_acum[i] -prop_var_acum[i-1]<0.03){
    thumb<-i-1
    stop()
  }
  
}
thumb
for(i in 2:length(prop_var_acum)){
  if(prop_pad_var_acum[i] -prop_pad_var_acum[i-1]<0.03){
    thumb_pad<-i-1
    stop()
  }
  
}
thumb_pad

# Pela regra de bolso de adicionar os componentes que incrementam a variância explicada
# em pelo menos 3% foram selecionados 8 componentes considerando o PCA feito com as variáveis padronizadas.

# Inspecionando os autovetores em busca das variáveis com maior ponderação:

# Olhando para o primeiro componente:
relevant_vars_PC1 <- rownames(pca16_pad_result$rotation)[order(abs(pca16_pad_result$rotation[, 1]), decreasing = TRUE)]
print(relevant_vars_PC1)
  
# Olhando para o segundo componente:
relevant_vars_PC2 <- rownames(pca16_pad_result$rotation)[order(abs(pca16_pad_result$rotation[, 2]), decreasing = TRUE)]
print(relevant_vars_PC2)

# Olhando para o terceiro componente:
relevant_vars_PC3 <- rownames(pca16_pad_result$rotation)[order(abs(pca16_pad_result$rotation[, 3]), decreasing = TRUE)]
print(relevant_vars_PC3)

# Olhando para o quarto componente:
relevant_vars_PC4 <- rownames(pca16_pad_result$rotation)[order(abs(pca16_pad_result$rotation[, 4]), decreasing = TRUE)]
print(relevant_vars_PC4)

# Ao inspecionarmos os 4 primeiros componentes principais já constatamos que para cada um deles, as variáveis
#mais relevantes (com maior peso) diferem, indicando que não há uma anomalia dominante para todos os componentes.
# Por exemplo, para o 1º, a anomalia dominante é "IDIOVOL", para o 2º é "MOM36".

## d)
# Usando os componentes selecionados dos dois grupos:

anomalies_returns_pcs <- data.frame(cbind(returns_pca_pad_result$x[,1:2], pca16_pad_result$x[,1:8]))
colnames(anomalies_returns_pcs) <- c("Rtrns1", "Rtrns2", "Anmls1", "Anmls2", "Anmls3", "Anmls4", "Anmls5", "Anmls6", "Anmls7", "Anmls8")
ggpairs(anomalies_returns_pcs)

# A maior correlação aparece entre os dois primeiros componentes de ambos os grupos. Isso ocorre pois o PCA constrói os componentes
# ordenando pela relevância explicativa das séries. Ao fazer o PCA de dois grupos de séries muito correlacionadas, é razoável
# que a correlação maior aparece entre os vetores com mais informação sobre cada grupo, os primeiros vetores.

#########
########## questao 2
dados<-read.csv("2021-12.csv")
dados$CPIAUCSL ==dados[,107] #ok

dados[3,107] - dados[2,107]
cpiaucsl<-(diff(dados$CPIAUCSL[-1] ) / lag(dados$CPIAUCSL[-1])[-1]) # yt - yt_1 / yt





max_ordem <- 10
bic_valores <- numeric(max_ordem)
modelo <-ar(cpiaucsl, order = 2)
BIC(modelo)
for (p in 1:max_ordem) {
  modelo <- arima(cpiaucsl, order = c(p,0,0))
  bic_valores[p] <- BIC(modelo)
}
ordem_otima <- which.min(bic_valores)
ordem_otima
modelo <- arima(cpiaucsl, order = c(ordem_otima,0,0))
previsoes <- predict(modelo, n.ahead = 1)$pred

coeficientes_ar <- coef(modelo)
sim<-cpiaucsl[746:754]
simt<-rev(c(1,sim))

prev<-coeficientes_ar%*%simt
######### ar + pcr 

#olhar as trasnformadas e tirar os pcrs
library(dynlm)

pi<-cpiaucsl[2:(length(pc1)+1) ]
m <- dynlm(pi ~ L(pi, 1) + L(pi, 2) + L(pi, 3) + pc1 + pc2)
autovalores1_pad<<-c(autovalores_pad[-1],1)
biggest_drop_pad<-which.max(autovalores_pad/autovalores1_pad)
biggest_drop_pad

############



