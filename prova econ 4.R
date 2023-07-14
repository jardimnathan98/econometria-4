
setwd("C:/Users/Nathan/Downloads/livros economia/estatistica/puc/econometria 4")
library(stats)
library(readxl)
library(ggplot2)
library(tidyverse)
#a
returns<-read_excel("returns.xlsx")
pca<-returns[,-(1:17)]
pca_pad<-scale(returns[,-(1:17)])
pca_result <- prcomp(pca)
pca_pad_result <- prcomp(pca_pad)
plot(pca_result$x[,1], pca_result$x[,2], xlab = "PC1", ylab = "PC2")

#otimo
#Informal Way

prop_var <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
prop_var_acum <- cumsum(prop_var)
plot(prop_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

prop_pad_var <- (pca_pad_result$sdev^2) / sum(pca_pad_result$sdev^2)
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

#########Biggest Drop
autovalores<-pca_result$sdev
autovalores1<<-c(autovalores[-1],1)
biggest_drop<-which.max(autovalores/autovalores1)
biggest_drop

autovalores_pad<-pca_pad_result$sdev
autovalores1_pad<<-c(autovalores_pad[-1],1)
biggest_drop_pad<-which.max(autovalores_pad/autovalores1_pad)
biggest_drop_pad

############


pc1<-pca_pad_result$x[,1]
dados<-data.frame(pc1, scale(returns[,2:17]))
scale(returns[,2:17])
ggplot(dados, aes(pc1, MKT)) +
  geom_point() +
  labs(x = "Variável Independente", y = "Variável Dependente") +
    ylim(-0.2, 0.15) +  # Definir limites no eixo x
    xlim(-15, 25) 
cor(pca_result$x[,1], returns[,2])
pca1<-pca_result$x[,1]
returns_reg<-(cbind(pca1,returns[2:17]))
reg1<-lm( pca1~MKT + HML + SMB + MOM1 +  MOM36 + ACC + BETA + CFP+
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


###########


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

#########Biggest Drop
autovalores<-pca16_result$sdev
autovalores1<<-c(autovalores[-1],1)
biggest_drop<-which.max(autovalores/autovalores1)
biggest_drop

autovalores_pad<-pca16_pad_result$sdev

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

#economia




