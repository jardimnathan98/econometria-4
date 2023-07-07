
setwd("C:/Users/Nathan/Downloads/livros economia/estatistica/puc/econometria 4")
library(stats)
library(readxl)
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
