library(tidyverse)
library(fmsb)

carros = read.csv("F1.csv",sep=";",header = T)
equipe = unique(carros$Construtor)

car = t(carros)
car = as.data.frame(car)
rownames(car) <- c("","Número_GPs","Número_Temporadas","Número_Pilotos",
                   "Vitórias","Poles","Voltas mais rápidas","Pódios")
names(car) <- car[1,]
car <- car[-1,]
for(i in 1:60){
  car[,i] <- sapply(car[,i], as.numeric)
}

names(carros) <- c("","Número_GPs","Número_Temporadas","Número_Pilotos",
                   "Vitórias","Poles","Voltas mais rápidas","Pódios")
rownames(carros) <- equipe
carros <- carros[,-1]

a = c(1040,1040,1040,1040,1040,1040,1040)
b = c(0,0,0,0,0,0,0)
carros = rbind(a,b,carros)

for(i in 1:7){
  carros[,i] <- sapply(carros[,i], as.numeric)
}

####################################

par(mar=c(4,11,4,4))
bp = barplot(car[,2],horiz = T,names.arg = rownames(car),col="orange",las=1,xaxt="n")
text(5,bp,car[,2],cex = 1)

##__________________________________________________________________________
radarchart(carros[c(1,2,"BAR"),], 
           pcol=rgb(0.2,0.5,0.5,0.9),pfcol=rgb(0.2,0.5,0.5,0.5),plwd=3,
           axistype=1,caxislabels = seq(0,1050,260),cglcol="grey",
           cglty=1,axislabcol="grey",cglwd=0.8,vlcex=0.8)