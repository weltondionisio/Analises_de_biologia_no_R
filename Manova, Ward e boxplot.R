library(readxl)
data<-read_excel("eusarcusT.xlsx")
mydata <- na.omit(data)
mydata
require(vegan)
citation("vegan")

#criando um dendrograma que agrupa os indivíduos por similaridade morfológica
pd <- vegdist(mydata[,2:6], method = 'euclidean', binary = F)
pc <- hclust(pd, method = 'ward.D2')
pc
n <- c('E. sp. 2', 'E. sp. 3', 'E. sp. 4', 'E. mg', 'E. sp. 6')
pc$labels <- n
de <- as.dendrogram(pc)
par(mar=c(4.2, 2, 0.5, 11), family = 'serif')
par(pc$labels, font = 3)
tree<-plot(de, horiz = T, xlab = 'Ward', main = NULL)


#criando objetos para cada caractere morfológico mensurado
data2<-read_excel("eusarcusRM.xlsx")
mydata2 <- na.omit(data2)
library(dplyr)
PDP <- mydata2$PDP_SL
LI <- mydata2$LI_SL
LII <- mydata2$LII_SL
TrsII <- mydata2$TrsII_SL
LIII <- mydata2$LIII_SL
LIV <- mydata2$LIV_SL
Subject <- mydata2$Subject

#utilizando a manova para avaliar diferenças entre a morfologia das espécies
res.man <- manova(cbind(PDP, LI, LII, TrsII, LIII, LIV) ~ Species, data = mydata2)
summary(res.man)
summary.aov(res.man)
#citation("MANOVA.RM")

#install.packages("MANOVA.RM", dep=T)
library(MANOVA.RM)

library(FSA)

#teste post hoc de dunn com correção de bonferroni para evitar erro do tipo I
#perform Dunn's Test with Bonferroni correction for p-values
dunnTest(PDP ~ Species,
         data=mydata2,
         method="hochberg")

dunnTest(LI ~ Species,
         data=mydata2,
         method="hochberg")

dunnTest(LII ~ Species,
         data=mydata2,
         method="hochberg")

dunnTest(LIII ~ Species,
         data=mydata2,
         method="hochberg")

dunnTest(LIV ~ Species,
         data=mydata2,
         method="hochberg")

dunnTest(TrsII ~ Species,
         data=mydata2,
         method="hochberg")


#BOXPLOT das variações das estruturas morfológicas

dev.off()
boxPDP <- read_excel("eusarcusbox.xlsx", sheet = 1, col_names = T)
boxLI <- read_excel("eusarcusbox.xlsx", sheet = 2, col_names = T)
boxLII <- read_excel("eusarcusbox.xlsx", sheet = 3, col_names = T)
boxLIII <- read_excel("eusarcusbox.xlsx", sheet = 4, col_names = T)
boxLIV <- read_excel("eusarcusbox.xlsx", sheet = 5, col_names = T)
boxTrsII <- read_excel("eusarcusbox.xlsx", sheet = 6, col_names = T)

names <- c(rep("sp2", 8) , rep("sp3", 6) , rep("sp4", 2), rep("sp6", 12), rep("Emg", 11))
names

#boxplot das variações morfológicas
#PDP

sp2<-na.omit(boxPDP$sp2)
sp3<-na.omit(boxPDP$sp3)
sp4<-na.omit(boxPDP$sp4)
sp6<-na.omit(boxPDP$sp6)
Emg<-na.omit(boxPDP$Emg)

sp2
value <- c(sample(sp2, 8, replace=T), sample(sp3, 6, replace=T), 
            sample(sp4, 2, replace=T), sample(sp6, 12, replace=T), sample(Emg, 11, replace=T))
value
data <- data.frame(names,value)
data
data$namess <- factor(data$names, levels=c("sp2", "sp3", "sp4", "sp6", "Emg"))
box1<- boxplot(data$value ~ data$namess, col=rgb(0.5,0.5,0.4,0.6), ylab="Comprimento (mm)", xlab="Esp?cies")
dev.off()

#LI

sp2<-na.omit(boxLI$sp2)
sp3<-na.omit(boxLI$sp3)
sp4<-na.omit(boxLI$sp4)
sp6<-na.omit(boxLI$sp6)
Emg<-na.omit(boxLI$Emg)

value <- c( sample(sp2, 8, replace=T), sample(sp3, 6, replace=T), 
            sample(sp4, 2, replace=T), sample(sp6, 12, replace=T), sample(Emg, 11, replace=T))
data2 <- data.frame(names,value)
data2$names <- factor(data2$names, levels=c("sp2", "sp3", "sp4", "sp6", "Emg"))
box2 <- boxplot(data2$value ~ data2$names, col=rgb(0.5,0.5,0.4,0.6), ylab="Comprimento (mm)", xlab="Esp?cies")

#LII

sp2<-na.omit(boxLII$sp2)
sp3<-na.omit(boxLII$sp3)
sp4<-na.omit(boxLII$sp4)
sp6<-na.omit(boxLII$sp6)
Emg<-na.omit(boxLII$Emg)

value <- c( sample(sp2, 8, replace=T), sample(sp3, 6, replace=T), 
            sample(sp4, 2, replace=T), sample(sp6, 12, replace=T), sample(Emg, 11, replace=T))
data3 <- data.frame(names,value)
data3$names <- factor(data3$names, levels=c("sp2", "sp3", "sp4", "sp6", "Emg"))
box3 <- boxplot(data3$value ~ data3$names, col=rgb(0.5,0.5,0.4,0.6), ylab="Comprimento (mm)", xlab="Esp?cies")

#LIII

sp2<-na.omit(boxLIII$sp2)
sp3<-na.omit(boxLIII$sp3)
sp4<-na.omit(boxLIII$sp4)
sp6<-na.omit(boxLIII$sp6)
Emg<-na.omit(boxLIII$Emg)

value <- c( sample(sp2, 8, replace=T), sample(sp3, 6, replace=T), 
            sample(sp4, 2, replace=T), sample(sp6, 12, replace=T), sample(Emg, 11, replace=T))
data4 <- data.frame(names,value)
data4$names <- factor(data4$names, levels=c("sp2", "sp3", "sp4", "sp6", "Emg"))
box4<-boxplot(data4$value ~ data4$names, col=rgb(0.5,0.5,0.4,0.6), ylab="Comprimento (mm)", xlab="Esp?cies")

#LIV

sp2<-na.omit(boxLIV$sp2)
sp3<-na.omit(boxLIV$sp3)
sp4<-na.omit(boxLIV$sp4)
sp6<-na.omit(boxLIV$sp6)
Emg<-na.omit(boxLIV$Emg)

value <- c( sample(sp2, 8, replace=T), sample(sp3, 6, replace=T), 
            sample(sp4, 2, replace=T), sample(sp6, 12, replace=T), sample(Emg, 11, replace=T))
data5 <- data.frame(names,value)
data5$names <- factor(data5$names, levels=c("sp2", "sp3", "sp4", "sp6", "Emg"))
box5<-boxplot(data5$value ~ data5$names, col=rgb(0.5,0.5,0.4,0.6), ylab="Comprimento (mm)", xlab="Esp?cies")

#TrsII

sp2<-na.omit(boxTrsII$sp2)
sp3<-na.omit(boxTrsII$sp3)
sp4<-na.omit(boxTrsII$sp4)
sp6<-na.omit(boxTrsII$sp6)
Emg<-na.omit(boxTrsII$Emg)


value <- c( sample(sp2, 8, replace=T), sample(sp3, 6, replace=T), 
            sample(sp4, 2, replace=T), sample(sp6, 12, replace=T), sample(Emg, 11, replace=T))
data6 <- data.frame(names,value)
data6$names <- factor(data6$names, levels=c("sp2", "sp3", "sp4", "sp6", "Emg"))
box6<-boxplot(data6$value ~ data6$names, col=rgb(0.5,0.5,0.4,0.6) , ylab="Comprimento (mm)", xlab="Esp?cies")
