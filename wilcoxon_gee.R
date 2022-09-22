library(readxl)
library(normtest)

#Carregando planilhas com diferentes comportamentos
lpa<-read_excel("b_acts.xlsx", sheet=3, col_names=T)
attach(lpa)
llpa<-read_excel("b_acts.xlsx", sheet=4, col_names=T)
attach(llpa)
t<-read_excel("b_acts.xlsx", sheet=5, col_names=T)
attach(t)
tf<-read_excel("b_acts.xlsx", sheet=6, col_names=T)
attach(tf)
impa<-read_excel("b_acts.xlsx", sheet=7, col_names=T)
attach(impa)
l<-read_excel("b_acts.xlsx", sheet=8, col_names=T)
attach(l)
bt<-read_excel("b_acts.xlsx", sheet=10, col_names=T)
attach(bt)
impf<-read_excel("b_acts.xlsx", sheet=11, col_names=T)
attach(impf)

#Testando normalidade e prosseguindo para o teste de hipótese
#(controle x tratamento)

#foraging
ac3<-(lpa$c)
at3<-(lpa$t)
ajb.norm.test(ac3, nrepl=25)
ajb.norm.test(at3, nrepl=25)
wilcox.test(ac3,at3,paired=F) 

#ajustando o p para a quantidade de testes que serão realizados
p.adjust(1.228e-08, "bonferroni", n = 8)

#slowly foraging
ac4<-(llpa$c)
at4<-(llpa$t)
ajb.norm.test(ac4, nrepl=25)
ajb.norm.test(at4, nrepl=25)
wilcox.test(ac4,at4,paired=F) 

#groping
ac5<-(t$c)
at5<-(t$t)
ajb.norm.test(ac5, nrepl=25)
ajb.norm.test(at5, nrepl=25)
wilcox.test(ac5,at5,paired=F) 

#climbing
ac6<-(tf$c)
at6<-(tf$t)
ajb.norm.test(ac6, nrepl=25)
ajb.norm.test(at6, nrepl=25)
wilcox.test(ac6,at6,paired=F) 

#alert
ac7<-(impa$c)
at7<-(impa$t)
ajb.norm.test(ac7, nrepl=25)
ajb.norm.test(at7, nrepl=25)
wilcox.test(ac7,at7,paired=F) 

#cleaning
ac8<-(l$c)
at8<-(l$t)
ajb.norm.test(ac8, nrepl=25)
ajb.norm.test(at8, nrepl=25)
wilcox.test(ac8,at8,paired=F) 

#tail-wagging
ac10<-(bt$c)
at10<-(bt$t)
ajb.norm.test(ac10, nrepl=25)
ajb.norm.test(at10, nrepl=25)
wilcox.test(ac10,at10,paired=F) 


#Rest
ac11<-(impf$c)
at11<-(impf$t)
ajb.norm.test(ac11, nrepl=25)
ajb.norm.test(at11, nrepl=25)
wilcox.test(ac11,at11,paired=F)

#testando se há diferença de persistência (tempo) entre
#área controle e área tratamento e através do tempo (GEE)

library("geepack")
gee <- read_excel("gee.xlsx", sheet = 1, col_names = T)
attach(gee)
gee$Ess <- as.factor(gee$Ess)
gee$Time <- as.factor(gee$Time)
mf <- formula(Pers ~ Ess + Time)

geeInd <- geeglm(mf, id=Ind, data=gee, family="poisson", corstr="ind")
summary(geeInd)