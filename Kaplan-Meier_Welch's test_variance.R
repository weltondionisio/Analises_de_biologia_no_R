library(readxl)
read_excel("body_size.xlsx")
instarII<-read_excel("body_size.xlsx", sheet=1, col_names=T)
attach(instarII)
na1<- (instarII$`litter 1`)
na2 <- na.omit(instarII$`litter 2`, NA)
str(na1)
str(na2)

#Comparando tamanho do prossoma (proxy de tamanho total) no instar II:
#install.packages("normtest", dependencies = T)
library(normtest)
ajb.norm.test(na1, nrepl=46) #testar normalidade
ajb.norm.test(na2, nrepl=29) #testar normalidade
library(stats)
var.test(na1, na2) # testar variâncias
wilcox.test(na1,na2,paired=F) #teste de Wilcoxon Mann-Whitney para dados com variâncias desiguais


#repetir processo no instar III:
instarIII<-read_excel("body_size.xlsx", sheet=2, col_names=T)
attach(instarIII)
na1<- (instarIII$`litter 1`)
na2 <- na.omit(instarIII$`litter 2`, NA)
str(na1)
str(na2)
ajb.norm.test(na1, nrepl=45) #testar normalidade
ajb.norm.test(na2, nrepl=27) #testar normalidade
wilcox.test(na1,na2,paired=F)

#repetir processo no instar IV:
instarIV<-read_excel("body_size.xlsx", sheet=3, col_names=T)
attach(instarIV)
na1<- (instarIV$`litter 1`)
na2 <- na.omit(instarIV$`litter 2`, NA)
str(na1)
str(na2)
ajb.norm.test(na1, nrepl=41) #testar normalidade
ajb.norm.test(na2, nrepl=24) #testar normalidade
wilcox.test(na1,na2,paired=F)

#repetir processo no instar V:
instarV<-read_excel("body_size.xlsx", sheet=4, col_names=T)
attach(instarV)
na1<- (instarV$`litter 1`)
na2 <- na.omit(instarV$`litter 2`, NA)
ajb.norm.test(na1, nrepl=29) #testar normalidade
ajb.norm.test(na2, nrepl=19) #testar normalidade
var.test(na1, na2) # testar variâncias 
t.test (na1, na2) #Teste T de Welch para dados normais com variâncias desiguais



#repetir processo no instar VI:
instarVI<-read_excel("body_size.xlsx", sheet=5, col_names=T)
attach(instarVI)
na1<- (instarVI$`litter 1`)
na2 <- na.omit(instarVI$`litter 2`, NA)
str(na1)
str(na2)
ajb.norm.test(na1, nrepl=11) #testar normalidade
ajb.norm.test(na2, nrepl=2) #testar normalidade
var.test(na1, na2) # testar variâncias 
wilcox.test(na1,na2,paired=F)


#Ajustando os valores de P através da correção de Bonferroni afim de evitar erro tipo I
require(graphics)
require(stats)
p.adjust(0.3142, "bonferroni", n = 5) #instar II = 1
p.adjust(0.9625, "bonferroni", n = 5) #instar III = 1
p.adjust(0.05024, "bonferroni", n = 5)   #instar IV = 0.2512
p.adjust(0.1245, "bonferroni", n = 5)    #instar V = 0.6225
p.adjust(0.5448, "bonferroni", n = 5)    #instar VI = 1


#Os mesmos procedimentos são repetidos para comparar o tempo de desenvolvimento
dev_time <-read_excel("developmental_time.xlsx")
summary(dev_time)
instarII<-read_excel("developmental_time.xlsx", sheet=1, col_names=T)
attach(instarII)
na1<- (instarII$`litter 1`)
na2 <- na.omit(instarII$`litter 2`, NA)

#Repetir processo para o instarI:
library(normtest)
ajb.norm.test(na1, nrepl=47) #dados não foram normais
ajb.norm.test(na2, nrepl=27) #dados não foram normais
library(stats)
var.test(na1, na2) # variâncias foram desiguais -> recorrer ao teste Mann-Whitney 
wilcox.test(na1,na2,paired=F)


#Repetir processo para o instar III:
instarIII<-read_excel("developmental_time.xlsx", sheet=2, col_names=T)
attach(instarIII)
na1<- (instarIII$`litter 1`)
na2 <- na.omit(instarIII$`litter 2`, NA)
library(normtest)
ajb.norm.test(na1, nrepl=45) #dados não foram normais
ajb.norm.test(na2, nrepl=25) #dados não foram normais
library(stats)
var.test(na1, na2) # variâncias foram desiguais -> recorrer ao teste de Mann-Whitney
wilcox.test(na1,na2,paired=F) 

#Repetir processo para o instar IV:
instarIV<-read_excel("developmental_time.xlsx", sheet=3, col_names=T)
attach(instarIV)
na1<- (instarIV$`litter 1`)
na2 <- na.omit(instarIV$`litter 2`, NA)
library(normtest)
ajb.norm.test(na1, nrepl=34) #dados não foram normais
ajb.norm.test(na2, nrepl=20) #dados não foram normais
library(stats)
var.test(na1, na2)
wilcox.test(na1,na2,paired=F)


#Repetir processo para o instar V:
instarV<-read_excel("developmental_time.xlsx", sheet=4, col_names=T)
attach(instarV)
na1<- (instarV$`litter 1`)
na2 <- na.omit(instarV$`litter 2`, NA)
library(normtest)
ajb.norm.test(na1, nrepl=16) #dados não foram normais
ajb.norm.test(na2, nrepl=9) #dados não foram normais
library(stats)
wilcox.test(na1,na2,paired=F) 

#Ajuste dos valores de P através da correção de Bonferroni para múltiplas comparações
require(graphics)
require(stats)
p.adjust(0.006175, "bonferroni", n = 4) #instar II = 0.0247
p.adjust(0.004956, "bonferroni", n = 4) #instar III = 0.019824
p.adjust(0.4847, "bonferroni", n = 4)   #instar IV = 1
p.adjust(0.846, "bonferroni", n = 4)    #instar V = 1

#COMPARAÇÃO DE SOBREVIVÊNCIA DAS PROLES (entre ninhadas 1 e 2)
#Através do teste de Kaplan-Meier:

library(readxl)
#carregando arquivo excel
d2<-read_excel("Pasta2.xlsx", sheet = 2, col_names = T)
dy<-as.data.frame(d2) #convertendo para uma matriz de dados
dy #conferindo dados
#install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
fit <- survfit(Surv(dtime, status) ~ lorder, data = dy)
print(fit)
print(fit)
summary(fit)$table #sumário do resultado
d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)

#plotando os resultados de sobrevivência
ggsurvplot(fit, 
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

#plotando os resultados de sobrevivência em um gráfico mais minimalista
library(ggplot2)
ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  linetype = 7,
  pval = F,             # show p-value of log-rank test.
  conf.int = F,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = , # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = F,      # plot the number of censored subjects at time t
  legend.labs = 
    c("Litter 1", "Litter 2"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)