library(readxl)
reprod3<- read_excel("invest.reprod3.xlsx", sheet=1, col_names = T)
attach(reprod3)
TOM<- (reprod3$TOM2)
ON<- (reprod3$OS)
FS<- (reprod3$FS)
FM<- (reprod3$FM)

#testando a normalidade das variáveis
shapiro.test(TOM)
shapiro.test(ON)
shapiro.test(FS)
shapiro.test(FM)



#Modelo Linear Geral com critério de Akaike para testar
#qual o melhor modelo que representa a influência da condição maternal sobre a ninhada

#glm com número de filhotes na ninhada como variável dependente das condições maternais
glmON <- glm(ON ~ FS * FM, family = gaussian())
library(MASS)
summary(glmON)
anova(glmON)

#glm com massa total da ninhada como variável dependente das condições maternais
glmTOM <- glm(TOM ~ FS * FM, family = gaussian())
library(MASS)
summary(glmTOM)
anova(glmTOM)


ggplot(reprod3, aes(x = ON, y = TOM, color = FM) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #gráfico de dispersão entre tom, on e FM

plot(glmON) #Aperte <Return> para exibir os gráficos do primeiro modelo

plot(glmTOM) #Aperte <Return> para exibir os gráficos do segundo modelo


#modelo alternativo 1
model1<- lm (ON ~ FS * FM * TOM)
summary(model1)

#modelo alternativo 2
model2<- lm (TOM ~ FS * FM * TOM)
summary(model2)


##testando o melhor modelo com o AIC##
AIC(glmON, glmTOM, model1, model2) #glmON e glmTOM foram os melhores

step(glmON, scale = 0,
     direction = c("both", "backward", "forward"),
     trace = 1, keep = NULL, steps = 1000, k = 2) #checando o AIC passo-a-passo

step(glmTOM, scale = 0,
     direction = c("both", "backward", "forward"),
     trace = 1, keep = NULL, steps = 1000, k = 2) #checando o AIC passo-a-passo



#Ajustando o valor de P dos modelos com correção de Bonferroni
#para evitar erro tipo I e obtendo os resíduos de ON e TOM em seus respectivos modelos

p.adjust(0.6491, "bonferroni", n = 2) 
r1<-residuals(glmON) #obtendo os resíduos de ON (dado padronizado sem efeito maternal)
r1

p.adjust(0.05092, "bonferroni", n = 2) 
r2<-residuals(glmTOM) #obtendo os resíduos de TOM (dado padronizado sem efeito maternal)
r2


shapiro.test(r1) #testando a normalidade dos resíduos
shapiro.test(r2) #testando a normalidade dos resíduos


#correlação entre os resíduos 
rescor <- cor.test(r1, r2, method = c("spearman"))

#criando dataframe com os resíduos
dfres <- data.frame(r1, r2)
dfres

#produzindo um gráfico de dispersão da correlação (spearman) entre os resíduos através do dataframe

library("ggpubr")
ggscatter(dfres, x = "r1", y = "r2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = F, cor.method = "spearman",
          xlab = "Offspring number", ylab = "Total offspring mass (g)")