library(FactoMineR) #carregando pacote
library(readxl) #carregando pacote
library(factoextra) #carregando pacote
sheet<-read_excel("sheet.xlsx") #carregando planilha de dados
MCA(sheet, ncp=5, graph=T) #análise de múltipla correspondência (MCA)
res.mca<-MCA(sheet, graph=F) #obter resíduos da MCA
print(res.mca) #gráfico da MCA
res.mca$eig #Auto-valores da MCA
res.mca$var$cos2 #qualidade da representação na MCA
res.mca$var$contrib #contribuição na MCA
res.mca$call$marge.col #peso das variáveis
res.mca$var$v.test #teste V para selecionar as variáveis importantes
res.mca$var$coord
res.mca$ind
fviz_mca_var(res.mca, col.var = "coord", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, ggtheme=theme_minimal())
fviz_mca_ind(res.mca, col.ind = "coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()) #gráfico das dimensões 1 e 2 da MCA


#install.packages("groupdata2", dep=T)
#install.packages("dplyr", dep=T)
#install.packages("knitr", dep=T)
library("groupdata2")
library("dplyr")
library("knitr")

sheet2<-read_excel("sheet2.xlsx") 
dim1<-sheet2$dim1
dim2<-sheet2$dim2
dfdim1<- as.data.frame(dim1)
dfdim2<- as.data.frame(dim2)
df_grouped<- group(dfdim1, n=3, method="n_rand")
df_grouped


df_obs <- data.frame("coordinates"=1:17, "Dim1"=c(-0.4517651, 0.6635215,-0.2671567,-1.068668,0.6184873,0.6112383,0.6112383,-0.6211474,1.1252766,-0.2744057,-1.068668,-0.6211474,-0.2744057,-0.6211474,0.3367602,0.6112383,0.6907508))
df_ratings<- data.frame("session"= c(1:3), "rating"= c(-3, 0, +3))
group(data=df_obs, n=c(1, 2, 3), method='l_starts', starts_col='coordinates', col_name = 'session')


group(
  data = df_observations,
  n = dim1,  # Starting values
  method = 'l_starts',
  starts_col = 'participant',
  col_name = 'session'
) %>%
  kable()

df_observations <- group(
  data = dfdim1,
  n = 'auto',
  method = 'l_starts',
  starts_col = 'participant',
  col_name = 'session'
) 

df_observations %>% 
  kable()

eig.val<-get_eigenvalue(res.mca)
head(eig.val)
fviz_screeplot(res.mca, addlabels=T, ylim=c(0, 45)) #histograma das dimensões
fviz_mca_biplot(res.mca, repel = T, ggtheme=theme_minimal())
var<-get_mca_var(res.mca)
var[["coord"]]
var[["cos2"]]
var[["contrib"]]
fviz_mca_var(res.mca, col.var="cos2", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel=T, ggtheme=theme_minimal()) #variáveis dim1 e dim2 com gradiente cos2 (cosseno ao quadrado)

fviz_mca_var(res.mca, col.var = "contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, ggtheme=theme_minimal()) #variáveis dim1 e dim2 com gradiente de contribuição
fviz_mca_var(res.mca, col.var = "coord", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, ggtheme=theme_minimal()) #variáveis dim1 e dim2 com gradiente das coordenadas
