#Script - Projeto Final - Biologia Computacional e Sistemas

# Carregamento dos dados pré-processados, os mesmos que foram utilizados nas atividades EaD1 e EaD2 e mesclados em um arquivo RData
load(file = "./merge_data12_df.RData")

#Visualização das variáveis que estão disponíveis para validação após a mescla dos dois arquivos utilizados nas atividades EaD1 e EaD2
names(merge_data12_df)

#Filtragem feita para analisar os países com OECD
OECD_extended <- c("AUS","AUT","BEL","CAN","CHE","DEU","DNK","ESP","FIN","FRA",
                   "GBR","GRC","ISL","ITA","JPN","KOR","LUX","MEX","NLD",
                   "NOR","NZL","PRT","SWE","TUR","USA","BRA","CHL","ARG","COL",
                   "CRI","BGR","RUS","PER","ROU","CZE","EST","IRL","ISR","LVA",
                   "LTU","POL","SVK","SVN")
merge_data12_df <- merge_data12_df[merge_data12_df$ISO3%in%OECD_extended, ]

#Filtragem feita com a data do registro como parâmetro
merge_data12_df[,c("COUNTRY","DATE_UPDATED")]

#Verificação de consistência entre pessoas totalmente vacinadas e casos registrados nos últimos 7 dias
plot(x=merge_data12_df$PERSONS_FULLY_VACCINATED,
     y=merge_data12_df$Cases...newly.reported.in.last.7.days,
     xlab = "Pessoas totalmente vacinadas",
     ylab = "Casos registrados nos ultimos 7 dias")

#Carregamento das bibliotecas para fazer a visualização dos dados em grafo
library(RedeR)
library(TreeAndLeaf)
library(RColorBrewer)
library(igraph)

#Separação das duas variáveis escolhidas para fazer a relação dos dados de vacinação e contágio por país: pessoas totalmente vacinadas e casos registrados nos últimos 7 dias
x_df <- merge_data12_df[,c("PERSONS_FULLY_VACCINATED",
                           "Cases...newly.reported.in.last.7.days")]

#Comando feito para simplificar os nomes das colunas no grafo
colnames(x_df) <- c("Totalmente vacinados","Casos ultimos 7 dias")

#Agrupamento dos países (cluster de registros)
hc <- hclust(dist(x_df), "ward.D2")
plot(hc, cex=0.3)

#Transformação do cluster gerado em um objeto TreeAndLeaf
tal <- treeAndLeaf(hc)

#Mapeamento para criar o objeto TreeAndLeaf no modelo de um dataset Íris
tal <- att.mapv(g = tal, dat = iris, refcol = 0)

#Mapeamento dos dados selecionados no objeto TreeAndLeaf
cols <- c("#80b1d3","#fb8072","#8dd3c7")
tal <- att.mapv(g = tal, dat = x_df, refcol = 0)
pal <- rev(brewer.pal(7, "RdYlGn"))
tal <- att.setv(g = tal, from = "Totalmente vacinados",
                to = "nodeColor", cols = pal, breaks=seq(-2,2,0.4), nquant = 7)
tal <- att.setv(g = tal, from = "Casos ultimos 7 dias",
                to = "nodeSize", xlim = c(50, 200, 5), nquant = 7)

#Ajuste da fonte da variável "tal"
V(tal)$nodeFontSize[V(tal)$isLeaf] <- 40

#Envio para a RedeR
addGraph(rdp, tal, gzoom=40)
addLegend.color(rdp, tal, title="Totalmente vacinados")
addLegend.size(rdp, tal, title="Casos ultimos 7 dias", position="bottomleft")

#Carregamento da visualização do grafo
rdp <- RedPort()
calld(rdp)

#Resetar grafo
resetd(rdp)
