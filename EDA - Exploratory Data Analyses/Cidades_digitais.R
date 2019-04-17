# Análise das obras do governo sobre cidades digitais
# Silvio Cesar Lima
# Fonte:
# https://www.mctic.gov.br/mctic/opencms/indicadores/detalhe/Cidades-Digitais-Lista-de-Cidades-Atendidas-2.html

# Carregando os pacotes
library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(leaflet)
library(gmodels)


# Carregando as bases de dados
df <- read.csv("cidades_digitais.csv", sep=',',stringsAsFactors = FALSE)
regioes<-read.csv("REGIAO.csv",stringsAsFactors = FALSE)

LatLong<-read.csv("LatLong.csv",sep=',',stringsAsFactors = FALSE)

# Unindo as bases a partir de campos comuns.
df_1 <- merge(df, LatLong, by=c("CIDADE"))
df <- merge(df_1, regioes, by=c("UF"))

# Removendo coluna desnecessária
colnames(df)
df$IBGE<-NULL

# Resumo 
str(df)

# # QUAIS OS 10 MAIORES VALORES PREVISTOS DE SEREM INVESTIDOS ?
#
# Ordenação em ordem decrescente
df_sort <- df[order(df$VALOR_TOTAL_PREVISTO,decreasing = TRUE),]
df_sort%>%select('UF','CIDADE','VALOR_TOTAL_PREVISTO')%>%head(10)
#
# Valores previstos por faixa
valor_previsto_obra=df$VALOR_TOTAL_PREVISTO
hist(valor_previsto_obra)

# Detalhes do histograma gerado
histinfo<-hist(valor_previsto_obra)
histinfo
#
# Apresenta a frequencia de valores em determinada faixa.
# Nesse caso, há 123 valores previstos na faixa entre 400 e 600 mil.
histinfo$counts
#
##  QUAL CIDADE RECEBERÁ O MAIOR VALOR PREVISTO ?
#
df_max_valor_previsto<- filter(df,VALOR_TOTAL_PREVISTO == max(valor_previsto_obra))
df_max_valor_previsto
#
## QUANTAS CIDADES ESTÃO PREVISTAS POR REGIÃO ?
#
df_Reg_cidades<- df%>%group_by(REGIAO)%>%tally()
df_n<-as.data.frame(df_Reg_cidades)
setnames(df_n, "n", "Total_cidades")
df_Reg_cidades<-df_n[order(df_n$Total_cidades,decreasing=TRUE),]
df_Reg_cidades
#
## QUANTAS CIDADES ESTÃO PREVISTAS POR UF ?
#
df_UF_cidades<- df%>%group_by(UF)%>%tally()
df_n<-as.data.frame(df_UF_cidades)
setnames(df_n, "n", "Total_cidades")
df_UF_cidades<-df_n[order(df_n$Total_cidades,decreasing=TRUE),]
df_UF_cidades
#
## QUAL O NÚMERO DE CIDADES POR STATUS DAS OBRAS ?
#
table(df$STATUS)
#
# Total de obras por status e região 
CrossTable(df$REGIAO, df$STATUS)
#
# Calculando a proporção de distribuição do status  das obras entre as cidades
# Valores arrendondados e na forma de porcentagem
status_table<-table(df$STATUS)
status_table <- prop.table(status_table) * 100 # valores em porcentagem
status_table<-round(status_table, digits = 1)
status_table
#
# Ajustar nomes de colunas
df_status0=data.frame(status_table)
df_status1<-setnames(df_status0,"Var1","Status")
df_status<-setnames(df_status1,"Freq","Percentual")
df_status_sort <- df_status[order(df_status$Percentual,decreasing = TRUE),]
#
## QUAL O PERCENTUAL DE STATUS DAS OBRAS
#
# Ordenando os valores de percentual a partir do maior
# Coluna Status segue a ordenação pelo campo numerico Percentual em ordem decrescente
df_status_sort$Status <- factor(df_status_sort$Status, levels = unique(df_status_sort$Status)[order(df_status_sort$Percentual, decreasing = TRUE)])

# Plotando a informação gerada
plot_ly(df_status_sort,type='bar',x=~Status,y=~Percentual)%>%layout(title = "Status e Percentual das obras",xaxis = list(title = "Status"),yaxis = list(title = "Percentual (%)"))
#
## QUAL O NÚMERO DE CIDADES POR REGIÃO ?
#
# Total de cidades por regiao
df_reg_cid<- df%>% select("CIDADE","REGIAO")%>%
  group_by(REGIAO)%>%
  summarise("Total" = n())
head(df_reg_cid)
#
# Ordenando os totais de cidades por região
df_reg_cid_sort <- df_reg_cid[order(df_reg_cid$Total,decreasing = TRUE),]
head(df_reg_cid_sort)

# 
# Coluna Regiao segue a ordenação pelo campo numerico Total
df_reg_cid_sort$REGIAO <- factor(df_reg_cid_sort$REGIAO, levels = unique(df_reg_cid_sort$REGIAO)[order(df_reg_cid_sort$Total, decreasing = TRUE)])

# Plotando a informação gerada
plot_ly(df_reg_cid_sort,type='bar',x=~REGIAO,y=~Total)%>%layout(title = "Total cidades por regiao",xaxis = list(title = "Regiao"),yaxis = list(title = "Total"))
#
#
## QUAIS OS VALORES DE INVESTIMENTOS POR REGIÃO ?
#
# Agrupando por regiao os valores previstos
df_regiao_inv <- df %>% select(REGIAO,VALOR_TOTAL_PREVISTO)%>%
  group_by(REGIAO)%>%
  summarise(Total=sum(VALOR_TOTAL_PREVISTO))
#
# Ordenar os valores
df_regiao_inv_sort<-df_regiao_inv[order(df_regiao_inv$Total,decreasing = TRUE),]

# Coluna Regiao segue a ordenação pelo campo numerico Total em ordem decrescente
df_regiao_inv_sort$REGIAO <- factor(df_regiao_inv_sort$REGIAO, levels = unique(df_regiao_inv_sort$REGIAO)[order(df_regiao_inv_sort$Total, decreasing = TRUE)])

# Plotando a informação gerada
plot_ly(df_regiao_inv_sort,type='bar',x=~REGIAO,y=~Total)%>%layout(title = "Total previsto de investimento por regiao",xaxis = list(title = "Regiao"),yaxis = list(title = "Total (R$)"))

## BIBLIOTECA LEAFLET
#
# Mapa do Brasil e os status das obras nas cidades

# Passos necessários para gerar um mapa customizado com leaflet
# Vectorized SWITCH:
# Cada valor do status recebe um valor de 0 a 2
sit <- Vectorize(function(a) {
  switch(as.character(a),
         "Sem Previsao" = 0,
         "Em andamento" = 1,
         "Concluida" = 2
         )
}, "a")

# A função sit é aplicada ao coluna STATUS no dataframe
df$stat=sapply(df$STATUS,sit)

# Função que define a cor do icone de acordo com o valor do STATUS
getColor <- function(df) {
  sapply(df$stat, function(stat) {
    if(stat == 1) {
      "green"
    } 
    else if(stat == 0) {
      "red"
    } 
    else if(stat == 2){
      "blue"
    } })
}

# Customização do icone
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df)
)
# Mapa default
#map3 = leaflet(df) %>% addTiles()%>%
#  addMarkers(~long, ~lat,popup = (df$STATUS),label=df$CIDADE)
#map3

# Mapa customizado apresentando cores de acordo com status
map4 = leaflet(df) %>% 
  addTiles()%>%
  addAwesomeMarkers(~LONG, ~LAT, icon=icons,popup = (df$STATUS),label=df$CIDADE)

# Adicionando uma legenda para as cores dos icones
map4%>%
  addLegend(
  position='topright',
  colors= c("green", "red", "blue"),
  labels= c("Em andamento","Sem Previsao","Concluida"),
  opacity = 0.75,
  title="Legenda"
)


