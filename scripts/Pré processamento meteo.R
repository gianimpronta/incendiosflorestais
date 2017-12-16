library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(Rcmdr)
library(mice)
library(reshape2)
library(ff)
library(sm)

meteo <- read_delim("~/Dropbox/Data Science/Datasets/Incendios florestais BR/estacoes_met/ meteo_data.csv", 
                         ";", quote = "\\\"", escape_double = FALSE, 
                         col_types = cols(Data = col_date(format = "%d/%m/%Y")), trim_ws = TRUE)
summary(meteo)

# As colunas PressaoAtmEstacao e Estacao estão com o tipo errado
meteo$Estacao <- as.factor(meteo$Estacao)
meteo$PressaoAtmEstacao <- as.numeric(meteo$PressaoAtmEstacao)

# Criando duas colunas "Mes" e "Ano"
meteo <- meteo %>%
      mutate(mes = as.factor(format(meteo$Data, "%m")), 
             ano = as.factor(format(meteo$Data, "%Y")))

# Verificando as observações faltantes
sapply(meteo, function(x)(sum(is.na(x))))


# Checando a frequencia das estacoes por ano
freq_table <- xtabs(~ano+Estacao, data=meteo) %>% 
      t(.)
freq_table <- reshape(as.data.frame(freq_table), timevar = "ano", idvar = "Estacao", direction = "wide")
freq_table[which(freq_table == 0, arr.ind = TRUE)[,1],]

# Existem estacoes que não tem dados para todos os anos, o que não pode ser reparado, 
# Vamos extrair essas estacoes do dataframe
retira <- freq_table[which(freq_table == 0, arr.ind = TRUE)[,1],]$Estacao
meteo2 <- meteo[meteo$Estacao != retira,]

# Mostrando a distribuição de Valores faltantes por ano
aggregate(. ~ ano, data=meteo2, function(x) {sum(is.na(x))}, na.action = NULL) %>%
      melt() %>% 
      filter (value > 0) %>%
      ggplot(data = . , aes(variable, value, fill = variable)) + 
      geom_bar(stat = "identity") + facet_grid(ano ~ .)

# Mostrando a distribuição de valores faltantes por estacao
aggregate(. ~ Estacao, data=meteo2, function(x) {ic(x)}, na.action = NULL)
#melt() %>% 
#filter (value > 0) %>%
#ggplot(data = . , aes(variable, value, fill = variable)) + 
#geom_bar(stat = "identity") + facet_grid(Estacao ~ .)

# Utilizando pacote mice para imputar valores faltantes
ini <- meteo2 %>%
      select(c("TempBulboSeco", "TempBulboUmido", "PressaoAtmEstacao", 
               "VelocidadeVento", "UmidadeRelativa", "Nebulosidade")) %>%
      mice(data = ., maxit = 0, seed = 1869)

imp_merged <- mice.mids(ini, maxit = 1)

meteo3 <- complete(imp_merged)

# Comparando a distribuição das variáveis antes e depois da imputação. 

plot.multi.dens <- function(s)
{
      junk.x = NULL
      junk.y = NULL
      for(i in 1:length(s)) {
            junk.x = c(junk.x, density(s[[i]])$x)
            junk.y = c(junk.y, density(s[[i]])$y)
      }
      xr <- range(junk.x)
      yr <- range(junk.y)
      plot(density(s[[1]]), xlim = xr, ylim = yr, main = "")
      for(i in 1:length(s)) {
            lines(density(s[[i]]), xlim = xr, ylim = yr, col = i)
      }
}

par(mfrow = c(3, 2))
for (i in names(meteo3)){
      plot.multi.dens(list(cc(meteo2[[i]]), meteo3[[i]]))      
}

# Substituindo os valores antigos pelos novos

for ( i in names(meteo3)){
      meteo2[[i]] <- meteo3[[i]]
}


meteo <- meteo2
rm("meteo2", "meteo3", imp_merged, i)



