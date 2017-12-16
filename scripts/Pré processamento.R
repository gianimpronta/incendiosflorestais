library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)


# Os dados de focos de inc?ndio foram obtidos em arquivos CSV divididos em s?rie temporais que v?o do dia 1? de janeiro
# at? o dia 31 de dezembro de cada ano, ent?o, faz-se necess?rio efetuar a integra??o dos dados em um ?nico Dataframe

Focos_2010_01_01_2010_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2010-01-01.2010-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
Focos_2011_01_01_2011_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2011-01-01.2011-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
Focos_2012_01_01_2012_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2012-01-01.2012-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
Focos_2013_01_01_2013_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2013-01-01.2013-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
Focos_2014_01_01_2014_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2014-01-01.2014-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
Focos_2015_01_01_2015_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2015-01-01.2015-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
Focos_2016_01_01_2016_12_31 <- read_csv2("~/Dropbox/Data Science/Datasets/Incendios florestais BR/Focos/Focos.2016-01-01.2016-12-31.csv", col_types = cols(DataHora = col_datetime(format = "%Y/%m/%d %H:%M:%S")), trim_ws = TRUE)
df = list(Focos_2010_01_01_2010_12_31, Focos_2011_01_01_2011_12_31, Focos_2012_01_01_2012_12_31, 
          Focos_2013_01_01_2013_12_31, Focos_2014_01_01_2014_12_31, Focos_2015_01_01_2015_12_31, 
          Focos_2016_01_01_2016_12_31)


head(Focos_2011_01_01_2011_12_31)
dim(Focos_2011_01_01_2011_12_31)
str(Focos_2011_01_01_2011_12_31)

# Presen?a de NA's nas colunas FRP e AreaIndu, vamos checar quantos s?o.

Focos_2011_01_01_2011_12_31[!is.na(Focos_2011_01_01_2011_12_31$AreaIndu),]
Focos_2011_01_01_2011_12_31[!is.na(Focos_2011_01_01_2011_12_31$FRP),]

# Coluna AreaIndu e coluna FRP possui NA em todos as linhas do Dataframe Focos_2011_01_01_2011_12_31.
# Vamos testar os outros df's

for (i in df){
  print(paste("AreaInd:", nrow(i[!is.na(i$AreaIndu),])))
  print(paste("FRP:",nrow(i[!is.na(i$FRP),])))
}

# Todos os Dataframes possuem a coluna AreaIndu e a coluna FRP n?o preenchidas.
# Podemos excluir tais colunas.

for (i in 1:7){
  df[[i]]$AreaIndu = NULL
  df[[i]]$FRP = NULL
}

# Checar se as colunas foram exclu?das.

for (i in df){ print(names(i))}

# Integrando todas os dataframes

focos <- rbind(df[[1]], df[[2]], df[[3]], df[[4]], df[[5]], df[[6]], df[[7]])
rm(Focos_2010_01_01_2010_12_31, Focos_2011_01_01_2011_12_31, Focos_2012_01_01_2012_12_31, Focos_2013_01_01_2013_12_31, Focos_2014_01_01_2014_12_31, Focos_2015_01_01_2015_12_31, Focos_2016_01_01_2016_12_31)
rm(df, i)
# Corrigindo nomes das colunas

colunas <- names(focos)
colunas[5] <- "Municipio"
colunas[7] <- "DiasSemChuva"
colunas[8] <- "Precipitacao"
colunas[9] <- "RiscoFogo"
colunas[11] <- "Longitude"
names(focos) <- colunas

# Transformando algumas colunas em Factors e retirando a coluna Pais, j? que s? existem 
# dados do Brasil no dataset
focos$Satelite = as.factor(focos$Satelite)
focos$Pais = NULL
focos$Estado = as.factor(focos$Estado)
focos$Municipio = as.factor(focos$Municipio)
focos$Bioma = as.factor(focos$Bioma)

# Ainda existem registro faltando no Dataframe?
sum(is.na(focos))
for (i in 1:10){ print(paste(names(focos)[i],":", sum(is.na(focos[,i]))))}

# Existem 38 registros que possuem a vari?vel "Bioma" vazia.
# Removendo essas linhas em branco
focos <- focos[!is.na(focos$Bioma),]


focos <- focos %>% mutate(mes = as.factor(format(DataHora, "%m")), ano = as.factor(format(DataHora, "%Y")))
focos %>% select(mes, ano) %>% group_by(mes, ano) %>% qplot(mes, facets = ano ~ .)

summary(focos)


qplot(focos$Precipitacao)
qplot(focos$RiscoFogo) 
qplot(focos$DiasSemChuva)
qplot(focos$Latitude) 
qplot(focos$Longitude)
qplot(focos$Satelite)
focos %>% filter(ano == "2016") %>% qplot(DataHora, data = .)
qplot(y= Latitude, x= Longitude, data = focos)

g <- ggplot(focos, aes(Longitude, Latitude))
g + borders("worldHires", "Brazil")
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)

meteo_data <- read_delim("~/Dropbox/Data Science/Datasets/Incendios florestais BR/estacoes_met/ meteo_data.csv", 
                  ";", quote = "\\\"", escape_double = FALSE, 
                  col_types = cols(Data = col_date(format = "%d/%m/%Y")), 
                  locale = locale(time_format = "%H%M", tz = "America/Sao_Paulo"), 
                  na = "", trim_ws = TRUE)

sum(is.na(meteo_data))
for (i in 1:11){ print(paste(names(meteo_data)[i],":", mean(is.na(meteo_data[,i]))))}


g <- ggplot(meteo_data, aes(Longitude, Latitude))
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)
g <- ggplot(meteo_data[is.na(meteo_data$PressaoAtmEstacao),], aes(Longitude, Latitude))
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)

qplot(meteo_data[is.na(meteo_data$TempBulboUmido),]$Data)
qplot(meteo_data[!is.na(meteo_data$PressaoAtmEstacao),]$Data)

plot(x= meteo_data[!is.na(meteo_data$PressaoAtmEstacao),]$PressaoAtmEstacao, type='h')
abline(v=mean(meteo_data[!is.na(meteo_data$PressaoAtmEstacao),]$PressaoAtmEstacao), col='red')

Mode <- function(x) {
      if (is.numeric(x)) {
            x_table <- table(x)
            return(as.numeric(names(x_table)[which.max(x_table)]))
      }
}

amostra_focos <- focos[sample(nrow(focos), 100000),]
amostra_meteo <- meteo_data[sample(nrow(meteo_data), 100000),]
amostra_meteo %>% count(PressaoAtmEstacao) %>% plot(type = "h", ylim=c(0, 250))
abline(v = mean(amostra_meteo$PressaoAtmEstacao, na.rm=TRUE), col=2)
abline(v = median(amostra_meteo$PressaoAtmEstacao, na.rm=TRUE), col=3)
abline(v = Mode(amostra_meteo$PressaoAtmEstacao), col=4)

g <- ggplot(amostra_meteo, aes(Longitude, Latitude))
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)

