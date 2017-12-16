library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(Rcmdr)
library(mice)


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
# Preenchendo essas linhas através de imputação multivariada por equações emm cascata

ini <- focos %>%
      select(Latitude, Longitude, Bioma) %>%
      mice(data = ., maxit = 0, seed = 1869)
imp <- mice.mids(ini, maxit = 1) 
focos2 <- complete(imp)

ggplot(focos2[is.na(focos$Bioma),], aes(Longitude, Latitude, color= Bioma)) + 
      geom_point() + borders("worldHires", "Brazil")

focos$Bioma <- focos2$Bioma

# Criando duas colunas "Mes" e "ano"
focos <- focos %>% 
      mutate(mes = as.factor(format(DataHora, "%m")), 
             ano = as.factor(format(DataHora, "%Y")))

# Removendo valores inválidos para "RiscoFogo", oiu seja, valores acima de 1
focos <- focos %>%
      filter(RiscoFogo <= 1) 
      
