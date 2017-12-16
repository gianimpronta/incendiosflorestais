library(dplyr)
library(ff)
library(parallel)
library(outliers)
library(ggplot2)
library(mapdata)
library(maps)
library(geosphere)
library(ggmap)
load("./dados/focos.RData")
load("./dados/meteo(tratado).RData")

# Analisando o conjunto de dados "focos" 
summary(focos)

par(mfcol = c(3,1))
for (i in names(focos[6:8])){
  hist(focos[[i]], xlab = i, main = "Sem normalização", probability = TRUE)
  lines(density(focos[[i]]), col="red")
}

# Analisando o conjunto de dados "meteo"
par(mfcol = c(6,2))
for (j in names(meteo[4:9])){
      hist(meteo[[j]], xlab = j, main = "Sem normalização", probability = TRUE)
      lines(density(meteo[[j]]), col="red")
}

# Normalizando o conjunto de dados focos
colunas <- c("DiasSemChuva", "Precipitacao", "RiscoFogo")
focos_norm <- focos %>%
  select(colunas) %>%
  sapply(.,scale)

# Plot comparativo da normalização
par(mfrow = c(3,1))
for (i in dimnames(focos_norm)[[2]]){
  hist(focos_norm[,i], xlab = i, main = "Normalização Z-Score", probability = TRUE)
  lines(density(focos_norm[,i]), col="red")
}

# Juntando os dados normalizados com o DF original
colunas <- c("DiasSemChuva_norm", "Precipitacao_norm", "RiscoFogo_norm")
focos_norm <- as.data.frame(focos_norm)
names(focos_norm) <- colunas
focos <- cbind(focos, focos_norm)

# Normalizando DF meteo
colunas <- names(meteo)[4:9]
meteo_norm <- meteo %>%
  select(colunas) %>%
  sapply(.,scale)

# Plot comparativo
par(mfrow = c(3,2))
for (i in dimnames(meteo_norm)[[2]]){
  hist(meteo_norm[,i], xlab = i, main = "Normalização Z-Score", probability = TRUE)
  lines(density(meteo_norm[,i]), col="red")
}

# Juntando os dados normalizados com o DF original
colunas <- paste0(colunas, "_norm")
meteo_norm <- as.data.frame(meteo_norm)
names(meteo_norm) <- colunas
meteo <- cbind(meteo, meteo_norm)

# Análise Exploratória
p1 <- qplot(focos$mes, xlab = "Mês do ano")
# A maioria das queimadas ocorrem entre agosto e novembro
p2 <- qplot(focos$Bioma, xlab = "Bioma")
# A marioria das queimadas ocorrem nos biomas "Amazonia" e "Cerrado"
p3 <- qplot(focos$Latitude, binwidth = 1, xlab = "Latitude")
# pode-se observar que a maioria das queimadas ocorrem em locais entre as 
# latitudes -4 e -15, inclusive
p4 <- qplot(focos$Longitude, binwidth =1, xlab = "Longitude")
# pode-se observar que a maioria das queimadas ocorrem em locais entre as 
# longitudes -44 e -52, inclusive
multiplot(p1,p2,p3,p4, cols = 2)

# Maior parte das queimadas do Brasil
temp <- focos2 %>%
  select(Latitude, Longitude, Bioma) %>%
  filter(Latitude >= -16, Latitude <= -3, Longitude <= -43, Longitude >= -55)
ggplot(temp, aes(Longitude, Latitude)) + 
  borders("worldHires", "Brazil") + 
  geom_point(aes(color = Bioma))

(nrow(temp)/nrow(focos2))*100


# Distribuição de focos por bioma utlizando waffle
vals <- round(as.vector(table(focos2$Bioma))/sum(as.vector(table(focos2$Bioma))), 3)*250
val_names <- sprintf("%s (%s)",levels(focos2$Bioma), 
                     scales::percent(round(as.vector(table(focos2$Bioma))/sum(as.vector(table(focos2$Bioma))), 3)))
names(vals) <- val_names

waffle::waffle(vals)

# Plotando Heatmap dos Focos
locFocos <- as.data.frame(table(round(focos2[,c("Longitude", "Latitude")], 2)$Longitude, 
                                round(focos2[,c("Longitude", "Latitude")], 2)$Latitude))
names(locFocos) <- c('long', 'lat', 'Frequency')
locFocos$long <- as.numeric(as.character(locFocos$long))
locFocos$lat <- as.numeric(as.character(locFocos$lat))
locFocos <- subset(locFocos, Frequency > 0)
brazil <- get_map(location = 'brazil', zoom = 4)
ggmap(brazil) + geom_tile(data = locFocos, aes(x = long, y = lat, alpha = Frequency),
                          fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())
