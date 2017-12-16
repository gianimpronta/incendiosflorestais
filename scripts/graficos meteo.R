


g <- ggplot(meteo, aes(Longitude, Latitude))
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)
g <- ggplot(meteo[is.na(meteo$PressaoAtmEstacao),], aes(Longitude, Latitude))
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)

qplot(meteo[is.na(meteo$TempBulboUmido),]$Data)
qplot(meteo[!is.na(meteo$PressaoAtmEstacao),]$Data)

plot(x= meteo[!is.na(meteo$PressaoAtmEstacao),]$PressaoAtmEstacao, type='h')
abline(v=mean(meteo[!is.na(meteo$PressaoAtmEstacao),]$PressaoAtmEstacao), col='red')



amostra_focos <- focos[sample(nrow(focos), 100000),]
amostra_meteo <- meteo[sample(nrow(meteo), 100000),]
amostra_meteo %>% count(PressaoAtmEstacao) %>% plot(type = "h", ylim=c(0, 250))
abline(v = mean(amostra_meteo$PressaoAtmEstacao, na.rm=TRUE), col=2)
abline(v = median(amostra_meteo$PressaoAtmEstacao, na.rm=TRUE), col=3)
abline(v = Mode(amostra_meteo$PressaoAtmEstacao), col=4)

g <- ggplot(amostra_meteo, aes(Longitude, Latitude))
g + borders("worldHires", "Brazil") + geom_point(col="red", pch=20)