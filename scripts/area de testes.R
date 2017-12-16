i <- 66
df_focos <- amostra
df_meteo <- meteo2



estacoes <- setDT(df_meteo[,c("Estacao", "Longitude", "Latitude")])
setkey (estacoes, Estacao)
estacoes <- as.data.frame(unique(estacoes))
novo_df <- data.frame(matrix(NA, nrow = 0, ncol = 6))
names(novo_df) <- colunas <- c("TempBulboSeco", "TempBulboUmido", 
                               "UmidadeRelativa", "PressaoAtmEstacao", 
                               "VelocidadeVento", "Nebulosidade")


obs <- df_focos[i,]
ponto <- c(df_focos[[i,10]], df_focos[[i,9]])
obs_meteo <- setDT(df_meteo)
obs_meteo <- obs_meteo[Estacao == estacao_mais_perto(ponto, estacoes)[[1]]]
obs_meteo <- obs_meteo[Data == achar_data(obs)]
obs_meteo <- obs_meteo[Hora == hora_mais_perto(obs, obs_meteo)]
obs_meteo <- as.data.frame(obs_meteo[,-c(1:3,10:13)])
if (dim(obs_meteo)[1] == 0){
      obs_meteo <- rbind(obs_meteo,
                         data.frame(matrix(NA, nrow = 1, ncol = 6, 
                                           dimnames = list(NULL,
                                                           names(obs_meteo)))))
}
novo_df <- rbind(novo_df, as.data.frame(obs_meteo))

#funcao hora mais perto
hora_mais_perto(obs, obs_meteo)
hora <-as.numeric(format(obs[["DataHora"]], "%H%M"))
x1 <- abs(obs_meteo[["Hora"]] - hora)
idx <- which.min(x1)
as.numeric(df_meteo[idx,"Hora"])

rm(idx, x1, hora, novo_df, obs_meteo, longlat, obs, df_meteo, df_focos, i,
   estacoes, colunas,  ponto, matriz)




amostra <- focos2[sample(1000),]






DaTa <- function(x) {
      return(unique(setDT(x[,c("Estacao", "Longitude", "Latitude")], 
                          key = Estacao)))
}

normal <- function(x){
      estacoes <- unique(x[,c("Estacao", "Longitude", "Latitude")])
      return(estacoes)
      }


microbenchmark(df1 <- DaTa(meteo2), df2 <- normal(meteo2), times = 10)





# função estacaomaisperto 

matriz <- as.data.table(distm(ponto, as.matrix(estacoes[2:3])))
idx <- which.min(matriz)
estacoes[idx,]

m = distm(ponto, as.matrix(estacoes[2:3]))

microbenchmark("opcao1" = as.data.table(distm(ponto, as.matrix(estacoes[2:3]))),
               "opcao2" = setattr(as.data.frame(distm(ponto, as.matrix(estacoes[2:3])), stringsAsFactors=FALSE), 
               "class", "data.table"),
               times = 100)









rm(idx, x1, hora, novo_df, obs_meteo, longlat, obs, df_meteo, df_focos, i,
   estacoes, colunas,  ponto, matriz)
