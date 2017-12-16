library(dplyr)
library(geosphere)
library(parallel)
library(data.table)

# Juntando os Dataframes
juncao <- function(df_focos, df_meteo=meteo2){
      estacoes <- setDT(df_meteo[,c("Estacao", "Longitude", "Latitude")])
      setkey (estacoes, Estacao)
      estacoes <- as.data.frame(unique(estacoes))
      df_meteo <- setDT(df_meteo)
      novo_df <- data.frame(matrix(NA, nrow = 0, ncol = 6))
      names(novo_df) <- c("TempBulboSeco", "TempBulboUmido", 
                                     "UmidadeRelativa", "PressaoAtmEstacao", 
                                     "VelocidadeVento", "Nebulosidade")
      for ( i in 1:nrow(df_focos)){
            obs <- df_focos[i,]
            ponto <- c(df_focos[[i,10]], df_focos[[i,9]])
            obs_meteo <- pega_obs(ponto, obs, df_meteo, estacoes)
            novo_df <- rbind(novo_df, as.data.frame(obs_meteo))
      }
      df_focos <- cbind(df_focos, novo_df)
      return(df_focos)
}

estacao_mais_perto <- function(ponto, estacoes=estacoes){
  matriz <- setattr(as.data.frame(distm(ponto, as.matrix(estacoes[2:3])), 
                                  stringsAsFactors=FALSE), "class", 
                    "data.table")
  idx <- which.min(matriz)
  return(estacoes[idx,])
}

achar_data <- function(x){
  return(format(x[["DataHora"]], "%Y-%m-%d"))
}

hora_mais_perto <- function(obs, df_meteo){
  hora <-as.numeric(format(obs[["DataHora"]], "%H%M"))
  x1 <- abs(df_meteo[["Hora"]] - hora)
  idx <- which.min(x1)
  return(as.numeric(df_meteo[idx,"Hora"]))
}

pega_obs <- function(ponto, obs, df_meteo = df_meteo, estacoes = estacoes){
  obs_meteo <- df_meteo[Estacao == estacao_mais_perto(ponto, estacoes)[[1]]]
  obs_meteo <- obs_meteo[Data == achar_data(obs)]
  obs_meteo <- obs_meteo[Hora == hora_mais_perto(obs, obs_meteo)]
  obs_meteo <- as.data.frame(obs_meteo[,-c(1:3,10:13)])
  colunas <- names(df_meteo)[-c(1:3,10:13)]
  if (dim(obs_meteo)[1] == 0){
    obs_meteo <- rbind(obs_meteo,
                       data.frame(matrix(NA, nrow = 1, ncol = 12, 
                                         dimnames = list(NULL,
                                                         colunas))))
  }
  return(obs_meteo)
}      

parte1 <- focos[1:2827955,]
parte2 <- focos[2827956:5655911,]
parte3 <- focos[5655912:8483866,]


teste <- focos[sample(1),]
juncao(teste, meteo)

cl <- makeCluster(3)
clusterSetRNGStream(cl, 1869)
clusterExport(cl, c("parte1", "parte2", "parte3", "meteo"))
clusterExport(cl, c("focos", "meteo"))
clusterExport(cl, c("achar_data", "estacao_mais_perto", "hora_mais_perto", 
                    "juncao", "pega_obs"))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(geosphere))
clusterEvalQ(cl, library(data.table))
system.time(teste <- parLapply(cl, list(focos[1:2827955,], 
                                        focos[2827956:5655911,], 
                                        focos[5655912:8483866,]), 
                               function(x) juncao(x, meteo)))
stopCluster(cl)


