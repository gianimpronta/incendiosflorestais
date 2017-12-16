library(dplyr)
library(geosphere)
library(parallel)
library(data.table)

load("./dados/focos_normalizado_final.RData")
load("./dados/meteo_normalizado_final.RData")

juncao <- function(df_focos, df_meteo=df_meteo, dt_estacoes = NULL){
      if (is.null(dt_estacoes)){
            dt_estacoes <- cria_estacoes(df_meteo)
      }     
      dt_meteo <- setDT(df_meteo)
      dt_focos <- setDT(df_focos)
      dt_focos <- df_focos[,DataHora := as.IDate(DataHora)]
      for (i in 1:nrow(dt_meteo)){
            #para cada obs_meteo calcular se tem um foco dentro do raio da 
            #estacão no dia
            obs_meteo <- dt_meteo[i]
            fdia <- focos_no_dia(obs_meteo, dt_focos)
            if (nrow(fdia) > 0){ 
                  n_focos <- checa_dist(obs_meteo, fdia, dt_estacoes)
            } else {
                  n_focos <- 0
            }
            dt_meteo[i, "n_focos"] <- n_focos 
      }
return(dt_meteo)

}
cria_estacoes <- function(df_meteo){
      dt_estacoes <- setDT(df_meteo[,c("Estacao", "Longitude", "Latitude")])
      dt_estacoes <- unique(dt_estacoes)
      for (i in 1:nrow(dt_estacoes)){
            dt_estacoes[i,"Raio"] <- cria_raio_estacao(dt_estacoes[i,2:3, 
                                                                  with = FALSE],
                                                       dt_estacoes)
      }
      dt_estacoes <- dt_estacoes[,lapply(.SD, mean), by = .(Estacao)]
      setkey(dt_estacoes, Estacao)
      return(dt_estacoes)
}
cria_raio_estacao <- function(ponto, dt_estacoes){
      matriz <- setattr(as.data.frame(distm(ponto, as.matrix(dt_estacoes[,2:3, 
                                                                with = FALSE])), 
                                      stringsAsFactors=FALSE),
                        "class", "data.table")
      matriz <- matriz[,matriz > 100, with = FALSE]
      idx <- which.min(matriz)
      return(as.numeric(matriz[,..idx])/2)
}
focos_no_dia <- function(obs_meteo, dt_focos){
      dia <- obs_meteo[,Data]
      return(dt_focos[DataHora == as.IDate(format(dia, "%Y-%m-%d"))])
}
checa_dist <- function(obs_meteo, dt_focos, dt_estacoes){
      # extrai número estacao
      estacao <- obs_meteo[,Estacao]
      # checa em estacoes pelas coordenadas da estacao
      coord_estacao <- dt_estacoes[Estacao == estacao, 
                                   c("Longitude", "Latitude"), 
                                   with= FALSE]
      # checa em estacoes pelo raio da estacao
      raio_estacao <- dt_estacoes[Estacao == estacao, Raio]
      # calcula distancias entre todos os focos e a estacao
      matriz <- setattr(
            as.data.frame(distm(coord_estacao, 
                                as.matrix(dt_focos[,c("Longitude", "Latitude"), 
                                                   with = FALSE])), 
                                      stringsAsFactors=FALSE), "class", 
                        "data.table")
      # verifica se existem focos dentro do raio
      n_focos <- sum(matriz <= raio_estacao)
      # caso positivo retorna o número de focos
      return(n_focos)
}

dt_estacoes <- cria_estacoes(df_meteo)
dt_final <- juncao(df_focos, meteo, dt_estacoes)

dt_final[, c("mes", "ano") := list(format(Data, "%m"), 
                                   format(Data, "%Y"))]

