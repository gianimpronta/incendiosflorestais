maxmin_norm <- function(x){
      normalizado <- x
      for (i in 1:ncol(x)){
            maximo <- max(x[i])
            minimo <- min(x[i])
            normalizado[i] <- (x[i]-minimo)/(maximo-minimo)
      }
      return(normalizado)
}