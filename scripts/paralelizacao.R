library(parallel)
library(VIM)
# Using all cores can slow down the computer
# significantly, I therefore try to leave one
# core alone in order to be able to do something 
# else during the time the code runs
cores_2_use <- detectCores() - 1

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 1869)
clusterExport(cl, "ini")
clusterEvalQ(cl, library(mice))
imp_pars <- 
      parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
            mice.mids(ini, maxit= 1)
      })
stopCluster(cl)

imp_merged <- imp_pars[[1]]
for (n in 2:length(imp_pars)){
      imp_merged <- 
            ibind(imp_merged,
                  imp_pars[[n]])
}