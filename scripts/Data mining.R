library(C50)
library(pROC)
library(rpart)
library(data.table)
library(randomForest)
library(ggplot2)
library(rattle)
library(caret)
library(caretEnsemble)
library(parallel)
library(doParallel)

##sampling
modNames <- unique(modelLookup()[modelLookup()$forClass,c(1)])

# Ensemble method
cluster <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cluster)
algorithmList <- c("vglmAdjCat", "lda", "mda", 'qda', "J48", "PART", "treebag",
                   "rda", "nnet", "fda", "svmRadialCost", "gbm", "C5.0", "C5.0Cost", 
                   "knn", "naive_bayes", "rpart", "parRF")
algorithmList2 <- c("vglmAdjCat", "lda", "mda", 'qda', "J48", "PART", "treebag",
                   "rda", "nnet", "fda", "gbm", 
                   "knn", "naive_bayes", "rpart")
algorithmList3 <- c("treebag", "fda", "gbm", "naive_bayes")
idx <- createDataPartition(dt_final$incendio, p = .75, list = FALSE)
treino <- dt_final[idx,]
teste <- dt_final[-idx,]
control <- trainControl(method="repeatedcv",
                        number = 10, repeats = 3,
                        savePredictions="final", classProbs = T,
                        verboseIter = T)
form <- as.formula("incendio ~ Data+TempBulboSeco+TempBulboUmido+
                   UmidadeRelativa+PressaoAtmEstacao+VelocidadeVento+
                   Nebulosidade+Latitude+Longitude")
treino_menor <- treino[sample(nrow(treino), 5000),]
set.seed(1869)
metric <- "Accuracy"
tuning <- expand.grid(model = "rules",
                      winnow = FALSE,
                      cost = 1:5, trials=c(1:5))
models <- caretList(form, data = treino_menor,
                    trControl = control, 
                    methodList = algorithmList3, 
                    metric = metric, 
                    continue_on_fail = F)
results <- resamples(models)
summary(results)
stopCluster(cluster)
registerDoSEQ()
gc()
ensemble <- caretEnsemble(models, 
                          metric = metric, 
                          trControl = control)

idx2 <- sample(nrow(teste), nrow(treino_menor)*0.25)
teste_menor <- teste[idx2,]
results2 <- as.data.frame(predict(models, teste_menor))
for (i in names(results2)){
      mat <- confusionMatrix(reference = teste_menor$incendio, 
                            data = results2[,i],
                            positive = "sim")
      if (mat$overall['Accuracy'] > 0.8){
            if(mat$byClass['Sensitivity'] > 0.3){
                  print(paste0(i," -> accu/seg: ", mat$overall['Accuracy']/(results$timings[which(rownames(results$timings) ==  i),])))
            }
      }
}


# Um algoritmo só
cluster <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cluster)
idx <- createDataPartition(dt_final$incendio, p = .75, list = FALSE)
treino <- dt_final[idx,]
teste <- dt_final[-idx,]
control <- trainControl(method="cv",
                        number = 10,
                        savePredictions="final", classProbs = T,
                        verboseIter = T)
form <- as.formula("incendio ~ Data+TempBulboSeco+TempBulboUmido+
                         UmidadeRelativa+PressaoAtmEstacao+VelocidadeVento+
                         Nebulosidade+Latitude+Longitude+mes")
treino_menor <- treino[sample(nrow(treino), 1000),]
set.seed(1869)
metric <- "Accuracy"
tuning <-  expand.grid(interaction.depth = c(3, 6, 9),
                          n.trees = c(100, 250), 
                          shrinkage = .1,
                          n.minobsinnode = c(10, 20, 30))
fit.gbm <- train(form, data = treino,
                 trControl = control, method = "gbm", 
                 metric = metric, 
                 tuneGrid = tuning)
stopCluster(cluster)
registerDoSEQ()
gc()
save(fit.gbm, file = "./dados/fit.gbm.rds")
pred <- predict(fit.gbm, teste)
mat_conf <- confusionMatrix(pred, reference = teste$incendio, 
                             positive = "sim")

mat_conf
plot(fit.gbm)
summary(fit.gbm)
