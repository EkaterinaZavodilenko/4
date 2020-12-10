con <- DBI::dbConnect(RClickhouse::clickhouse()
                      res <- DBI::dbGetQuery(con, "SELECT * FROM 50916kekw.iris")
                      res$Class<-factor(c("Iris-setosa", "Iris-virginica", "Iris-versicolor"))
                      print(res)
                      
                      sapply(res, class)
                      levels(res$datafive)
                      summary(res)
                      
                      install.packages("caret")
                      library(caret)
                      data(iris)
                      dataset <- iris
                      summary(dataset)
                      dim(dataset)
                      sapply(dataset, class)
                      head(dataset)
                      levels(dataset$Species)
                      
                      percentage <- prop.table(table(dataset$Species)) * 100
                      cbind(freq=table(dataset$Species), percentage = percentage)
                      
                      x <- dataset[,1:4]
                      y <- dataset[,5]
                      
                      par(mfrow=c(1,4))
                      for(i in 1:4) {
                        boxplot(x[,i], main = names(iris)[i])
                      }
                      
                      install.packages("ellipse")
                      
                      featurePlot(x=x, y=y, plot="ellipse")
                      featurePlot(x=x, y=y, plot="box")
                      
                      install.packages('e1071', dependencies=TRUE)
                      control <- trainControl(method="cv", number=10)
                      metric <- "Accuracy"
                      set.seed(13)
                      fit.lda <- train(Species~., data = dataset, method = "lda", metric=metric, trControl=control)
                      set.seed(13)
                      fit.cart <- train(Species~., data = dataset, method = "rpart", metric=metric, trControl=control)
                      set.seed(13)
                      fit.knn <- train(Species~., data = dataset, method = "knn", metric=metric, trControl=control)
                      set.seed(13)
                      fit.svm <- train(Species~., data = dataset, method = "svmRadial", metric=metric, trControl=control)
                      set.seed(13)
                      fit.rf <- train(Species~., data = dataset, method = "rf", metric=metric, trControl=control)
                      
                      results <- resamples(list(lda=fit.lda, cart=fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
                      summary(results)
                      dotplot(results)
                      
                      print(fit.lda)
                      
                      validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
                      validation <- dataset[-validation_index,]
                      dataset <- dataset[validation_index,]
                      
                      predictions <- predict(fit.lda, validation)
                      confusionMatrix(predictions, validation$Species)