con <- DBI::dbConnect(RClickhouse::clickhouse()
res <- DBI::dbGetQuery(con, "SELECT * FROM 50916kekw.iris")
res$Class<-factor(c("Iris-setosa", "Iris-virginica", "Iris-versicolor"))
print(res)
sapply(res, class)
levels(res$datafive)
summary(res)
#Пакет для работы с алгоритмами машинного обучения
install.packages("caret")
library(caret)
#Загрузим данныем.
data(iris)
# Загрузим встроенный датасет и сохраним ссылку на него в переменную
dataset <- iris
summary(dataset)
#Измерения
dim(dataset)
#список типов каждого атрибута
sapply(dataset, class)
#взглянем на данные
head(dataset)
#просмотрим уровни классификатора
levels(dataset$Species)
#Исследуем и визуализируем данные. Распределяем виды Ирисов в данном наборе
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage = percentage)
#Разбиваем данные на переменные и отклик
x <- dataset[,1:4]
y <- dataset[,5]
#Выборки будем визуализировать диаграммой размаха
par(mfrow=c(1,4))
for(i in 1:4) {
boxplot(x[,i], main = names(iris)[i])
}
                      
install.packages("ellipse")
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")
install.packages('e1071', dependencies=TRUE)
#Запускаем все алгоритмы и проверяем кроссвалидацию через 10 блоков
control <- trainControl(method="cv", number=10)
#Контролируем метрику
metric <- "Accuracy"
#Линейные алгоритмы
set.seed(13)
fit.lda <- train(Species~., data = dataset, method = "lda", metric=metric, trControl=control)
#нелинейные алгоритмы
set.seed(13)
fit.cart <- train(Species~., data = dataset, method = "rpart", metric=metric, trControl=control)
set.seed(13)
fit.knn <- train(Species~., data = dataset, method = "knn", metric=metric, trControl=control)
#Сложные алгоритмы
set.seed(13)
fit.svm <- train(Species~., data = dataset, method = "svmRadial", metric=metric, trControl=control)
set.seed(13)
fit.rf <- train(Species~., data = dataset, method = "rf", metric=metric, trControl=control)
#Получим оценки конролируемой метрики для каждого алгоритма                      
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)
dotplot(results)
                      
print(fit.lda)
#Отрежем часть данных для последующей валидации, получим 80% данных                      
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
#Выберем 20% для валидации
validation <- dataset[-validation_index,]
#80% у нас будут исходные данные
dataset <- dataset[validation_index,]
                      
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
