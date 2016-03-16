source('~/MachineLearning/Assign3/em.R')

wine.train = load.wine_data()
wine.train$quality = as.factor(wine.train$quality)

wine.em.data = as.data.frame(em.wine.class)
wine.em.data= rename(wine.em.data, c("tmp"="feat"))
#wine.em.data$feat = as.factor(wine.em.data$feat)
wine.em.data = cbind(wine.em.data, wine.train$quality)
wine.em.data= rename(wine.em.data, c("wine.train$quality"="quality"))
wine.em.data = wine.em.data[,7:8]
wine.nn.em = nnet(wine.em.data$quality~wine.em.data$feat, data=wine.em.data, size=12, decay=5e-4, maxit=1000)
wine.pred.em = predict(wine.nn.em, wine.em.data,type="class")
emtr_matrix = table(wine.pred.em, wine.em.data$quality)
emnn_train_err = 1-((emtr_matrix[1,3]+emtr_matrix[2,4])/1199)

wine.kmeans = kmeans(wine.train[,-12], centers=10, iter.max = 50, nstart=20)
wine.km.data = as.data.frame(wine.kmeans$cluster)
wine.km.data= rename(wine.km.data, c("wine.kmeans$cluster"="feat"))
#wine.km.data$feat = as.factor(wine.km.data$feat)
wine.km.data = cbind(wine.km.data, wine.train$quality)
wine.km.data= rename(wine.km.data, c("wine.train$quality"="quality"))
wine.nn.km = nnet(wine.km.data$quality~wine.km.data$feat, data=wine.km.data, size=12, decay=5e-4, maxit=1000)
wine.pred.km = predict(wine.nn.km, wine.km.data,type="class")
kmtr_matrix = table(wine.pred.km, wine.km.data$quality)
kmnn_train_err = 1-((kmtr_matrix[1,3]+kmtr_matrix[2,4])/1199)

