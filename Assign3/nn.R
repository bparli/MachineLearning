source('~/MachineLearning/Assign3/unsup.R')
source('~/MachineLearning/Assign3/rproj.R')
source('~/MachineLearning/Assign3/pca.R')
source('~/MachineLearning/Assign3/ica.R')
source('~/MachineLearning/Assign3/unsup.R')
source('~/MachineLearning/Assign3/svd.R')
library(nnet)
library(plyr)
library(ggplot2)

wine.train = load.wine_data()
wine.train$quality = as.factor(wine.train$quality)

wine.nn = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine.train, size=12, decay=5e-4, maxit=1000)
wine.pred = predict(wine.nn, wine.train,type="class")
tr_matrix = table(wine.pred, wine.train$quality)
nn_train_err = 1-((tr_matrix[1,1]+tr_matrix[2,2]+tr_matrix[3,3]+tr_matrix[4,4]+tr_matrix[5,5]+ tr_matrix[6,6])/nrow(wine.train))


#wine.ica.data = wine.ica$S[,1:11]
#wine.ica.data = as.data.frame(wine.ica.data)
#wine.ica.data = cbind(wine.ica.data, wine.train$quality)
#wine.ica.data= rename(wine.ica.data, c("wine.train$quality"="quality"))
#wine.nn.ica = nnet(wine.ica.data$quality~wine.ica.data$V1+wine.ica.data$V2+wine.ica.data$V3+wine.ica.data$V4+wine.ica.data$V5+wine.ica.data$V6+wine.ica.data$V7+wine.ica.data$V8+wine.ica.data$V9+wine.ica.data$V10+wine.ica.data$V11, 
#                   data=wine.ica.data, size=12, decay=5e-4, maxit=1000)
#wine.pred.ica = predict(wine.nn.ica, wine.ica.data,type="class")
#icatr_matrix = table(wine.pred.ica, wine.ica.data$quality)
#icann_train_err = 1-((icatr_matrix[1,1]+icatr_matrix[2,2]+icatr_matrix[3,3]+icatr_matrix[4,4]+icatr_matrix[5,5]+ icatr_matrix[6,6])/nrow(wine.ica.data))

wine.ica.data = wine.ica$S[,1:6]
wine.ica.data = as.data.frame(wine.ica.data)
wine.ica.data = cbind(wine.ica.data, wine.train$quality)
wine.ica.data= rename(wine.ica.data, c("wine.train$quality"="quality"))
wine.nn.ica = nnet(wine.ica.data$quality~wine.ica.data$V1+wine.ica.data$V2+wine.ica.data$V3+wine.ica.data$V4+wine.ica.data$V5, 
                   data=wine.ica.data, size=12, decay=5e-4, maxit=1000)
wine.pred.ica = predict(wine.nn.ica, wine.ica.data,type="class")
icatr_matrix = table(wine.pred.ica, wine.ica.data$quality)
icann_train_err = 1-((icatr_matrix[1,1]+icatr_matrix[2,2]+icatr_matrix[3,3]+icatr_matrix[4,4]+icatr_matrix[5,5]+ icatr_matrix[6,6])/nrow(wine.ica.data))
# 2  -  43.1%
# 3  -  39.86
# 4  -  36.44
# 5  -  35.6 


#wine.pca.data = wine.pca$scores[,1:11]
#wine.pca.data = as.data.frame(wine.pca.data)
#wine.pca.data = cbind(wine.pca.data, wine.train$quality)
#wine.pca.data= rename(wine.pca.data, c("wine.train$quality"="quality"))
#wine.nn.pca = nnet(wine.pca.data$quality~wine.pca.data$Comp.1+wine.pca.data$Comp.2+wine.pca.data$Comp.3+wine.pca.data$Comp.4+wine.pca.data$Comp.5+wine.pca.data$Comp.6+wine.pca.data$Comp.7+wine.pca.data$Comp.8+wine.pca.data$Comp.9+wine.pca.data$Comp.10+wine.pca.data$Comp.11, 
#                   data=wine.pca.data, size=12, decay=5e-4, maxit=1000)
#wine.pred.pca = predict(wine.nn.pca, wine.pca.data,type="class")
#pcatr_matrix = table(wine.pred.pca, wine.pca.data$quality)
#pcann_train_err = 1-((pcatr_matrix[1,1]+pcatr_matrix[2,2]+pcatr_matrix[3,3]+pcatr_matrix[4,4]+pcatr_matrix[5,5]+ pcatr_matrix[6,6])/nrow(wine.pca.data))

wine.pca.data = wine.pca$scores[,1:6]
wine.pca.data = as.data.frame(wine.pca.data)
wine.pca.data = cbind(wine.pca.data, wine.train$quality)
wine.pca.data= rename(wine.pca.data, c("wine.train$quality"="quality"))
wine.nn.pca = nnet(wine.pca.data$quality~wine.pca.data$Comp.1+wine.pca.data$Comp.2+wine.pca.data$Comp.3+wine.pca.data$Comp.4+wine.pca.data$Comp.5, 
                   data=wine.pca.data, size=12, decay=5e-4, maxit=1000)
wine.pred.pca = predict(wine.nn.pca, wine.pca.data,type="class")
pcatr_matrix = table(wine.pred.pca, wine.pca.data$quality)
#pcann_train_err = 1-((pcatr_matrix[1,1]+pcatr_matrix[2,2]+pcatr_matrix[3,3]+pcatr_matrix[4,4]+pcatr_matrix[5,5]+ pcatr_matrix[6,6])/nrow(wine.pca.data))
#2  -  40.3
#3  -  39.28
#4  -  37.86
#5  -  36.64


rand_errs = c()
#for(i in 1:1){
#  wine.rand = gaussian_random_projection(wine.train[,-12], n_features=6)
#  wine.rand.data = as.data.frame(wine.rand$RP)
#  wine.rand.data = cbind(wine.rand.data, wine.train$quality)
#  wine.rand.data= rename(wine.rand.data, c("wine.train$quality"="quality"))
#  wine.nn.rand = nnet(wine.rand.data$quality~wine.rand.data$V1+wine.rand.data$V2+wine.rand.data$V3+wine.rand.data$V4+wine.rand.data$V5++wine.rand.data$V6, 
#                      data=wine.rand.data, size=12, decay=5e-4, maxit=1000)
#  wine.pred.rand = predict(wine.nn.rand, wine.rand.data, type="class")
#  randtr_matrix = table(wine.pred.rand, wine.rand.data$quality)
  #randnn_train_err = 1-((randtr_matrix[1,1]+randtr_matrix[2,2]+randtr_matrix[3,3]+randtr_matrix[4,4]+randtr_matrix[5,5]+ randtr_matrix[6,6])/nrow(wine.rand.data))
  #rand_errs = c(rand_errs, randnn_train_err)
#}
#2  -  45.8
#3  -  42.7
#4  -  40.95
#5  -  38.8

wine.svd.data = as.data.frame(wine.recon)
wine.svd.data = cbind(wine.svd.data, wine.train$quality)
wine.svd.data= rename(wine.svd.data, c("wine.train$quality"="quality"))
wine.nn.svd = nnet(wine.svd.data$quality~wine.svd.data$V1+wine.svd.data$V2+wine.svd.data$V3+wine.svd.data$V4+wine.svd.data$V5,
                   data=wine.svd.data, size=12, decay=5e-4, maxit=1000)
wine.pred.svd = predict(wine.nn.svd, wine.svd.data, type="class")
svdtr_matrix = table(wine.pred.svd, wine.svd.data$quality)
svdnn_train_err = 1-((svdtr_matrix[1,1]+svdtr_matrix[2,2]+svdtr_matrix[3,3]+svdtr_matrix[4,4]+svdtr_matrix[5,5]+ svdtr_matrix[6,6])/nrow(wine.svd.data))
# 2 -  48.37
# 3 -  46.6
# 4 -  43.7
# 5 -  41.3

pca_errs = c(51.5, 40.3, 39.28, 37.86, 36.64, 33.8)
ica_errs = c(49.5, 43.1, 39.86, 36.44, 35.6, 35.1)
rp_errs = c(48.2, 45.7, 42.7, 40.95, 38.8, 32.8)
svdnn_errs = c(50.3, 48.37, 46.6, 43.7, 41.3, 34.6)
errs = c(pca_errs, ica_errs, rp_errs, svdnn_errs)

models = rep("pca",6)
for(name in c("ica", "rp","svd")){
  tmp.names = rep(name,6)  
  models = c(models, tmp.names)
}

nums = c(1,2,3,4,5,6)
num.comps = c(nums, nums, nums, nums)
iters.results = data.frame(models, nums, errs)

names = rep("pca",11)
for(name in c("ica")){
  tmp.names = rep(name,11)  
  names = c(names, tmp.names)
}

comps = c(wine.pca$sdev, wine.ica$vafs)

tmp = c(1,2,3,4,5,6,7,8,9,10,11)
x= c(tmp, tmp)
comp.results = data.frame(comps, names, x)

pl23 = ggplot(comp.results, aes(x=x, y=comps, group=names)) +
  geom_line(aes(colour=names)) +
  ggtitle("PCA and ICA Components") +
  theme(axis.title=element_text(face="bold.italic", color="brown"), title=element_text(face="bold", color="brown")) +
  xlab("Variance") +
  ylab("Components") +
  theme(legend.position="bottom")



svd.name = rep("svd",11)
svd_plot = data.frame(wine.svd$d, svd.name, tmp)
  
pl24 = ggplot(svd_plot, aes(x=tmp, y=wine.svd.d, group=svd.name)) +
  geom_line(aes(colour=svd.name)) +
  ggtitle("SVD") +
  theme(axis.title=element_text(face="bold.italic", color="brown"), title=element_text(face="bold", color="brown")) +
  xlab("") +
  ylab("Singular Values of X") +
  theme(legend.position="bottom")

