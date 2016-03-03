source('~/MachineLearning/Assign2/ro.R')

wine.train = load.data()

wine.nn = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
               data=wine.train, size=3, decay=5e-4, maxit=1000)
wine.pred = predict(wine.nn, wine.train,type="class")
tr_matrix = table(wine.pred, wine.train$quality)


seeds = runif(10, min=1, max=5000)
best=0
seed= 0 
scoress <- rep(0.1, 10)
weights=rep(1,60)
for(e in 1:10){
  tmp= nnet.sa(sa.train=wine.train, sa.class=wine.train$quality, sa.iter=5, sa.seed=seeds[e], sa.num_weights = 60, sa.d=30, sa.temp=10000)
  scoress[e] = tmp$best.score
  if(tmp$best.score > best){
    best = tmp$best.score
    seed = tmp$best.seed
    weights = tmp$best.model$wts
  }
}