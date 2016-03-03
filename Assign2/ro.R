##JRE8 x 64 for Windows required

library(nnet)
library(GenSA)
library(GA)

library(nnet)

load.data = function(){
  
  neuralwine = read.csv2(file="C:/Users/bparli/Downloads/winequality-red.csv", header=TRUE)
  neuralwine$citric.acid=as.numeric(as.character(neuralwine$citric.acid))
  neuralwine$fixed.acidity=as.numeric(as.character(neuralwine$fixed.acidity))
  neuralwine$volatile.acidity=as.numeric(as.character(neuralwine$volatile.acidity))
  neuralwine$residual.sugar=as.numeric(as.character(neuralwine$residual.sugar))
  neuralwine$chlorides=as.numeric(as.character(neuralwine$chlorides))
  neuralwine$free.sulfur.dioxide=as.numeric(as.character(neuralwine$free.sulfur.dioxide))
  neuralwine$total.sulfur.dioxide=as.numeric(as.character(neuralwine$total.sulfur.dioxide))
  neuralwine$density=as.numeric(as.character(neuralwine$density))
  neuralwine$pH=as.numeric(as.character(neuralwine$pH))
  neuralwine$sulphates=as.numeric(as.character(neuralwine$sulphates))
  neuralwine$alcohol=as.numeric(as.character(neuralwine$alcohol))
  neuralwine$quality=as.factor(neuralwine$quality)
  
  mins = apply(neuralwine[,-12], 2, min)
  maxs = apply(neuralwine[,-12], 2, max)
  neuralscaled = as.data.frame(scale(neuralwine[,-12], center = mins, scale = maxs - mins))
  neuralscaled = cbind(neuralscaled,quality=neuralwine$quality)
  
  set.seed(1)
  samp.wine=floor(nrow(scaled)*0.75)
  wine.train_ind <- sample(seq_len(nrow(neuralscaled)), size = samp.wine)
  wine.train=neuralscaled[wine.train_ind,]
  wine.test=neuralscaled[-wine.train_ind,]
  return(wine.train)
}

## This function creates a list of neighbours to test
## Distance function adds a value to each dimension

rhc.nb = function(val=0, d=0.01) {
  nb.out = val
  n = length(val)
  max = d # runif(1, 0, d)
  min = -1*max
  #nb.out = rbind(nb.out, nb.out + min)
  #nb.out = rbind(nb.out, nb.out + max) 
  for(i in 1:n) {
    nb.left = val
    nb.left[i] = nb.left[i] - d
    nb.right = val
    nb.right[i] = nb.right[i] + d    
    nb.out = rbind(nb.out, nb.left)
    nb.out = rbind(nb.out, nb.right)
  }
  nb.out
}
## rhc.test is the test set
## rhc.testIdx is the test results expected
## rhc.seed is the start seed
## rhc.min/max are the min and max values of the weights
## rhc.d is the distance threshold to evaluate a neighbour
## rhc.n is the number of weights
## rhc.iter is the number of iterations that we loop over


nnet.rhc = function(rhc.d=0.01, rhc.min=-100, rhc.max=100, rhc.n=222, rhc.test, rhc.testIdx, rhc.seed=1, rhc.iter=100) {
  t1 = proc.time()
  j = 1
  rhc.best.score = 0
  set.seed(rhc.seed)
  for(k in 1:rhc.iter) {
    rhc.keepgoing = TRUE
    while(all(rhc.keepgoing)==TRUE) {
      rhc.keepgoing = FALSE
      #print(c(j, rhc.best.score))
      j = j + 1 
      rhc.initial = runif(as.numeric(rhc.n), as.numeric(rhc.min), as.numeric(rhc.max))
      rhc.neighbors = rhc.nb(val=rhc.initial, d=rhc.d)
      i = 1
      rhc.best.value = 0
      rhc.best.results = NULL
      for(i in 1:nrow(rhc.neighbors)) {
        #for(i in 1:1) {
        rhc.test.value = rhc.neighbors[i, ]
        rhc.test.fit = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
             data=rhc.test, size=3, maxit=0, Wts=rhc.test.value)
        #rhc.test.fit = cars.nn = nnet(class~buying+maint+doors+persons+lug_boot+safety, data=rhc.test, size=3, type="class", maxit=0, 
        #                             Wts=rhc.test.value)

        rhc.test.results = predict(rhc.test.fit, newdata=rhc.test, type="class")
        rhc.test.score = sum((rhc.test.results == rhc.testIdx) == TRUE)      
        if(rhc.test.score > rhc.best.score) {
          rhc.keepgoing = TRUE
          rhc.best.value = rhc.test.value
          rhc.best.fit = rhc.test.fit
          rhc.best.results = rhc.test.results
          rhc.best.score = rhc.test.score     
          rhc.best.seed = rhc.seed*k
        } 
      }
    }  
  }
  
  rhc.best.score = rhc.best.score/nrow(rhc.test)
  
  rhc.run.time = proc.time() - t1
  list(best.model=rhc.best.fit, best.score=rhc.best.score, best.seed=rhc.best.seed, run.time=rhc.run.time) 
}

## sa.temp is initial temperature
## sa.test is the test set
## sa.testIdx is the test results expected
## sa.seed is the start seed
## sa.d is the absolute value of the upper and lower bounds to the weights
## sa.n is the number of weights
## sa.iter is the number of iterations that is passed to the SA function


nnet.sa = function(sa.train, sa.class, sa.seed=1, sa.d=100, sa.num_weights=222, sa.iter=100, sa.time = 900, sa.temp = 100) {
  t1 = proc.time()
  
  set.seed(sa.seed)
  lower = as.numeric(-1.*rep(sa.d, sa.num_weights))
  upper = as.numeric(-1.*lower)
  fit = function(fit.wts, fit.train=sa.train, fit.class=sa.class) {
    temp.fit = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
                   data=fit.train, size=3, maxit=0, Wts=fit.wts)
    temp.predict = predict(temp.fit, newdata=fit.train, type="class")
    temp.score = sum((temp.predict == fit.class) == FALSE)
    as.numeric(temp.score)
  }
  best.fit = GenSA(par=NULL, fn=fit, lower=lower, upper=upper,
                    control=list(maxit=sa.iter, max.time=sa.time))
  sa.best.fit = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
       data=sa.train, size=3, maxit=0, Wts=best.fit$par, temperature=sa.temp)
  sa.best.score = (nrow(sa.train) - best.fit$value)/nrow(sa.train)
  sa.best.seed = sa.seed
  sa.run.time = proc.time() - t1
  list(best.model=sa.best.fit, best.score=sa.best.score, best.seed=sa.best.seed, run.time=sa.run.time)  
}


## ga.test is the test set
## ga.testIdx is the test results expected
## ga.seed is the start seed
## ga.d is the absolute value of the upper and lower bounds to the weights
## ga.n is the number of weights
## ga.iter is the number of iterations that is passed to the GA function

nnet.ga = function(ga.train, ga.class, ga.seed=1, ga.d=100, ga.num_weights=222, ga.iter=10, ga.pc=0.8, ga.pm=0.1, ga.elitism=0.5) {
  t1 = proc.time()
  
  set.seed(ga.seed)
  lower = as.numeric(-1.*rep(ga.d, ga.num_weights))
  upper = as.numeric(-1.*lower)
  fit = function(fit.wts, fit.train=ga.train, fit.class=ga.class) {
    temp.fit = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
                   data=fit.train, size=12, maxit=0, Wts=fit.wts)
    temp.predict = predict(temp.fit, newdata=fit.train, type="class")
    temp.score = sum((temp.predict == fit.class) == TRUE)
    as.numeric(temp.score)
  }
  best.fit = ga(type="real-valued", fitness=fit, min=lower, max=upper, maxiter=ga.iter, monitor=NULL, 
                parallel=TRUE, run=20, seed=ga.seed, elitism =ga.elitism)
  
  ga.best.fit = nnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
                 data=ga.train, size=12, maxit=0, Wts=best.fit@solution[1,])
  
  ga.best.score = max(best.fit@fitness)/nrow(ga.train)
  ga.best.seed = ga.seed
  ga.run.time = proc.time() - t1
  list(best.model=ga.best.fit, best.score=ga.best.score, best.seed=ga.best.seed, run.time=ga.run.time)  
}
