library(MASS)
library(caret)
library(isotree)
library('mclust')

df = iris
df <- na.omit(df) # remove any missing value

df[c(1,4)] <- lapply(df[c(1, 4)], function(x) c(-log(x)))

df$Species[df$Species == "setosa"] <- 1

sample_size <- floor(0.70 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = sample_size)
train_samp <- df[train_ind, ]
test_samp <- df[-train_ind, ]

train_samp = as.data.frame(train_samp)
test_samp = as.data.frame(test_samp)

x_test<- test_samp[,c(1:4)]

y_test<- test_samp[, 5]

x_train <- train_samp[, c(1:4)]
y_train <- train_samp[, 5]

Y_init = sample(1:3, nrow(train_samp), replace = T)
Y_init

n <- nrow(x_train)
K <- 3


basicKRF <- function(X,Y_init,K,maxIter)
{
  Y <- Y_init
  t <- 0
  n <- nrow(X)
  w <- matrix(1/K,K, n)
  w
  iso <- list()
  repeat{
    t <- t+1
    print("New iteration")
    for(k in 1:K)
    {
      Xk <- x_train[which(Y_init == k), ]
      Xo <- x_train[which.max(w[k, ]), ]
      Xk <- rbind(Xk, Xo)
      iso[[k]] <- isolation.forest(Xk)
    }
    Y1 <- vector("numeric", length = n)
    for(i in 1:n)
    {
      for(k in 1:K){
        w[k, i] <- 1 - predict(iso[[k]], Xk[i, ], type="score")
      }
      Y1[i] <- which.max(w[, i])
    }
    if(all(Y == Y1) || t == maxIter)
      break
    Y <- Y1
  }
  results <- list(Y = Y, t = t, ts = iso)
  return(results)
}

temp <- basicKRF(x_train, Y_init, 3, 10)


Cluster_purity <- function(clusters, classes) {
  purity_index <- sum(apply(table(classes, clusters), 2, max)) / length(clusters)
  return(purity_index)
}



predictKRF <- function(X, K, trees){
  predictions <- matrix(ncol=nrow(X), nrow=K)
  for(i in 1:K){
    temp <- predict(trees[[i]], X, type = "score")
    for(j in 1:nrow(X)){
      predictions[i, j] = 1-temp[j]
    }
  }
  Y <- vector("numeric", nrow(X))
  for(i in 1:nrow(X)){
    value <- -1
    index <- -1
    for(j in 1:K){
      if(predictions[j, i] > value){
        value <- predictions[j,i]
        index <- j
      }
    }
    Y[i] <- index
  }
  return(Y)
}

purity_sum <- 0
ari_sum <- 0

maxtrials <- 10
for(j in 1:maxtrials){
  print("New Trial")
  df = iris
  df <- na.omit(df) # remove any missing value
  
  
  df[c(1,4)] <- lapply(df[c(1, 4)], function(x) c(-log(x)))
  
  sample_size <- floor(0.70 * nrow(df))
  
  train_ind <- sample(seq_len(nrow(df)), size = sample_size)
  train_samp <- df[train_ind, ]
  test_samp <- df[-train_ind, ]
  
  train_samp = as.data.frame(train_samp)
  test_samp = as.data.frame(test_samp)
  
  x_test<- test_samp[,c(1:4)]
  
  y_test<- test_samp[, 5]
  y_test <- unclass(y_test)
  y_test
  
  x_train <- train_samp[, c(1:4)]
  y_train <- train_samp[, 5]
  
  Y_init = sample(1:3, nrow(train_samp), replace = T)
  Y_init
  
  n <- nrow(x_train)
  K <- 3
  
  output <- basicKRF(x_train,Y_init,3,10)
  
  y_test_predictions <- predictKRF(x_test, 3, output$ts)
  purity_sum = purity_sum + Cluster_purity(y_test, y_test_predictions)
  ari_sum <- ari_sum + adjustedRandIndex(y_test, y_test_predictions)
}

print(purity_sum / maxtrials)
print(ari_sum / maxtrials)


df <- iris

num_in_each_set <- nrow(df)/10

sample_size <- num_in_each_set
x <- matrix(nrow=10, ncol=15)
train_ind <- sample(seq_len(nrow(df)), size = sample_size)

tot <- 1:nrow(df)
x <- matrix(nrow=10, ncol=15)


for(i in 1:10){
  x[i, ] <- sample(tot, 15, replace = FALSE)
  tot <- setdiff(tot, x[i, ])
}

i <- 1
for(i in 1:10){
  if(i == 1){
    x_train <- df[c(x[2:10, ]), c(1:4)]
    y_train <- df[c(x[2:10, ]), 5]
    x_test <- df[x[1, ], c(1:4)]
    y_test <- df[x[1, ], 5]
  }
  else if(i == 10){
    
    x_train <- df[c(x[1:9, ]), c(1:4)]
    y_train <- df[c(x[1:9, ]), 5]
    x_test <- df[x[10, ], c(1:4)]
    y_test <- df[x[10, ], 5]
  }
  else{
    
    x_train <- df[c(x[c(1:(i-1), (i+1):10), ]), c(1:4)]
    y_train <- df[c(x[c(1:(i-1), (i+1):10), ]), 5]
    x_test <- df[x[i, ], c(1:4)]
    y_test <- df[x[i, ], 5]
  }
  
  purity_sum <- 0
  ari_sum <- 0
  
  maxtrials <- 10
  for(j in 1:maxtrials){
    Y_init = sample(1:3, nrow(x_train), replace = T)
    Y_init
    
    n <- nrow(x_train)
    K <- 3
    
    output <- basicKRF(x_train,Y_init,3,10)
    
    y_test_predictions <- predictKRF(x_test, 3, output$ts)
    purity_sum = purity_sum + Cluster_purity(y_test, y_test_predictions)
    ari_sum <- ari_sum + adjustedRandIndex(y_test, y_test_predictions)
  }
  
}






sample_size <- floor(0.75*nrow(iris))

tot <- 1:nrow(iris)




for(i in 1:20){
  print(paste0("Iteration: ", i))
  
  
  train_samples <- sample(tot, train_samples_number, replace = TRUE)
  test_samples <- setdiff(1:nrow(iris), unique(train_samples))
  train_iris <- df[unique(train_samples), ]
  test_iris <- df[test_samples, ]
  
  x_train <- train_iris[, c(1:4)]
  y_train <- train_iris[, 5]
  x_test <- test_iris[, c(1:4)]
  y_test <- test_iris[, 5]
  
  
  
  purity_sum <- 0
  ari_sum <- 0
  
  maxtrials <- 10
  for(j in 1:maxtrials){
    Y_init = sample(1:3, nrow(x_train), replace = T)
    Y_init
    
    n <- nrow(x_train)
    K <- 3
    
    output <- basicKRF(x_train,Y_init,3,10)
    
    y_test_predictions <- predictKRF(x_test, 3, output$ts)
    purity_sum = purity_sum + Cluster_purity(y_test, y_test_predictions)
    ari_sum <- ari_sum + adjustedRandIndex(y_test, y_test_predictions)
  }
}