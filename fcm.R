#fcm code
data("iris")
summary(iris)
require(Metrics)
require(hydroPSO)
require(caret)
require(caret)

featurePlot(iris[,c(1,2)],iris[,4])
fcm <- function (input, wts, t) {
  size <- length(input)
  wts <- matrix(wts,size,size)
  temp <- input
  concepts <- input
  for(step in 1:t){
    for(i in 1:size){
      concepts[i] <- sum(temp*wts[,i])
    }
    temp <- concepts
  }
  return(temp)
}

runfcm <- function (data, wts, t, num_target = "all") {
  output <- fcm(data,wts,t)
  if(num_target=='all'){return(output)}
  else{
    return(output[(length(output)-num_target + 1):length(output)])
    }
}

objfcm <- function(wts, data, target, t, num_target) {
  concepts <- runfcm(inputs, wts,t,num_target)
  error <- sqrt(mse(concepts,target))
}

trainfcm <- function(data, target, t, num_target){
  size <-length(data[1,])^2
  wts <- hydroPSO(rep(x = 0,size),fn = objfcm, data, target, t, num_target, rep(x = -1,size),rep(x = 1,size))
  return(wts)
}

target <- model.matrix( ~ 0 + Species, iris)
inputdat <- cbind(iris[1,1:ncol(iris)-1], rep(0,nrow(iris)), rep(0,nrow(iris)), rep(0,nrow(iris)))

#training FCM
zest <- trainfcm(inputdat,target,5,3)
cat("hello I'm just cheking out how to cat")
