library(rpart)
library(randomForest)
library(e1071)
library(kknn)
library(nnet)
library(CORElearn)

mae <- function(observed, predicted)
{
	mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
	sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
	mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
	sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}


raw_data <- read.table("pollution.txt", header = T, sep=",")
raw_data$TRAJ=as.factor(raw_data$TRAJ)
raw_data$SHORT_TRAJ=as.factor(raw_data$SHORT_TRAJ)
raw_data$DATE=as.numeric(raw_data$DATE)

missing=c(na.omit)

coreTree=function(dist,data){
	CoreModel(dist,data, model="regTree", modelTypeReg = 1)
}

reg.models=c(lm,rpart,coreTree,randomForest,svm)
reg.m.names=c("lin reg","tree","tree2","forest","svm")

cross.val=10
reg.dists=c(O3_max ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            PM10   ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            PM2.5  ~ DATE + TRAJ +              AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            PM2.5  ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum)

reg.test.idx=c("O3_max","PM10","PM2.5")
#reg.test.idx=c(8,9,10)

for(i in 1:length(missing)){
	data=missing[[i]](raw_data)
	for(l in 1:length(reg.dists)){
    reg.rmse=0*(1:length(reg.models))
    reg.attrs=c(0)
    for(k in 1:cross.val){                     #cross validation
    	sel <- sample(1:nrow(data), size=as.integer(nrow(data)*0.8), replace=F)
    	learn <- data[sel,]
    	test <- data[-sel,]
  	  reg.attrs=reg.attrs+attrEval(reg.dists[[l]], learn, "RReliefFsqrDistance")
  		for(j in 1:length(reg.models)){
  			m=reg.models[[j]](reg.dists[[l]] ,learn)
  			prediction=predict(m,test)
      	#print(mae(test$O3_max,prediction))
      	#print(mse(test$O3_max,prediction))
      	#print(rmae(test$O3_max,prediction,mean(learn$O3_max)))
  			reg.rmse[[j]]=reg.rmse[[j]]+rmse(test[,reg.test.idx[l]],prediction,mean(learn[,reg.test.idx[l]]))
  		}
		}
	  print("*********************************")
	  print(reg.dists[[l]])
	  print(reg.attrs)
  	for(j in 1:length(reg.models)){
  	  print(reg.m.names[[j]])
  	  print(reg.rmse[[j]]/cross.val)
  	}
	}
}







