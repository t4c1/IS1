library(rpart)
library(randomForest)
library(e1071)
library(kknn)
library(nnet)
library(CORElearn)
library(ipred)
#setwd("F:/Users/Tadej/Documents/Fax dn/Is/IS1")
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
atributi<-names(raw_data)
missing=c(na.omit)

coreTree=function(dist,data){
	CoreModel(dist,data, model="regTree", modelTypeReg = 6,minNodeWeightTree=3)
}

rpartTree=function(dist,data){
  rpart(dist,data,cp=0.002)
}

lwReg=function(dist,data){
  CoreModel(dist,data, model="regTree", modelTypeReg = 8,minNodeWeightTree=100000)
}

rf=function(dist,data){
  randomForest(dist,data,ntree=150)
}

reg.models=c(lm,lwReg,rpartTree,coreTree,randomForest,svm)
reg.m.names=c("lin reg","localy weighted reg","tree","tree2","forest","svm","avg","weighted avg")

#reg.models=c(rf,rf1,rf2,rf3,rf4,rf5)
#reg.m.names=c("tree","tree1","tree2","tree3","tree4","tree5","avg","weighted avg")

cla.models<-c("tree", "rf", "knn", "bayes")

reg.n.models=length(reg.models)

cross.val=10
reg.dists=c(O3_max ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            #PM10   ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            #PM2.5  ~ DATE + TRAJ +              AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            PM2.5  ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum)

reg.test.idx=c("O3_max","PM2.5")
reg.n.comb=2

reg.weights=1/c(0.37,0.62,0.43,0.36,0.28,0.32)**15
reg.weights=reg.weights/sum(reg.weights)

#za hitrejse izvajanje
class<-F
regre<-T
for(i in 1:length(missing)){
	data=missing[[i]](raw_data)
	if(regre){
  	for(l in 1:length(reg.dists)){
      reg.rmse=0*(1:(reg.n.models+reg.n.comb))
      reg.attrs=c(0)
      for(k in 1:cross.val){                     #cross validation
      	sel <- sample(1:nrow(data), size=as.integer(nrow(data)*0.8), replace=F)
      	learn <- data[sel,]
      	test <- data[-sel,]
      	predictions=data.frame()
    	  reg.attrs=reg.attrs+attrEval(reg.dists[[l]], learn, "RReliefFsqrDistance")
    		for(j in 1:reg.n.models){
    			m=reg.models[[j]](reg.dists[[l]] ,learn)
    			prediction=predict(m,test)
    			predictions=rbind(predictions,prediction)
        	#print(mae(test$O3_max,prediction))
        	#print(mse(test$O3_max,prediction))
        	#print(rmae(test$O3_max,prediction,mean(learn$O3_max)))
    			reg.rmse[[j]]=reg.rmse[[j]]+rmse(test[,reg.test.idx[l]],prediction,mean(learn[,reg.test.idx[l]]))
    		}
    	  #kombinirani modeli
    	  prediction=colSums(predictions)/reg.n.models
    	  j=j+1
    	  reg.rmse[[j]]=reg.rmse[[j]]+rmse(test[,reg.test.idx[l]],prediction,mean(learn[,reg.test.idx[l]]))
    	  
    	  for(k in 1:reg.n.models){
    	    predictions[k,]=predictions[k,]*reg.weights[k]
    	  }
    	  prediction=colSums(predictions)
    	  j=j+1
    	  reg.rmse[[j]]=reg.rmse[[j]]+rmse(test[,reg.test.idx[l]],prediction,mean(learn[,reg.test.idx[l]]))
  		}
  	  print("*********************************")
  	  print(reg.dists[[l]])
  	  print(reg.attrs)
    	for(j in 1:(reg.n.models+reg.n.comb)){
    	  print(reg.m.names[[j]])
    	  print(reg.rmse[[j]]/cross.val)
    	}
  	}
	}
	#klasifikacija
	if(class){
  	indexO3<-match("O3_max",atributi)
  	data[,indexO3]<-cut(data[,indexO3], c(-Inf, 60, 120, 180, Inf), labels=c("LOW", "MEDIUM", "HIGH", "EXTREME"))
  	sel <- sample(1:nrow(data), size=as.integer(nrow(data)*0.8), replace=F)
  	learn <- data[sel,]
  	test <- data[-sel,]
  	
  	maj.class <- which.max(table(learn$O3_max))
  	ca.vals <- table(test$O3_max)[maj.class]/nrow(test)
  	observed <- test$O3_max
  	cv.res <- vector()
  	mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
  	mypredict <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}
  	
  	
  	 for (m in cla.models)
  	 {
  	 obj <- CoreModel(O3_max ~ ., learn, model=m)
  	 predicted <- predict(obj, test, type="class")
  	 tab <- table(observed, predicted)
  	 ca.vals <- c(ca.vals, sum(diag(tab))/sum(tab))
  	 res <- errorest(O3_max~., data=data, model = mymodel, predict = mypredict, target.model=m)
  	cv.res <- c(cv.res, 1-res$error)
  	 }
  	 names(ca.vals)<-c("majority", "tree", "rf", "knn", "bayes")
  	print(ca.vals)
  	names(cv.res) <- cla.models
  	print(cv.res)
	}
}







