library(rpart)
library(randomForest)

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

reg.models=c(lm,rpart,randomForest)

for(i in 1:length(missing)){
	data=missing[[i]](raw_data)
	sel <- sample(1:nrow(data), size=as.integer(nrow(data)*0.7), replace=F)
	learn <- data[sel,]
	test <- data[-sel,]
	for(j in 1:length(reg.models)){
		m=reg.models[[j]](O3_max ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum ,learn)
		prediction=predict(m,test)
		print(mae(test$O3_max,prediction))
		print(mse(test$O3_max,prediction))
		print(rmae(test$O3_max,prediction,mean(learn$O3_max)))
		print(rmse(test$O3_max,prediction,mean(learn$O3_max)))
		print('**********************************')
	}
}






