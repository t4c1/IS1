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

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  #1-winter
  #2-spring
  #3-summer
  #4-fall
  ifelse (d >= WS | d < SE, "1",
          ifelse (d >= SE & d < SS, "2",
                  ifelse (d >= SS & d < FE, "3", "4")))
}

getMonth<-function(DATES){
  months<-vector();
  for(i in 1:length(DATES)){
    months[i]<-format(as.Date(DATES[[i]], format = "%Y-%m-%d"),"%m")
  }
  return (months)
}
getYear<-function(DATES){
  year<-vector();
  for(i in 1:length(DATES)){
    year[i]<-format(as.Date(DATES[[i]], format = "%Y-%m-%d"),"%Y")
  }
  return (year)
}

getDay<-function(DATES){
  day<-vector();
  for(i in 1:length(DATES)){
    day[i]<-format(as.Date(DATES[[i]], format = "%Y-%m-%d"),"%d")
  }
  return (day)
}

getDayInYear<-function(DATES){
  day<-vector();
  for(i in 1:length(DATES)){
    day[i]<-format(as.Date(DATES[[i]], format = "%Y-%m-%d"),"%j")
  }
  return (day)
}

dates2Season<-function(dates=dates){
  season<-vector();
  for(i in 1:length(dates)){
    season[[i]]<-getSeason(dates[[i]])
  }
  return (season)
}

removeNARows<-function(final,col){
  for(i in col){
    final<-final[complete.cases(final[,i]),]
  }
  return (final)
}

klasifikacija<-function(data=data,indexObserved=indexObserved,index=index,odstotek=odstotek,cross=cross){
  res1<-c(0,0,0,0,0,0,0);
  res2<-c(0,0,0,0,0,0);
  for(k in 1:cross){
    sel <- sample(1:nrow(data), size=as.integer(nrow(data)*odstotek), replace=F)
    learn <- data[sel,]
    test <- data[-sel,]
    maj.class <- which.max(table(learn[[indexObserved]]))
    ca.vals <- table(test[[indexObserved]])[maj.class]/nrow(test)
    observed <- test[[indexObserved]]
    cv.res <- vector()
    mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
    mypredict <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}
    
    
    for (m in cla.models)
    {
      obj <- CoreModel(reg.class[[index]], learn, model=m)
      predicted <- predict(obj, test, type="class")
      tab <- table(observed, predicted)
      ca.vals <- c(ca.vals, sum(diag(tab))/sum(tab))
      res <- errorest(reg.class[[index]], data=data, model = mymodel, predict = mypredict, target.model=m)
      cv.res <- c(cv.res, 1-res$error)
    }
    print(k)
    print(as.vector(ca.vals))
    print(cv.res)
    res1<-as.vector(res1)+as.vector(ca.vals)
    res2<-as.vector(res2)+as.vector(cv.res)
  }
  x<-sprintf("%7s",c("major.","rf","rfNear","tree","knn","knnKernel","bayes"))
  print(x)
  print(res1/cross)
  x<-sprintf("%7s",cla.models)
  print(x)
  print(res2/cross)
  
}

replaceWithAverage<-function(DATA){
  rez<-DATA;
  for(i in 1:length(DATA)){
    avg<-mean(as.numeric(raw_data[[i]]), na.rm=TRUE)
    for(j in 1:length(rez[[i]])){
      if(is.na(rez[[j,i]])){
        rez[[j,i]]<-avg
      }
    }
  }
  return (rez)
}

raw_data <- read.table("pollution.txt", header = T, sep=",")
raw_data=replaceWithAverage(raw_data)
raw_data$TRAJ=as.factor(raw_data$TRAJ)
raw_data$SHORT_TRAJ=as.factor(raw_data$SHORT_TRAJ)
season<-dates2Season(dates=raw_data$DATE)
day<-getDay(raw_data$DATE)
months<-getMonth(raw_data$DATE)
year<-getYear(raw_data$DATE)
dayinyear<-getDayInYear(raw_data$DATE)
raw_data$SEASONS<-as.numeric(season)
raw_data$YEAR<-as.numeric(year)
raw_data$MONTHS<-as.numeric(months)
raw_data$DAY<-as.numeric(day)
raw_data$DAYINYEAR<-as.numeric(dayinyear)
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

cla.models<-c("rf","rfNear","tree","knn","knnKernel","bayes")

reg.n.models=length(reg.models)

cross.val=10
reg.dists=c(O3_max ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum+SEASONS+YEAR+MONTHS+DAY+DAYINYEAR,
            #PM10   ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum+SEASONS+YEAR+MONTHS+DAY+DAYINYEAR,
            #PM2.5  ~ DATE + TRAJ +              AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum,
            PM2.5  ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum+SEASONS+YEAR+MONTHS+DAY+DAYINYEAR)
reg.class=c(O3_max ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum+SEASONS+YEAR+MONTHS+DAY+DAYINYEAR,
            PM10   ~ DATE + TRAJ + SHORT_TRAJ + AMP_TMP2M_mean + AMP_RH_mean + AMP_WS_mean + AMP_PREC_sum+SEASONS+YEAR+MONTHS+DAY+DAYINYEAR)
reg.test.idx=c("O3_max","PM2.5")
reg.n.comb=2

reg.weights=1/c(0.37,0.62,0.43,0.36,0.28,0.32)**15
reg.weights=reg.weights/sum(reg.weights)

#za hitrejse izvajanje
class<-F
regre<-T
for(i in 1:length(missing)){
  data=missing[[i]](raw_data)
  print(length(data[[1]]))
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
    data<-removeNARows(raw_data,c(1:7))
    data[,11]<-cut(data[,11], c(-Inf, 1, 2, 3, Inf), labels=c("SPRING", "SUMMER", "FALL","WINTER"))
    data1<-data[complete.cases(data[,8]),]
    
    data1[,8]<-cut(data1[,8], c(-Inf, 60, 120, 180, Inf), labels=c("LOW", "MEDIUM", "HIGH", "EXTREME"))
    
    print("O3_max")
    klasifikacija(data=data1,indexObserved =8 ,index=1,odstotek=0.8,cross=30)
    
    print("PM10")
    data2<-data[complete.cases(data[,9]),]
    data2[,9]<-cut(data2[,9], c(-Inf, 35, 50, Inf), labels=c("LOW", "MODERATE", "HIGH"))
    klasifikacija(data=data2,indexObserved =9 ,index=2,odstotek=0.8,cross=30)
  }
}

#"rf","rfNear","tree","knn","knnKernel","bayes"







