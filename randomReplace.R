compareNA <- function(v1,v2) {
    # primerjanje v1 in v2 tudi NA
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
   }
replaceRandom<-function(odstotki=od){
 #vrne indeks zamenjave 
 i<-1
 random<-runif(1,0,1)
 while(random>=0){
 random<-random-odstotki[i]
 i<-i+1
 }
 i<-i-1
 return(i)
 }

replaceAllRandom<-function(col=col){
#zamenja vse vrednosti v stolpcu na podlagi frekvenc pojavitev
st=col
b=compareNA(v1=st,v2=NA)
indexNa<-which(b,arr.ind=FALSE,useNames=TRUE)
b<-!b
index<-which(b,arr.ind=FALSE,useNames=TRUE)
nonNaValues=st[index]
relative=prop.table(table(nonNaValues))
odstotki<-as.vector(relative)
vrednosti<-names(relative)
j<-1
while(j<=length(indexNa)){
	i<-replaceRandom(odstotki=odstotki)
	st[indexNa[j]]<-vrednosti[i]
	j<-j+1
}
return(as.numeric(st))
}


data<-read.table("pollution.txt",header=T,sep=",")
atributi<-names(data)
for (i in 1:length(atributi)){
 data[[i]]<- replaceAllRandom(col=data[[i]])
 }
 
 indexO3<-match("O3_max",atributi)
data[,indexO3]<-cut(data[,indexO3], c(-Inf, 60, 120, 180, Inf), labels=c("LOW", "MEDIUM", "HIGH", "EXTREME"))
sel <- sample(1:nrow(data), size=as.integer(nrow(data)*0.7), replace=F)
learn <- data[sel,]
test <- data[-sel,]

library(CORElearn)
maj.class <- which.max(table(learn$O3_max))
 ca.vals <- table(test$O3_max)[maj.class]/nrow(test)
 observed <- test$O3_max

 for (m in c("tree", "rf", "knn", "bayes"))
 {
 obj <- CoreModel(O3_max ~ ., learn, model=m)
 predicted <- predict(obj, test, type="class")
 tab <- table(observed, predicted)
 ca.vals <- c(ca.vals, sum(diag(tab))/sum(tab))
 }
names(ca.vals) <- c("majority", "tree", "rf", "knn", "bayes")
barplot(ca.vals, xlab="models", ylab="Classification accuracy", main="Results")

library(ipred)

mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

cv.res <- vector()

for (m in c("tree", "rf", "knn", "bayes"))
{
	 res <- errorest(O3_max~., data=data, model = mymodel, predict = mypredict, target.model=m)
	cv.res <- c(cv.res, 1-res$error)
}
