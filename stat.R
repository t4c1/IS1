analiza<-function(vzorec=vzorec,title=title){
	minval<-min(vzorec,na.rm=T)
	maxval<-max(vzorec,na.rm=T)
	average<-mean(vzorec,na.rm=T)
	medval<-median(vzorec, na.rm = T)
	stat<-as.table(c(minval,maxval,average,medval))
	names(stat)<-c("minval","maxval","average","medianaval")
	hist(vzorec,main=title)
	return (stat)	
}


raw_data <- read.table("pollution.txt", header = T, sep=",")


a1<-analiza(vzorec=raw_data[[2]],title="TRAJ")
print(a1)

if(F){
a2<-analiza(vzorec=raw_data[[3]],title="SHORT_TRAJ")
print(a2)

a3<-analiza(vzorec=raw_data[[4]],title="AMP_TMP2M_mean")
print(a3)

a4<-analiza(vzorec=raw_data[[5]],title="AMP_RH_mean")
print(a4)

a5<-analiza(vzorec=raw_data[[6]],title="AMP_WS_mean")
print(a5)

a6<-analiza(vzorec=raw_data[[7]],title="AMP_PREC_sum")
print(a6)

}