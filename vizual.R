
raw_data <- read.table("pollution.txt", header = T, sep=",")



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
raw_data$DATE=as.numeric(raw_data$DATE)
raw_data$TRAJ=as.factor(raw_data$TRAJ)
raw_data$SHORT_TRAJ=as.factor(raw_data$SHORT_TRAJ)

summary(raw_data)

for (i in 2:7){
  plot(raw_data$DAYINYEAR,raw_data[,i],ylab=names(raw_data)[i])
}