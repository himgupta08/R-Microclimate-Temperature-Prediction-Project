
install.packages("pracma", "C:/Users/Himanshu/Anaconda3/R/library", repos = "http://cran.us.r-project.org")
install.packages("car", "C:/Users/Himanshu/Anaconda3/R/library", repos = "http://cran.us.r-project.org")
install.packages("lmtest", "C:/Users/Himanshu/Anaconda3/R/library", repos = "http://cran.us.r-project.org")
install.packages("e1071", "C:/Users/Himanshu/Anaconda3/R/library", repos = "http://cran.us.r-project.org")
install.packages("gbm", "C:/Users/Himanshu/Anaconda3/R/library", repos = "http://cran.us.r-project.org")

weather<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/WeatherData/weather data_10Nov_10Jan_v2.csv")
in_bushes<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/Microhabitats data/10thNov_10thJan_Data/in bushes.csv")
in_the_open<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/Microhabitats data/10thNov_10thJan_Data/in the open.csv")
puddle<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/Microhabitats data/10thNov_10thJan_Data/puddle.csv")
storm_drain<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/Microhabitats data/10thNov_10thJan_Data/storm drain.csv")
tree_coverage<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/Microhabitats data/10thNov_10thJan_Data/tree coverage.csv")
water_meter<-read.csv("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/Microhabitats data/10thNov_10thJan_Data/water meter.csv")


# Missing value analysis in Microhabitat dataframes

dataset <- list(in_bushes, in_the_open, puddle, storm_drain, tree_coverage, water_meter)
names(dataset)<-c("in_bushes", "in_the_open", "puddle", "storm_drain", "tree_coverage", "water_meter") 

for (i in 1:length(dataset)) {

    nacount_cols  <- colSums(is.na(dataset[[i]]))
    
    print(paste(names(dataset)[i], ": out of total", nrow(dataset[[i]]), "values, ",
    "missing value in Date is", nacount_cols[1], "and missing value in Temp is", nacount_cols[2]
           , "and missing value in lux intensity is ", nacount_cols[3]
           , "and missing value in relative humidity is", nacount_cols[4]))
}

print(weather_na  <- colSums(is.na(weather)))

print(paste("in weather dataset, out of total", nrow(weather), "values, ",
    "missing value in Date is", weather_na[1], "and missing value in Temp is", weather_na[2],
    "and missing value in humidity is ", weather_na[3]))

# Wind direction has VRB values which means that direction is variable with a variation of more than 180 degree and wind speed 6 km/h

# Observed that humidity data in microhabitats is captured from 20th Nov
# Capture minute and hour from weather data, and filter out data in all files from 20th nov, 5 o clock

weather$minute<-format(as.POSIXct(strptime(weather$DATE,"%m/%d/%Y %H:%M",tz="")) ,format = "%M")
weather$hour<-format(as.POSIXct(strptime(weather$DATE,"%m/%d/%Y %H:%M",tz="")) ,format = "%H")

# Filter data from 20th Nov 2017 after 5 pm
in_bushes1  <- in_bushes[((as.POSIXct(strptime(in_bushes$Date.Time,"%m/%d/%Y %H:%M"))) >= "2017-11-20 17:00:00 CST"),]
storm_drain1  <- storm_drain[((as.POSIXct(strptime(storm_drain$Date.Time,"%m/%d/%Y %H:%M"))) >= "2017-11-20 17:00:00 CST"),]
in_the_open1  <- in_the_open[((as.POSIXct(strptime(in_the_open$Date.Time,"%m/%d/%Y %H:%M"))) >= "2017-11-20 17:00:00 CST"),]
tree_coverage1  <- tree_coverage[((as.POSIXct(strptime(tree_coverage$Date.Time,"%m/%d/%Y %H:%M"))) >= "2017-11-20 17:00:00 CST"),]

weather1  <- weather[((as.POSIXct(strptime(weather$DATE,"%m/%d/%Y %H:%M"))) >= "2017-11-20 17:00:00 CST")
                     & ((as.POSIXct(strptime(weather$DATE,"%m/%d/%Y %H:%M"))) <= "2018-1-10 8:00:00 CST"),]


# tail(weather1)
# nrow(weather1) # 1668

# nrow(in_bushes1)
# nrow(storm_drain1)
# nrow(in_the_open1)
# nrow(tree_coverage1)


# Run Missing value analysis AGAIN in Microhabitat dataframes

dataset <- list(in_bushes1, in_the_open1, storm_drain1, tree_coverage1)
names(dataset)<-c("in_bushes", "in_the_open", "storm_drain", "tree_coverage") 

for (i in 1:length(dataset)) {

    nacount_cols  <- colSums(is.na(dataset[[i]]))
    
    print(paste(names(dataset)[i], ": out of total", nrow(dataset[[i]]), "values, ",
    "missing value in Date is", nacount_cols[1], "and missing value in Temp is", nacount_cols[2]
           , "and missing value in lux intensity is ", nacount_cols[3]
           , "and missing value in relative humidity is", nacount_cols[4]))
}

weather_na  <- colSums(is.na(weather1))

print(paste("in weather dataset, out of total", nrow(weather1), "values, ",
    "missing value in Date is", weather_na[1], "and missing value in Temp is", weather_na[2],
    "and missing value in humidity is ", weather_na[3]))
print(weather_na  <- colSums(is.na(weather1)))


# HG- Dropping rows by selecting minutes = 53 and only first 2 cols. 
weather2<-weather1[weather1$minute=="53",] 
nrow(weather2)
# Selected 1231 rows out of 1693 rows - Removed data that is not at 53 minutes

# tail(weather2) # Last row consist of 10th Jan 2018 at 7:53 in morning
# tail(in_the_open1)
# tail(in_bushes1)
# tail(puddle)
# tail(storm_drain1)
# tail(tree_coverage1)
# tail(water_meter)
# Last data in microhabitats recorded till 8:00 on 10th Jan. so limit mydata to 8 o clock in morning

# mydata  <- mydata[1:1472,]
# tail(mydata)

weather_na  <- colSums(is.na(weather2))
print(weather_na)
print(paste("in weather dataset, out of total", nrow(weather2), "values, ",
    "missing value in Date is", weather_na[1], "and missing value in Temp is", weather_na[2],
    "and missing value in humidity is ", weather_na[3]))

# Remove NA values (occuring sometimes due to double recording of same time) and limit data at 8 o clock in the morning

nrow(in_the_open2  <- in_the_open1[!is.na(in_the_open1$Temp), ]) # 4 NA values dropped
nrow(in_bushes2  <- in_bushes1[!is.na(in_bushes1$Temp), ])
nrow(storm_drain2  <- storm_drain1[!is.na(storm_drain1$Temp), ])
nrow(tree_coverage2  <- tree_coverage1[!is.na(tree_coverage1$Temp), ]) 
nrow(puddle2  <- puddle[!is.na(puddle$Temp), ]) # 8832 rows (no humidity data, so this data starts from 10th nov)
nrow(water_meter2  <- water_meter[!is.na(water_meter$Temp), ]) # same as puddle


# Taking hourly average of different habitats and binding it with weather file
# Leaving puddle, water meter as of now (Need to create weather data from 10th Nov). Fitting linear model only for datasets having humidity. 
for(i in 1:nrow(weather2)){
  weather2$in_open_temp[i]<-mean(in_the_open2[((6*(i-1))+1):((6*(i-1))+6),2])
  weather2$storm_drain_temp[i]<-mean(storm_drain2[((6*(i-1))+1):((6*(i-1))+6),2])
  weather2$tree_coverage_temp[i]<-mean(tree_coverage2[((6*(i-1))+1):((6*(i-1))+6),2])
  weather2$in_bushes_temp[i]<-mean(in_bushes2[((6*(i-1))+1):((6*(i-1))+6),2])
}

# Humidity data
for(i in 1:nrow(weather2)){
  weather2$in_open_Humidity[i]<-mean(in_the_open2[((6*(i-1))+1):((6*(i-1))+6),4], na.rm = T)
  weather2$storm_drain_Humidity[i]<-mean(storm_drain2[((6*(i-1))+1):((6*(i-1))+6),4], na.rm = T)
  weather2$tree_coverage_Humidity[i]<-mean(tree_coverage2[((6*(i-1))+1):((6*(i-1))+6),4], na.rm = T)
  weather2$in_bushes_Humidity[i]<-mean(in_bushes2[((6*(i-1))+1):((6*(i-1))+6),4], na.rm = T)
}
# tail(weather2, 20)

# Lux data
for(i in 1:nrow(weather2)){
  weather2$in_open_lux[i]<-mean(in_the_open2[((6*(i-1))+1):((6*(i-1))+6),3], na.rm = T)
  weather2$storm_drain_lux[i]<-mean(storm_drain2[((6*(i-1))+1):((6*(i-1))+6),3], na.rm = T)
  weather2$tree_coverage_lux[i]<-mean(tree_coverage2[((6*(i-1))+1):((6*(i-1))+6),3], na.rm = T)
  weather2$in_bushes_lux[i]<-mean(in_bushes2[((6*(i-1))+1):((6*(i-1))+6),3], na.rm = T)
}


# Missing value replacement for Humidity data
# weather2[((as.POSIXct(strptime(weather2$DATE,"%m/%d/%Y %H:%M"))) = "2017-12-15 5:53:00 CST")]
# colSums(is.na(weather2)) # some humidity cols have missing

# # Displaying Missing cases
# weather2[!complete.cases(weather2),]
#  which(is.na(weather2$storm_drain_Humidity)) # 589th row
#  which(is.na(weather2$in_bushes_Humidity)) #13 14 15 16th row

# fill storm drain by averaging 6 close humidities
weather2[589,'storm_drain_Humidity']  <- mean(weather2[587:592,'storm_drain_Humidity'], na.rm = T)
weather2[589,'Wind.direction']  <- 360

# # fill in bushes humidity by taking average of those hours
weather2[13,'in_bushes_Humidity']  <- mean(weather2[weather2$hour=='05',]$in_bushes_Humidity, na.rm=T)
weather2[14,'in_bushes_Humidity']  <- mean(weather2[weather2$hour=='06',]$in_bushes_Humidity, na.rm=T)
weather2[15,'in_bushes_Humidity']  <- mean(weather2[weather2$hour=='07',]$in_bushes_Humidity, na.rm=T)
weather2[16,'in_bushes_Humidity']  <- mean(weather2[weather2$hour=='08',]$in_bushes_Humidity, na.rm=T)

# Displaying NA cases
nrow(weather2[!complete.cases(weather2),])
nrow(weather2)

# Taking row difference of temp on 1-step, 2-steps, 3-steps..;
weather2$tempdiff1<-c(mean(weather2$temperature),diff(weather2$temperature))
weather2$tempdiff2<-c(mean(weather2$temperature),mean(weather2$temperature),diff(weather2$temperature,2))
weather2$tempdiff3<-c(mean(weather2$temperature),mean(weather2$temperature),mean(weather2$temperature),diff(weather2$temperature,3))
weather2$temp1<-c(mean(weather2$temperature), weather2$temperature[1:1214])

# Taking moving average of temperature
library(pracma)

weather2$ambtemp_movavg3 <- movavg(weather2$temperature, n = 3, type = "s")
weather2$ambtemp_movavg5 <- movavg(weather2$temperature, n = 5, type = "s")
weather2$ambtemp_movavg7 <- movavg(weather2$temperature, n = 7, type = "s")

ambtemp_movavg <- movavg(weather2$temperature, n = 7, type = "s")

# head(weather2, 5)

# plot(ambtemp_movavg)
# lines(ambtemp_movavg, type = "l", col = 2)

# Crearting copy of weather2 to do statistical modelling
setwd("D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav")
data1 <- weather2

row.names(data1)  <- seq(1, nrow(data1))

# head(data1)
# write.table(data1, file="D:/Himanshu/Acads/04. Mark Lawley/Dr. Madhav/WeatherMicrohabitat_datacombined.csv")

# Temperature plots for different environment

library(ggplot2)

p  <- ggplot() + 
geom_line(data=data1, aes(y=data1$temperature, x=as.numeric(rownames(data1)), colour="a")) + 
geom_line(data=data1, aes(y=data1$in_open_temp, x=as.numeric(rownames(data1)), colour="b")) + 
scale_color_discrete(name = "Temperatures", labels = c("ambient", "in_open")) + 
ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Ambient temp vs In_open temp") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = p, width = 12, height = 5.36, dpi = 300, filename="InopenTemp.png")

p <- ggplot() + 
geom_line(data=data1, aes(y=data1$temperature, x=as.numeric(rownames(data1)), colour="a")) + 
geom_line(data=data1, aes(y=data1$tree_coverage_temp, x=as.numeric(rownames(data1)), colour="b")) + 
scale_color_discrete(name = "Temperatures", labels = c("ambient", "tree coverage")) + 
ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Ambient temp vs tree_coverage temp") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = p, width = 12, height = 5.36, dpi = 300, filename="TreecovTemp.png")

p <- ggplot() + 
geom_line(data=data1, aes(y=data1$temperature, x=as.numeric(rownames(data1)), colour="a")) + 
geom_line(data=data1, aes(y=data1$storm_drain_temp, x=as.numeric(rownames(data1)), colour="b")) + 
scale_color_discrete(name = "Temperatures", labels = c("ambient", "storm drain")) + 
ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Ambient temp vs storm drain temp") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = p, width = 12, height = 5.36, dpi = 300, filename="Stromdrain.png")

# ggplot() + 
# geom_line(data=data1, aes(y=data1$temperature, x=as.numeric(rownames(data1)), colour="a")) + 
# geom_line(data=data1, aes(y=data1$in_bushes_temp, x=as.numeric(rownames(data1)), colour="b")) + 
# scale_color_discrete(name = "Temperatures", labels = c("ambient", "In bushes")) + 
# ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
# ggtitle(label = "Ambient temp vs In bushes temp") + theme(plot.title = element_text(hjust = 0.5))

# leaving puddle, and water meter as of now
# ggplot() + 
# geom_line(data=data1, aes(y=data1$temperature, x=as.numeric(rownames(data1)), colour="a")) + 
# geom_line(data=data1, aes(y=data1$puddle, x=as.numeric(rownames(data1)), colour="b")) + 
# scale_color_discrete(name = "Temperatures", labels = c("ambient", "puddle")) + 
# ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 10th Nov till 10th Jan") + 
# ggtitle(label = "Ambient temp vs puddle temp") + theme(plot.title = element_text(hjust = 0.5))

# ggplot() + 
# geom_line(data=data1, aes(y=data1$temperature, x=as.numeric(rownames(data1)), colour="a")) + 
# geom_line(data=data1, aes(y=data1$water_meter, x=as.numeric(rownames(data1)), colour="b")) + 
# scale_color_discrete(name = "Temperatures", labels = c("ambient", "water meter")) + 
# ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 10th Nov till 10th Jan") + 
# ggtitle(label = "Ambient temp vs water meter temp") + theme(plot.title = element_text(hjust = 0.5))

# Humidity plots for different environment

library(ggplot2)

p <- ggplot() + 
geom_line(data=data1, aes(y=data1$humidity, x=as.numeric(rownames(data1)), colour="a")) + 
geom_line(data=data1, aes(y=data1$in_open_Humidity, x=as.numeric(rownames(data1)), colour="b")) + 
scale_color_discrete(name = "Humidity", labels = c("ambient", "in_open")) + 
ylab(label="Humidity scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Ambient Humidity vs In_open Humidity") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = p, width = 12, height = 5.36, dpi = 300, filename="InopenHumidity.png")


ggplot() + 
geom_line(data=data1, aes(y=data1$humidity, x=as.numeric(rownames(data1)), colour="a")) + 
geom_line(data=data1, aes(y=data1$tree_coverage_Humidity, x=as.numeric(rownames(data1)), colour="b")) + 
scale_color_discrete(name = "Humidity", labels = c("ambient", "tree_coverage")) + 
ylab(label="Humidity scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Ambient Humidity vs tree_coverage Humidity") + theme(plot.title = element_text(hjust = 0.5))

p <- ggplot() + 
geom_line(data=data1, aes(y=data1$humidity, x=as.numeric(rownames(data1)), colour="a")) + 
geom_line(data=data1, aes(y=data1$storm_drain_Humidity, x=as.numeric(rownames(data1)), colour="b")) + 
scale_color_discrete(name = "Humidity", labels = c("ambient", "storm_drain")) + 
ylab(label="Humidity scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Ambient Humidity vs storm_drain Humidity") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = p, width = 12, height = 5.36, dpi = 300, filename="StormDrainHumidity.png")

# ggplot() + 
# geom_line(data=data1, aes(y=data1$humidity, x=as.numeric(rownames(data1)), colour="a")) + 
# geom_line(data=data1, aes(y=data1$in_bushes_Humidity, x=as.numeric(rownames(data1)), colour="b")) + 
# scale_color_discrete(name = "Humidity", labels = c("ambient", "in_bushes")) + 
# ylab(label="Humidity scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
# ggtitle(label = "Ambient Humidity vs in_bushes Humidity") + theme(plot.title = element_text(hjust = 0.5))


# lux plots for different environment

library(ggplot2)

# ggplot() + 
# geom_line(data=data1, aes(y=data1$in_open_lux, x=as.numeric(rownames(data1)), colour="b")) + 
# ylab(label="lux scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
# ggtitle(label = "in open lux ") + theme(plot.title = element_text(hjust = 0.5))

mydata  <- data1
mydata$Wind.direction <- as.numeric(as.character(mydata$Wind.direction))

# str(mydata)

table(mydata$Wind.direction)
table(data1$Wind.direction)

sum(is.na(mydata$Wind.direction)) # 23 values named as VRB converted to NA in conversion

mydata[is.na(mydata$Wind.direction),]$Wind.direction  <- mean(mydata$Wind.direction, na.rm=T)

mydata[21,]

# Plotting through R plot command 
# plot(mydata$temperature,type ="l", col=2)
# legend(x = 5,y = 5 , c("weather temp","In_open temp"),col=c(2,4),cex = 0.60,lty = c(1,1))
# lines(mydata$in_open,type = "l",col=4)
# plot(mydata$temperature,mydata$in_open_temp,col=3)

colnames(mydata) 

colnames(mydata)[2]  <- "amb_temp"
colnames(mydata)[27]  <- "amb_tempdiff1"
colnames(mydata)[28] <- "amb_tempdiff2"
colnames(mydata)[29] <- "amb_tempdiff3"
colnames(mydata)[30] <- "amb_templag1"

fit_storm_drain  <- lm(storm_drain_temp ~amb_temp+hour
                       +storm_drain_lux+amb_tempdiff2
                       +ambtemp_movavg7+Wind.speed
                       +Dew.point.temp+Wet.bulb.temp+Visibility
                       +Sea.level.pressure+Station.pressure, data=mydata)
summary(fit_storm_drain)
par(mfrow=c(2, 2))
plot(fit_storm_drain)



fit_storm_drain1  <- lm((storm_drain_temp) ~ (amb_temp
                       +storm_drain_lux+amb_tempdiff2
                       +ambtemp_movavg7+Wind.speed
                       +Dew.point.temp+Wet.bulb.temp+Visibility
                       +Sea.level.pressure+Station.pressure)^2 + hour + poly(amb_temp,2)
                       +poly(storm_drain_lux, 2)+ poly(amb_tempdiff2,2)
                       +poly(ambtemp_movavg7,2) + poly(Wind.speed, 2)
                       +poly(Dew.point.temp, 2)+ poly(Wet.bulb.temp, 2)+ poly(Visibility, 2)
                       +poly(Sea.level.pressure, 2)+ poly(Station.pressure, 2) 
                        , data=mydata)

summary(fit_storm_drain1)
par(mfrow=c(2, 2))
plot(fit_storm_drain1)

## Not able to change residual variance chart using response variable. 


# Significant interaction terms:
# amb_temp:ambtemp_movavg7, storm_drain_lux:Wind.speed, storm_drain_lux:Visibility, amb_tempdiff2:Wind.speed, ambtemp_movavg7:Visibility,
# ambtemp_movavg7:Wind.speed, Wind.speed:Visibility

fit_storm_drain2  <- lm((storm_drain_temp) ~ poly(amb_temp,2)
                       +poly(storm_drain_lux, 2)+ poly(amb_tempdiff2,2)
                       +poly(ambtemp_movavg7,2) + poly(Wind.speed, 2)
                       +poly(Dew.point.temp, 2)+ poly(Wet.bulb.temp, 2)+ poly(Visibility, 2)
                       +poly(Sea.level.pressure, 2)+ poly(Station.pressure, 2) + hour
                       +storm_drain_lux:Visibility
                        +ambtemp_movavg7:Visibility
                        , data=mydata)

summary(fit_storm_drain2)
par(mfrow=c(2, 2))
plot(fit_storm_drain2)

library(car)

vif(mod = fit_storm_drain)
vif(mod = fit_storm_drain2)

library(lmtest)

# Null hypothesis is that there is homoskedasticity (no variance in error)
bptest(fit_storm_drain1)
# P < 0.05 so null is rejected. it means there is variance in error.

fit_storm_drain2  <- lm((storm_drain_temp)~hour
                       +storm_drain_lux+amb_tempdiff2
                       +(ambtemp_movavg7)+Wind.speed
                       +Visibility
                        +amb_temp:Sea.level.pressure
                        +Dew.point.temp:ambtemp_movavg7
                       , data=mydata)
summary(fit_storm_drain2)
par(mfrow=c(2, 2))
plot(fit_storm_drain2)

# STORM DRAIN
par(mfrow=c(1,2))
plot(mydata$temperature,type ="l",col=2,ylim = c(-10,max(temperature)))
lines(mydata$strom_drain,type = "l",col=4)
legend(x = 5,y = 0 , c("weather temp","storm_drain"),col=c(2,4),cex = 0.60,lty = c(1,1))
plot(mydata$temperature,mydata$strom_drain,col=3)

pred_stormdrain_Reg<-predict(fit_storm_drain,newdata=mydata)
# par(mfrow=c(1,1))
# plot(mydata$storm_drain_temp, type="l", col=3, main="storm drain vs fitted values")
# legend(x=5, y=13, c("storm drain", "predicted storm drain temp"), col=c(3, 2), lty=c(1, 1))
# lines(pred_stormdrain_Reg, col=2)

df  <- data.frame(col1=seq(1, length(pred_stormdrain_Reg)), col2=pred_stormdrain_Reg)
               
p <- ggplot() + 
geom_line(data = df, aes(y=df$col2, x=df$col1, colour="a")) +
geom_line(data=mydata, aes(y=mydata$storm_drain_temp, x=as.numeric(rownames(mydata)), colour="b")) +
scale_color_discrete(name = "Temperature", labels = c("regression fit","Sensor")) +  
ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Storm drain sensor temp vs Regression fit temp") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = p, width = 12, height = 5.36, dpi = 300, filename="StormDrainRegressionfit.png")



#install.packages("tree", "C:/Users/Himanshu/Anaconda3/R/library", repos = "http://cran.us.r-project.org")
library(tree)
par(mfrow=c(1,1))
tree.storm_drain<-tree(storm_drain_temp~amb_temp+hour
                       +storm_drain_lux+amb_tempdiff2
                       +ambtemp_movavg7+Wind.speed
                       +Dew.point.temp+Wet.bulb.temp+Visibility
                       +Sea.level.pressure+Station.pressure,mydata)
summary(tree.storm_drain)
# plot(tree.storm_drain)
# text(tree.storm_drain, pretty=0)

# Reduced residual mean deviance to 1.76 from 2.614 after adding other variables

pred_stormdrain_tree=predict(tree.storm_drain,newdata = mydata)
par(mfrow=c(2,1))
plot(mydata$storm_drain_temp,type ="l",col=3,main="storm_drain temp and regression fit",ylim = c(12,max(mydata$amb_temp)-3))
legend(x = 500,y = 24 , c("storm_drain","Fited value_reg"),col=c(3,2),cex = 0.60,lty = c(1,1))
lines(pred_stormdrain_Reg,type = "l",col=2)

plot(mydata$storm_drain_temp,type ="l",col=3,main="storm_drain temp and tree fit",ylim = c(12,max(mydata$amb_temp)-3))
legend(x = 500,y = 24, c("storm_drain","Fited value_tree"),col=c(3,6),cex = 0.60,lty = c(1,1))
lines(pred_stormdrain_tree,type = "l",col=6)

# Calculating MSE
# for regression
mean((mydata$storm_drain_temp - pred_stormdrain_Reg)^2)# 1.79212

# for tree
mean((mydata$storm_drain_temp - pred_stormdrain_tree)^2)# 1.7223

# less MSE in tree model

library(e1071)

# col  <- c('storm_drain_temp', 'hour', 'storm_drain_lux', 'Visibility', 'ambtemp_movavg7', 'amb_tempdiff2', 'Wind.speed')
# mydata1  <- mydata[, col]
# tuned <- tune.svm(storm_drain_temp~amb_temp+hour
#                        +storm_drain_lux+amb_tempdiff2
#                        +ambtemp_movavg7+Wind.speed
#                        +Dew.point.temp+Wet.bulb.temp+Visibility
#                        +Sea.level.pressure+Station.pressure
#                        , data=mydata, kernel="radial", cost=10^(-2:2), gamma=10^(-2:2))

# tuned$best.parameters
# # gamma = 0.1, cost =10 (for radial kernel)

svmfit  <- svm(storm_drain_temp~amb_temp+hour
                       +storm_drain_lux+amb_tempdiff2
                       +ambtemp_movavg7+Wind.speed
                       +Dew.point.temp+Wet.bulb.temp+Visibility
                       +Sea.level.pressure+Station.pressure
                       , data=mydata, kernel="radial", cost=10, gamma=1.75)
# print(svmfit) # 1071 support vectors

pred  <- predict(svmfit, mydata)
# Calculating MSE
# for SVM
mean((mydata$storm_drain_temp - pred)^2) # 0.0403

#tune function gives different parameters for cost, gamma but error is lowest on cost=10, gamma = 1.75

df  <- data.frame(col1=seq(1, length(pred)), col2=pred)
               
ggplot() + 
geom_line(data = df, aes(y=df$col2, x=df$col1, colour="a")) +
geom_line(data=mydata, aes(y=mydata$storm_drain_temp, x=as.numeric(rownames(mydata)), colour="b")) +
scale_color_discrete(name = "Temperature", labels = c("svm fit","Sensor")) +  
ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Storm drain sensor temp vs svm fit temp") + theme(plot.title = element_text(hjust = 0.5))



## what if i move the graph by certain index horizontally. 
# Found that MSE reaches to 0.40 if i shift the graph by 15 hours horizontally
# p1  <- c(rep(20,15), p)
# length(p1)

p <- as.data.frame(p)


library(caret)

fitControl <- trainControl(method = "cv", number = 4)

gbm1 <- train(storm_drain_temp~amb_temp
                       +storm_drain_lux+amb_tempdiff2
                       +ambtemp_movavg7+Wind.speed
                       +Dew.point.temp+Wet.bulb.temp+Visibility
                       +Sea.level.pressure+Station.pressure,  data = mydata, method = "gbm",
               trControl = fitControl)

summary(gbm1)
gbm1
# The final values used for the model were n.trees = 150, interaction.depth =
#  3, shrinkage = 0.1 and n.minobsinnode = 10.
# R-sq :0.6326111 , RMSE - 1.252903  


p <- predict(gbm1, mydata)

mean((mydata$storm_drain_temp - p)^2) # 1.10695508781303


df  <- data.frame(col1=seq(1, length(p)), col2=p)
               
ggplot() + 
geom_line(data = df, aes(y=df$col2, x=df$col1, colour="a")) +
geom_line(data=mydata, aes(y=mydata$storm_drain_temp, x=as.numeric(rownames(mydata)), colour="b")) +
scale_color_discrete(name = "Temperature", labels = c("gbm fit","Sensor")) +  
ylab(label="Temperature scale") + xlab("Index of date-hours, starting from 20th Nov till 10th Jan") + 
ggtitle(label = "Storm drain sensor temp vs gbm fit temp") + theme(plot.title = element_text(hjust = 0.5))


