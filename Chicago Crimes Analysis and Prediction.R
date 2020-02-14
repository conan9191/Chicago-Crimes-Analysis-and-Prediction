###### Group final project  ######
###### Group Member name    ######
######     Yiqun Peng       ######
######     Shen Fan         ######
######     Yufei Ding       ######
######   Jianshuo Yang      ######

rm(list=ls())

###### Load the file "Crime - 2019.csv"  ######
###### Date scraped from https://data.cityofchicago.org/Public-Safety/Crimes-2019/w98m-zvie ######
csvfile<-file.choose()
crime<-read.csv(csvfile)
#View(crime)
summary(crime)

###### Load the file "weather_2019.1.1-2019.11.20.csv"  ######
###### https://www.wunderground.com/calendar/us/il/chicago/KMDW/date/2019-1/DailyHistory.html?req_city=Chicago&req_state=IL&req_statename=Illinois&reqdb.zip=60290&reqdb.magic=1&reqdb.wmo=99999&format=1 ######     
csvfile2<-file.choose()
weather<-read.csv(csvfile2,na.string="", stringsAsFactors = FALSE)
#View(weather)
weather[is.na(weather)]<-'Sunny-Cloudy'
weather<-weather[which(weather$city=='chicago'),]


weather$CST<-as.Date(weather$CST)
#summary(weather$CST)
crime$Date<-as.Date(crime$Date, format = "%m/%d/%Y %H:%M:%S")
#summary(crime$Date)

#####  Combinating the crime data and weather data  #####
temp_weather<-matrix(data=NA, nrow = nrow(crime), ncol = 3, byrow = FALSE, dimnames = NULL)
for(i in 1:nrow(crime)){
  temp_weather[i,1]<-weather$MaxTempF[which(weather$CST-crime$Date[i]==0)]
  temp_weather[i,2]<-weather$MinTempF[which(weather$CST-crime$Date[i]==0)]
  temp_weather[i,3]<-weather$Events[which(weather$CST-crime$Date[i]==0)]
}
#View(temp_weather)
crime$MaxTempF<-temp_weather[,1]
crime$MinTempF<-temp_weather[,2]
crime$Events<-temp_weather[,3]
crime$MaxTempF<-as.factor(crime$MaxTempF)
crime$MinTempF<-as.factor(crime$MinTempF)
crime$Events<-as.factor(crime$Events)
#View(crime)
#summary(crime)

##### Export the processed data file #####
#write.table(crime,"~/Desktop/Combination.csv",row.names=FALSE,col.names=TRUE)
Combination<-crime

############################### Statistics ###########################################
###### Explore the crime rate in the year 2019 ######
###### Visualize the criminal activity by creating a map ######
Crimes<-Combination
Crimes<-na.omit(Crimes)
data<-Crimes[,c(-2,-4,-5,-(10:19))]
colnames(data)[3]<-"Category"
summary(data)
View(data)
library(leaflet)
library(tidyverse)
library(scales)
library(ggmap)
library(RColorBrewer)
library(DT)
library(ggrepel) #better label
data1 <- data[1:100000,]
data1$popup <- paste("<b>Incident #: </b>", data1$ID, "<br>", "<b>Category: </b>", data1$Category,
                     "<br>", "<b>Description: </b>", data1$Description,
                     "<br>", "<b>Location Description: </b>", data1$Location.Description,
                     "<br>", "<b>Date: </b>", data1$Date,
                     "<br>", "<b>Arrest: </b>", data1$Arrest,
                     "<br>", "<b>Longitude: </b>", data1$Longitude,
                     "<br>", "<b>Latitude: </b>", data1$Latitude,
                     "<br>", "<b>Events: </b>", data1$Events)
leaflet(data1, height = 600,width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = data1$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Summarize the data by incident category.
category <- sort(table(data$Category),decreasing = TRUE)
category <- data.frame(category[category > 300])
colnames(category) <- c("Category", "Frequency")
category$Percentage <- category$Frequency / sum(category$Frequency)
datatable(category, options = list(scrollX='400px'))

#Create a bar plot based on the incident category.
bp<-ggplot(category, aes(x=Category, y=Frequency, fill=Category)) + geom_bar(stat="identity") + 
  theme(axis.text.x=element_blank()) +geom_label_repel(aes(label = category$Frequency),size = 2.5) +
  coord_flip() +
  labs(title = "Top 20 Crimes Commited in 2019", 
       x = "Crime Description", 
       y = "Total")

#Crimes committed over time
DataConvert<-as.Date(crime$Date, format = "%m/%d/%Y %H:%M:%S")
crime$Month=months(DataConvert)
library(tidyverse)
month <- sort(table(crime$Month),decreasing = TRUE)
month<-data.frame(month)
p<-ggplot(data=month, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  labs(title = "Crimes Commited each month in 2019", 
       x = "Month", 
       y = "Total")
#group <- data %>%
#  group_by(`Category`) %>%
#  dplyr::summarise(total = n()) %>%
#  distinct() %>%
#  top_n(20)
#group %>%
#  ggplot(aes(reorder(`Category`, total), y = total)) +
# geom_col(fill = "red") +
# geom_label_repel(aes(label = total), size = 2.5) +
# coord_flip() +
#  labs(title = "Top 20 Crime Commited in 2019", 
#       x = "Crime Description", 
#      y = "Total")
#google map
register_google(key="AIzaSyA55FTAeZeowOKyoyHjNYY7abicYg87_1I")
map <- ggmap(get_googlemap(center = "Chicago",
                           zoom = 12, scale = 2,
                           maptype ='terrain',
                           color = 'color'))
#focus on theft
map_crime <- data %>%
  select(`Category`,Longitude,Latitude) %>%
  filter(`Category` == 'THEFT') %>%
  na.omit()

map+ geom_point(aes(x = Longitude, y = Latitude,colour = Category), data = map_crime, alpha=0.25, size = 0.5) + 
  theme(legend.position="Bootom")

#plot the all crimes density
map +
  geom_density_2d(data = data,
                  aes(x = Longitude,
                      y = Latitude)) +
  stat_density_2d(data = data,
                  aes(x = Longitude,
                      y = Latitude,
                      fill = stat(level)),
                  alpha = .2,
                  bins = 25,
                  geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))


################################ PREDICTION ###########################################
###### Date cleaning, Categorizing the seriousness by fbi_code, and normalization
#Categorize
fbi_code <- data.frame(
  FBI.Code=c("01A","01B","02","03","04A","04B","05","06","07","08A","08B","09","10","11","12","13","14","15","16","17","18","19","20","22","24","26"),       
  seriousness=c("More Serious","Less Serious","More Serious","More Serious","More Serious","More Serious",
                "More Serious","More Serious","More Serious","Less Serious","Less Serious","More Serious",
                "Less Serious","Less Serious","Less Serious","Less Serious","Less Serious","Less Serious",
                "Less Serious","Less Serious","Less Serious","Less Serious","Less Serious","Less Serious",
                "Less Serious","Less Serious")
)
crime = merge(crime, fbi_code, by="FBI.Code")
crime$serious = 0
crime$serious[crime$seriousness == "More Serious"] <- 1
crime$serious<-as.factor(crime$serious)

#### data cleaning ####
crime<-na.omit(crime)
#summary(crime)

##### Nomalization #####
mmnorm <-function(x)
{z<-((x-min(x))/(max(x)-min(x)))
return(z)                              
}


####### To save running time, select a subset of the dataset to process the training ######
crime_rawdata<-crime
#idx2<-sort(sample(nrow(crime),as.integer(.10*nrow(crime))))
#crime_rawdata<-crime[idx2,]

##### Method 1 Segmentation of different climate variables with a value of 1 or 0 by climate name #####
crime_data<- data.frame(   ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                          ,Beat=mmnorm(crime_rawdata$Beat)
                          ,District=mmnorm(crime_rawdata$District)
                          ,Ward=mmnorm(crime_rawdata$Ward)
                          ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                          ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                          ,Fog=as.integer(ifelse(crime_rawdata$Events=='Fog'|
                                                   crime_rawdata$Events=='Fog-Rain'|
                                                   crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                   crime_rawdata$Events=='Fog-Snow',1,0))
                          ,Rain=as.integer(ifelse(crime_rawdata$Events=='Rain'|
                                                    crime_rawdata$Events=='Fog-Rain'|
                                                    crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                    crime_rawdata$Events=='Rain-Hail-Thunderstorm'|
                                                    crime_rawdata$Events=='Rain-Snow'|
                                                    crime_rawdata$Events=='Rain-Thunderstorm',1,0))
                          ,Snow=as.integer(ifelse(crime_rawdata$Events=='Snow'|
                                                    crime_rawdata$Events=='Fog-Snow'|
                                                    crime_rawdata$Events=='Rain-Snow'|
                                                    crime_rawdata$Events=='Rain-Snow',1,0))
                          ,Thunderstorm=as.integer(ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                            crime_rawdata$Events=='Rain-Hail-Thunderstorm'|
                                                            crime_rawdata$Events=='Rain-Thunderstorm',1,0))
                          ,SunnyCloudy=as.integer(ifelse(crime_rawdata$Events=='Sunny-Cloudy',1,0))
                          ,serious=as.integer(ifelse(crime_rawdata$seriousness=='More Serious',1,0))
)

##### Method 2 Dividing variable Events into ten levels #####
crime_data2<- data.frame( ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                         ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                         ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                         ,Events=(
                            ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm',1,
                            ifelse(crime_rawdata$Events=='Rain-Hail-Thunderstorm',0.9,
                            ifelse(crime_rawdata$Events=='Fog-Rain',0.8,
                            ifelse(crime_rawdata$Events=='Fog-Snow',0.7,
                            ifelse(crime_rawdata$Events=='Rain-Snow',0.6,
                            ifelse(crime_rawdata$Events=='Rain-Thunderstorm',0.5,
                            ifelse(crime_rawdata$Events=='Fog',0.4,
                            ifelse(crime_rawdata$Events=='Rain',0.3,
                            ifelse(crime_rawdata$Events=='Snow',0.2,
                            ifelse(crime_rawdata$Events==' Sunny-Cloudy',0.1,0)))))))))))
)

###### Using naiveBayes to predict severity of crime by training date, district, location and arrest, using 20% test 80% training data.  ######
idx<-sort(sample(nrow(crime),as.integer(.80*nrow(crime))))
training<-crime[idx,c(4,7,9,10,13,27)]
test<-crime[-idx,c(4,9,10,27)]
library(e1071)
crime$District<-as.numeric(crime$District)
nBayes <- naiveBayes(training$serious~.,training) 
#Prediction
predict<-predict(nBayes,test)
#Score the test dataset
table<-table(Actual=test$serious,predict)
table
sum(diag(table))/sum(table)
#Measure the error rate
wrong<-(test$serious!=predict)
rate<-sum(wrong)/length(wrong)
rate

###### Using naiveBayes to predict arrest of crime by training date, district, location and Severity, using 20% test 80% training data.  ######
idx<-sort(sample(nrow(crime),as.integer(.80*nrow(crime))))
training<-crime[idx,c(9,10,23,24,25,27)]
test<-crime[-idx,c(9,10,23,24,25,27)]
nBayes_arrest <- naiveBayes(training$Arrest~.,training) 
#Prediction
predict_arrest<-predict(nBayes_arrest,test)
#Score the test dataset
table_predict_arrest<-table(Actual=test$Arrest,predict_arrest)
table_predict_arrest
sum(diag(table_predict_arrest))/sum(table_predict_arrest)
#Measure the error rate
wrong_predict_arrest<-(test$Arrest!=predict_arrest)
rate_predict_arrest<-sum(wrong_predict_arrest)/length(wrong_predict_arrest)
rate_predict_arrest

###### Using KNN (1 to 100) to predict arrest of crime by training date, arrest, location, using 20% test 80% training data.  ######
### 50 min to run
idx<-sort(sample(nrow(crime),as.integer(.80*nrow(crime))))
training<-crime[idx,c(4,9,10,27)]
test<-crime[-idx,c(4,9,10,27)]
library(kknn)
### taking k=1 and k=5 as examples
index<-seq(0,5,by=5)
index[1]<-1
knn_table<-matrix(data=NA, nrow = 21, ncol = 3, byrow = FALSE, dimnames = NULL)
count<-1
for(i in c(1,index)){
predict<-kknn(training$Arrest~., training, test, k=i,kernel ="rectangular"  )
fit<-fitted(predict)
wrong<-(test$Arrest!=fit)
rate<-sum(wrong)/length(wrong)
rate
knn_table[count,1]<-i
knn_table[count,2]<-sum(wrong)
knn_table[count,3]<-rate
count<-count+1
} 
knn_table

#predict_k100<-kknn(training$Arrest~., training, test, k=100,kernel ="rectangular"  )
#fit100<-fitted(predict_k100)
#wrong100<-(test$Arrest!=fit100)
#rate100<-sum(wrong100)/length(wrong100)
#rate100


####### Using neuralnet topredict arrest by nomalizing district,serious and weather. #######

####### To save running time, select a subset of the dataset to process the training ######
### 30 min to run
idx2<-sort(sample(nrow(crime),as.integer(.10*nrow(crime))))
crime_rawdata<-crime[idx2,]

##### Method 1 Segmentation of different climate variables with a value of 1 or 0 by climate name #####
crime_data<- data.frame(   ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                           ,Beat=mmnorm(crime_rawdata$Beat)
                           ,District=mmnorm(crime_rawdata$District)
                           ,Ward=mmnorm(crime_rawdata$Ward)
                           ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                           ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                           ,Fog=as.integer(ifelse(crime_rawdata$Events=='Fog'|
                                                    crime_rawdata$Events=='Fog-Rain'|
                                                    crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                    crime_rawdata$Events=='Fog-Snow',1,0))
                           ,Rain=as.integer(ifelse(crime_rawdata$Events=='Rain'|
                                                     crime_rawdata$Events=='Fog-Rain'|
                                                     crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                     crime_rawdata$Events=='Rain-Hail-Thunderstorm'|
                                                     crime_rawdata$Events=='Rain-Snow'|
                                                     crime_rawdata$Events=='Rain-Thunderstorm',1,0))
                           ,Snow=as.integer(ifelse(crime_rawdata$Events=='Snow'|
                                                     crime_rawdata$Events=='Fog-Snow'|
                                                     crime_rawdata$Events=='Rain-Snow'|
                                                     crime_rawdata$Events=='Rain-Snow',1,0))
                           ,Thunderstorm=as.integer(ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                             crime_rawdata$Events=='Rain-Hail-Thunderstorm'|
                                                             crime_rawdata$Events=='Rain-Thunderstorm',1,0))
                           ,SunnyCloudy=as.integer(ifelse(crime_rawdata$Events=='Sunny-Cloudy',1,0))
                           ,serious=as.integer(ifelse(crime_rawdata$seriousness=='More Serious',1,0))
)

library("neuralnet")
idx3<-sort(sample(nrow(crime_data),as.integer(.80*nrow(crime_data))))
training<-crime_data[idx3,]
test<-crime_data[-idx3,]
#View(training)
net_type<-neuralnet(ArrestData~.,training, hidden=5, threshold=0.05,lifesign='full',linear.output=TRUE,stepmax=1e7)
plot(net_type)
#dev.off()

net_results <-compute(net_type, test)
ANN1=as.numeric(net_results$net.result)
ANN_cat1<-ifelse(ANN1<.5,0,1)
table(Actual=test$ArrestData,Predict=ANN_cat1)

wrong1<-(test$ArrestData!=ANN_cat1)
rate1<-sum(wrong1)/length(wrong1)
rate1 

par(mfrow=c(2,4))
gwplot(net_type,max =4, min = -4, selected.covariate = "MaxTempF")
gwplot(net_type,max =4, min = -4, selected.covariate = "MinTempF")
gwplot(net_type,max =4, min = -4, selected.covariate = "Fog")
gwplot(net_type,max =4, min = -4, selected.covariate = "Rain")
gwplot(net_type,max =4, min = -4, selected.covariate = "Snow")
gwplot(net_type,max =4, min = -4, selected.covariate = "Thunderstorm")
gwplot(net_type,max =4, min = -4, selected.covariate = "SunnyCloudy")


##### Method 2 Dividing variable Events into ten levels #####
####### To save running time, select a subset of the dataset to process the training ######
### 30 min to run
idx2<-sort(sample(nrow(crime),as.integer(.10*nrow(crime))))
crime_rawdata<-crime[idx2,]
crime_data2<- data.frame( ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                          ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                          ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                          ,Events=(
                           ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm',1,
                           ifelse(crime_rawdata$Events=='Rain-Hail-Thunderstorm',0.9,
                           ifelse(crime_rawdata$Events=='Fog-Rain',0.8,
                           ifelse(crime_rawdata$Events=='Fog-Snow',0.7,
                           ifelse(crime_rawdata$Events=='Rain-Snow',0.6,
                           ifelse(crime_rawdata$Events=='Rain-Thunderstorm',0.5,
                           ifelse(crime_rawdata$Events=='Fog',0.4,
                           ifelse(crime_rawdata$Events=='Rain',0.3,
                           ifelse(crime_rawdata$Events=='Snow',0.2,
                           ifelse(crime_rawdata$Events==' Sunny-Cloudy',0.1,0)))))))))))
)

library("neuralnet")
idx3<-sort(sample(nrow(crime_data2),as.integer(.80*nrow(crime_data2))))
training<-crime_data2[idx3,]
test<-crime_data2[-idx3,]
#View(training)
net_type2<-neuralnet(ArrestData~.,training, hidden=5, threshold=0.05,lifesign='full',linear.output=TRUE,stepmax=1e7)
plot(net_type)
#dev.off()

net_results2 <-compute(net_type2, test)
ANN2=as.numeric(net_results2$net.result)
ANN_cat2<-ifelse(ANN2<.5,0,1)
table(Actual=test$ArrestData,Predict=ANN_cat2)

wrong2<-(test$ArrestData!=ANN_cat2)
rate2<-sum(wrong1)/length(wrong2)
rate2

par(mfrow=c(2,2))
gwplot(net_type2,max =4, min = -4, selected.covariate = "MaxTempF")
gwplot(net_type2,max =4, min = -4, selected.covariate = "MinTempF")
gwplot(net_type2,max =4, min = -4, selected.covariate = "Events")


####### Using neuralnet to predict severity by nomalizing arrest, temperature and weather. #######

####### To save running time, select a subset of the dataset to process the training ######
### 30 min to run
idx2<-sort(sample(nrow(crime),as.integer(.10*nrow(crime))))
crime_rawdata<-crime[idx2,]

crime_data3<- data.frame( ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                          ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                          ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                          ,Events=(
                           ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm',1,
                           ifelse(crime_rawdata$Events=='Rain-Hail-Thunderstorm',0.9,
                           ifelse(crime_rawdata$Events=='Fog-Rain',0.8,
                           ifelse(crime_rawdata$Events=='Fog-Snow',0.7,
                           ifelse(crime_rawdata$Events=='Rain-Snow',0.6,
                           ifelse(crime_rawdata$Events=='Rain-Thunderstorm',0.5,
                           ifelse(crime_rawdata$Events=='Fog',0.4,
                           ifelse(crime_rawdata$Events=='Rain',0.3,
                           ifelse(crime_rawdata$Events=='Snow',0.2,
                           ifelse(crime_rawdata$Events=='Sunny-Cloudy',0.1,0)))))))))))
                           ,serious=as.numeric(ifelse(crime_rawdata$seriousness=='More Serious',1,0))
)

idx3<-sort(sample(nrow(crime_data3),as.integer(.80*nrow(crime_data3))))
training<-crime_data3[idx3,]
test<-crime_data3[-idx3,]

net_type3<-neuralnet(serious~.,training, hidden=5, threshold=0.05,lifesign='full',linear.output=TRUE,stepmax=1e7)
plot(net_type3)
#dev.off()

net_results3 <-compute(net_type3, test)
ANN3=as.numeric(net_results3$net.result)
ANN_cat3<-ifelse(ANN1<.5,0,1)
table(Actual=test$ArrestData,Predict=ANN_cat3)

wrong3<-(test$ArrestData!=ANN_cat3)
rate3<-sum(wrong1)/length(wrong3)
rate3

par(mfrow=c(2,2))
gwplot(net_type3,max =4, min = -4, selected.covariate = "ArrestData")
gwplot(net_type3,max =4, min = -4, selected.covariate = "MaxTempF")
gwplot(net_type3,max =4, min = -4, selected.covariate = "MinTempF")
gwplot(net_type3,max =4, min = -4, selected.covariate = "Events")


###### Predicting Primary Type using Random Forest by nomalizing district,serious and weather ######
library(randomForest)
set.seed(123)

####### To save running time, select a subset of the dataset to process the training ######
idx2<-sort(sample(nrow(crime),as.integer(.10*nrow(crime))))
crime_rawdata<-crime[idx2,]

# Method 1
crime_data<- data.frame(   ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                           ,Beat=mmnorm(crime_rawdata$Beat)
                           ,District=mmnorm(crime_rawdata$District)
                           ,Ward=mmnorm(crime_rawdata$Ward)
                           ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                           ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                           ,Fog=as.integer(ifelse(crime_rawdata$Events=='Fog'|
                                                    crime_rawdata$Events=='Fog-Rain'|
                                                    crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                    crime_rawdata$Events=='Fog-Snow',1,0))
                           ,Rain=as.integer(ifelse(crime_rawdata$Events=='Rain'|
                                                     crime_rawdata$Events=='Fog-Rain'|
                                                     crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                     crime_rawdata$Events=='Rain-Hail-Thunderstorm'|
                                                     crime_rawdata$Events=='Rain-Snow'|
                                                     crime_rawdata$Events=='Rain-Thunderstorm',1,0))
                           ,Snow=as.integer(ifelse(crime_rawdata$Events=='Snow'|
                                                     crime_rawdata$Events=='Fog-Snow'|
                                                     crime_rawdata$Events=='Rain-Snow'|
                                                     crime_rawdata$Events=='Rain-Snow',1,0))
                           ,Thunderstorm=as.integer(ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm'|
                                                             crime_rawdata$Events=='Rain-Hail-Thunderstorm'|
                                                             crime_rawdata$Events=='Rain-Thunderstorm',1,0))
                           ,SunnyCloudy=as.integer(ifelse(crime_rawdata$Events=='Sunny-Cloudy',1,0))
                           ,serious=as.integer(ifelse(crime_rawdata$seriousness=='More Serious',1,0))
)

crime_data$ArrestData<- as.factor(crime_data$ArrestData)

idx3<-sort(sample(nrow(crime_data),as.integer(.80*nrow(crime_data))))
training<-crime_data[idx3,]
test<-crime_data[-idx3,]

fit<-randomForest(ArrestData~.,data = training,importance=TRUE,ntree=1000)
plot(fit)
importance(fit)
varImpPlot(fit)
#dev.off()

predict <- predict(fit, test)

table(Predict=predict,Actual=test$ArrestData)
wrong<-(test$ArrestData!=predict)
rate<-sum(wrong)/length(wrong)
rate

# Method2
idx2<-sort(sample(nrow(crime),as.integer(.10*nrow(crime))))
crime_rawdata<-crime[idx2,]
crime_data2<- data.frame( ArrestData=as.integer(ifelse(crime_rawdata$Arrest=='true',1,0))  
                          ,MaxTempF=mmnorm(as.integer(crime_rawdata$MaxTempF))
                          ,MinTempF=mmnorm(as.integer(crime_rawdata$MinTempF))
                          ,Events=(
                           ifelse(crime_rawdata$Events=='Fog-Rain-Thunderstorm',1,
                           ifelse(crime_rawdata$Events=='Rain-Hail-Thunderstorm',0.9,
                           ifelse(crime_rawdata$Events=='Fog-Rain',0.8,
                           ifelse(crime_rawdata$Events=='Fog-Snow',0.7,
                           ifelse(crime_rawdata$Events=='Rain-Snow',0.6,
                           ifelse(crime_rawdata$Events=='Rain-Thunderstorm',0.5,
                           ifelse(crime_rawdata$Events=='Fog',0.4,
                           ifelse(crime_rawdata$Events=='Rain',0.3,
                           ifelse(crime_rawdata$Events=='Snow',0.2,
                           ifelse(crime_rawdata$Events==' Sunny-Cloudy',0.1,0)))))))))))
                          ,serious=as.numeric(ifelse(crime_rawdata$seriousness=='More Serious',1,0))
)

crime_data2$ArrestData<- as.factor(crime_data2$ArrestData)

idx3<-sort(sample(nrow(crime_data),as.integer(.80*nrow(crime_data))))
training<-crime_data2[idx3,]
test<-crime_data2[-idx3,]

# Change the ntree to 100
fit2<-randomForest(ArrestData~.,data = training,importance=TRUE,ntree=100)
plot(fit2)
importance(fit2)
varImpPlot(fit2)
#dev.off()

predict2 <- predict(fit2, test)

table(Predict=predict,Actual=test$ArrestData)
wrong2<-(test$ArrestData!=predict2)
rate2<-sum(wrong2)/length(wrong2)
rate2

