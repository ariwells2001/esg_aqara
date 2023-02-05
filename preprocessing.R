if (require('tidyverse')==F) install.packages(tidyverse)
library(tidyverse)
library(caret)

data <- read.csv(file='myiot.csv',sep=',')
names(data) <- c('id','code','created_at','updated_at','email',
                 'device_name','value','subject_id','timestamp','resource_id',
                 "device_id",'blank1',"log_time","blank2")

data <- data[,!names(data) %in% c("id","code","blank1","blank2")]

data$value <- ifelse((data$device_name == "온도센서" | 
                        data$device_name=="습도센서"),
                     substr(data$value,12,16), 
                     ifelse(data$device_name =="스마트큐브", 1, substr(data$value,14,14))
                     )
data$value <- as.numeric(data$value)


subjectIdSplit <- function(x){
  value = substr(x,18,37)
}

timestampSplit <- function(x){
  value = substr(x,15,28)
}

resourceIdSplit <- function(x){
  value =substr(x,19,24)
}
data['subject_id'] = sapply(data$subject_id,subjectIdSplit)
head(data['subject_id'])
data['timestamp'] = sapply(data$timestamp,timestampSplit)
head(data['timestamp'])
data['resource_id'] = sapply(data$resource_id,resourceIdSplit)
head(data['resource_id'])

nameConvert <- function(x){
  if (x == "모션센서"){
    new_name = "motion"
  } else if (x == "스마트조명 상단") {
    new_name = "lighting_upper"
  } else if (x == "스마트조명 하단") {
    new_name = "lighting_lower"
  } else if (x == "스마트큐브") {
    new_name = "cube"
  } else if (x == "습도센서") {
    new_name = "humidity"
  } else if (x == "열림감지센서") {
    new_name = "door"
  } else if (x == "온도센서") {
    new_name = "temperature"
  } else if (x == "재실존재") {
    new_name = "existence"
  } else if (x == "전동블라인드") {
    new_name = "rollershade"
  } else if (x == "조도센서") {
    new_name = "illuminance"
  } else if (x == "플러그") {
    new_name = "plug"
  }
}

data['device_name'] = sapply(data$device_name,nameConvert)

deviceName <- data.frame(data$timestamp,data$device_name,data$value)
head(deviceName)


dmy <- dummyVars(~ data.device_name,data=deviceName)
devices <-  data.frame(predict(dmy,newdata=deviceName))
str(devices)
df <- cbind(data,devices)
str(df)
colnames(df)
colnames(df)[11:21] <- c('cube','door','existence','humidity','illuminance','lighting_lower',
                         'lighting_upper','motion','plug','rollershade','temperature')
colnames(df)
df$lighting_upper


makeNA <- function(x){
  ifelse(x == 0, NA, x)
}



df$cube <- sapply(df$cube,makeNA)
df$door <- sapply(df$door,makeNA)
df$existence <- sapply(df$existence,makeNA)
df$humidity <- sapply(df$humidity,makeNA)
df$illuminance <- sapply(df$illuminance,makeNA)
df$lighting_lower <- sapply(df$lighting_lower,makeNA)
df$lighting_upper <- sapply(df$lighting_upper,makeNA)
df$motion <- sapply(df$motion,makeNA)
df$plug <- sapply(df$plug,makeNA)
df$rollershade <- sapply(df$rollershade,makeNA)
df$temperature <- sapply(df$temperature,makeNA)

df$cube <- df$cube * df$value
df$door <- df$door * df$value
df$existence <- df$existence * df$value
df$humidity <- df$humidity * df$value
df$illuminance <- df$illuminance * df$value
df$lighting_lower <- df$lighting_lower * df$value
df$lighting_upper <- df$lighting_upper * df$value
df$motion <- df$motion * df$value
df$plug <- df$plug * df$value
df$rollershade <- df$rollershade * df$value
df$temperature <- df$temperature * df$value

df <- df %>% select(-c('value','device_name','cube'))

deviceList <- c("door","existence","humidity",
               "illuminance","lighting_lower","lighting_upper",
                "motion","plug","rollershade","temperature","door")
dl <- c("door")
loop <-  seq(1:nrow(df))


for (item in deviceList){
  for (notNullLocation in loop){
    temp <- is.na(df[[item]][notNullLocation])
   
  
    if (temp == FALSE){
      print(df[[item]])
      if (df[[item]][notNullLocation] == 0){
        df[[item]][notNullLocation] <- 1
      
      } else if 
      (df[[item]][notNullLocation]==1){
        df[[item]][1] <-  0
       
        
      } else {
        df[[item]][1] <-  df[[item]][notNullLocation] + 1
      
      }
      break  
    } 
  }
}

head(df,1)

# Fill NAs

for (dn in deviceList){
  df <- fill(df,dn,.direction="down")
}

write.csv(df,"preprocessed.csv",row.names = FALSE)
str(df)
timestamp <- data.frame(as.numeric(df$timestamp))
colnames(timestamp) <- c("timestamp")
head(timestamp)
df <- df %>% select(c(deviceList))
head(df)                    
final_df <- cbind(timestamp,df)
write.csv(final_df,"pre.csv",row.names = FALSE)
