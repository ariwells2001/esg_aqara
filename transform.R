library(tidyverse)

library(scales)
library(DMwR2)
library(performanceEstimation)
library(class)


df = read.csv("pre.csv")
head(df)


df <- df %>% 
  mutate (timestamp = timestamp/1000) %>% 
  mutate(log = as.POSIXct(timestamp,origin="1970-01-01"))


dff <- subset(df,select=-c(timestamp,log,temperature,humidity,illuminance))
str(dff)
table(dff$existence)
dff$existence <- as.factor(dff$existence)
df_final <- smote(existence ~ ., data=dff,perc.over=20,k=5)
table(df_final$existence)





result <- glm(existence ~ .,family = binomial,data=dff)
summary(result)
plot(result)
str(dff)
a <- predict(result, newdata=dff,type='response')

summary(a)
head(a)
a <-  data.frame(a)
table(a)
tail(a,500)
str(dff)
idx <- which(a< 0.5)
tail(dff$existence,500)
a[idx,]
dff[idx,c('existence')]
table(dff$existence)
