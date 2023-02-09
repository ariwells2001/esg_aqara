library(tidyverse)
library(readxl)

read_excel_allsheets <- function(filename){
  sheets <- excel_sheets(filename)
  x <- lapply(sheets,function(x) read_xlsx(filename,sheet=x))
  x <- lapply(x,as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets('20230206.xlsx')

str(mysheets)
head(mysheets,2)
view(mysheets)
str(mysheets)
length(mysheets$"재실거리"$"로그값")
summary(mysheets)
head(mysheets$재실거리$로그값)
plot(mysheets$조도센서$로그값)
plot(mysheets$재실상태$로그값,mysheets$조도센서$로그값)
