#rm(list = ls())
#library(tidyverse)
#library(lubridate)
#testscsv <- read_csv("Tests Done.csv")
#testcsv <- testscsv %>% 
#  mutate(Date = mdy(Date))
#testcsv %>% 
#  mutate(New_tests = `No. Of Tests` - lag(`No. Of Tests`)) %>% 
#  view()
#install.packages("filesstrings")

rm(list=ls())
library(filesstrings)
library(rvest)
library(tidyverse)
library(lubridate)
ncdc_url <- "https://covid19.ncdc.gov.ng/"
ngr_covid_page<- read_html(ncdc_url)

tables_on_page <- ngr_covid_page%>%
  html_nodes("table")%>%
  html_table(fill=T)
tables_on_page

ngr_covid_cases <- tables_on_page[[1]]
  as_tibble(ngr_covid_cases) 
ngr_covid_cases <-  ngr_covid_cases %>%   
  mutate(`No. of Cases (Lab Confirmed)`=as.double(gsub(",","",`No. of Cases (Lab Confirmed)`)),
         `No. of Cases (on admission)`=as.double(gsub(",","",`No. of Cases (on admission)`)),
         `No. Discharged`=as.double(gsub(",","",`No. Discharged`))) 


ngr_covid_cases <- ngr_covid_cases %>% 
  mutate(Date=NA) %>% 
  dplyr::select(Date,everything()) %>% 
  filter(`States Affected`!="Total") %>% 
  mutate(`States Affected`=ifelse(`States Affected`=="FCT","Federal Capital Territory",`States Affected`))

datacsv <- read_csv("C:\\Users\\Laptop\\Desktop\\Covid map\\R\\Shiny\\CovidMap\\Complete NCDC Data.csv")

#datacsv <- datacsv %>% 
# mutate(Date=mdy(Date))
names(ngr_covid_cases) <- names(datacsv)


#Import another tibble that will be used to fill in non represented states
jointbl <- read_csv("C:\\Users\\Laptop\\Desktop\\Covid map\\Joining table.csv")
colnames(jointbl) <- names(datacsv)
tabch <- function(tabs){
  tabs <- jointbl %>% 
    filter(!State %in% tabs$State) %>% 
    rbind(tabs) 
}


for(x in ls()){
  if(any(class(get(x))=="tbl")& x!="jointbl"){
    assign(x,tabch(get(x)))
  }
}

ngr_covid_cases <-  ngr_covid_cases %>% 
  mutate(Date=max(datacsv$Date)+1)

datacsv <- rbind(datacsv,ngr_covid_cases)
glimpse(datacsv)
write_csv(datacsv,"C:/Users/Laptop/Desktop/Covid map/R/Shiny/CovidMap/rsconnect/Complete NCDC Data.csv")
file.move("C:/Users/Laptop/Desktop/Covid map/R/Shiny/CovidMap/Complete NCDC Data.csv",destinations =  "C:/Users/Laptop/Desktop/Covid map/R/Shiny", overwrite = TRUE )
file.move("C:/Users/Laptop/Desktop/Covid map/R/Shiny/CovidMap/rsconnect/Complete NCDC Data.csv",destinations = "C:/Users/Laptop/Desktop/Covid map/R/Shiny/CovidMap",overwrite = TRUE )
