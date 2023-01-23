#loading packages
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)
#library(tidyverse)
library(openxlsx)
library(splitstackshape)
#library(reshape2)
library(DataCombine)
#library(lubridate)
#Initiate script 


args <- commandArgs(trailingOnly = TRUE)
filename <- args[1]
matchingTableName <- args[2]
print(args)
if (length(args)==0) {
  stop("Script needs the following inputs: <data.file.csv> <matching_list.csv>", call.=FALSE)
  } else if (length(args)==2) {
  #Input and merge data 
  data <- read.csv(file=filename, header=TRUE, row.names=NULL) #read.csv("plant_report_2023-01-01.csv", header=T, row.names=NULL)
  colnames(data) <- colnames(data)[2:ncol(data)]  
  data <- data[ , - ncol(data)]
  matchingTable <- read.csv(file = matchingTableName)
  head(matchingTable)
  
  matchingTable <- matchingTable %>% 
    #rename(sensor_id = Phytech_Sensor_.Unique_ID)
    #rename(sensor_id = Phytech.Sensor_.Unique_ID)
    rename(sensor_id = Phytech_Sensor_.Unique_ID)
  
  merged_data<- merge(matchingTable, data, by="sensor_id")
  
  unique(matchingTable$Grower_Crop_Location)
  list.of.names <- unique(merged_data$Grower_Crop_Location)

  #Adding irrigation information 
  #X-axis is the date as it is now. Y-axis should be time-of-day for Max Shrinking, and the time-of-day for Max Swelling.

  i <- 1
  merged_data$min_time
  merged_data$max_time
  merged_data %>%
    filter(Grower_Crop_Location==list.of.names[i]) %>%
    arrange(Treatment, day) %>%
    select(OH_.Unique_ID, day, min_time) %>%
    spread(OH_.Unique_ID, min_time)
  
  merged_data %>%
    filter(Grower_Crop_Location==list.of.names[i]) %>%
    arrange(Treatment, day) %>%
    select(OH_.Unique_ID,sensor_id, day, min_time)%>%
    unite("ID", OH_.Unique_ID:sensor_id) %>% 
    distinct() %>% 
    spread(ID, min_time)
  
  for (i in 1:length(list.of.names)){ 
    min_data <- merged_data %>%
      filter(Grower_Crop_Location==list.of.names[i]) %>%
      arrange(Treatment, day) %>%
      select(OH_.Unique_ID,sensor_id, day, min_time)%>%
      unite("ID", OH_.Unique_ID:sensor_id) %>% 
      distinct() %>% 
      spread(ID, min_time)
    max_data <- merged_data %>%
      filter(Grower_Crop_Location==list.of.names[i]) %>%
      arrange(Treatment, day) %>%
      select(OH_.Unique_ID, sensor_id, day, max_time)%>%
      unite("ID", OH_.Unique_ID:sensor_id) %>% 
      distinct() %>% 
      spread(ID, max_time)
    write.xlsx(min_data, paste0(list.of.names[i],"_min_irrigationTimes", Sys.Date(), ".xlsx"))
    write.xlsx(max_data, paste0(list.of.names[i],"_max_irrigationTimes", Sys.Date(), ".xlsx"))
  }
}

