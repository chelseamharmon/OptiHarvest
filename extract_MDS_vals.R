#loading packages
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)
library(tidyverse)
library(openxlsx)
library(splitstackshape)
library(reshape2)
library(DataCombine)
library(lubridate)

#Initiate script 
if (length(args)==c(0,1) {
  stop("Script needs the following inputs: <data.file.csv> <matching_list.csv>", call.=FALSE)
} else if (length(args)==2) {
#Input and merge data 
data = read.csv(args[1], header=TRUE, row.names=NULL) #read.csv("plant_report_2023-01-01.csv", header=T, row.names=NULL)
colnames(data) <- colnames(data)[2:ncol(data)]  
data <- data[ , - ncol(data)]
matchingTable = read.csv(args[2])

matchingTable <- matchingTable %>% 
  #rename(sensor_id = Phytech_Sensor_.Unique_ID)
  #rename(sensor_id = Phytech.Sensor_.Unique_ID)
  rename(sensor_id = Phytech_Sensor_.Unique_ID)

merged_data<- merge(matchingTable, data, by="sensor_id")

unique(matchingTable$Grower_Crop_Location)

#Making MDS spreadsheet
list.of.names <- unique(merged_data$Grower_Crop_Location)

for (i in 1:length(list.of.names)){ 
  #for (i in 1:length(unique(merged_data$Grower_Crop_Location))){
  spread_data <- merged_data %>% 
    filter(Grower_Crop_Location==list.of.names[i]) %>% 
    arrange(Treatment, day)%>% 
    select(OH_.Unique_ID, sensor_id,day, mds) %>%
    unite("ID", OH_.Unique_ID:sensor_id) %>% 
    distinct() %>% #note has repeated data 
    spread(ID, mds)
  means <- merged_data %>% 
    filter(Grower_Crop_Location==list.of.names[i]) %>% 
    group_by(day, Treatment) %>% 
    summarize(Mean=mean(mds, na.rm=TRUE)) %>% 
    spread(Treatment, Mean)
  se_data <- merged_data %>% 
    filter(Grower_Crop_Location==list.of.names[i]) %>%
    group_by(day, Treatment) %>% 
    summarise_at(vars(mds), funs( sd(., na.rm=T), se = sd/sqrt(n()))) %>%
    select(-sd) %>%
    spread(Treatment, se)
  combined <- cbind(spread_data, means[2:length(means)], se_data[2:length(se_data)])#only two averaging
  write.xlsx(combined, paste0(list.of.names[i],"_mds", Sys.Date(), ".xlsx"))
}
}
