########################################################################
############Jan5th 2021, Code written by Anne Devan-Song################
######Create DFs for spadefoot assortaitvity############################
############Bend, OR, USA###############################################
########################################################################

#This code cleans raw data and creates combined CSVs 
rm(list = ls()) #clear workspace
graphics.off() #clear plots 

library(tidyverse) #load all the libraries you need for this code

data <- read.csv("Roads_cleaned_with_soil_habitat.csv")

#Pick useful columns for analyses and separate into 2 dataframes by year
#filter out unuseable data (repeated surveys or no egg data) then save as 2 separate CSVs 
#add an updated index with a column called lineOrig

subdf <- data[, c("Index", 
                  "Year", 
                  "Sheet_ID", 
                  "Date", 
                  "Date_survey_started", 
                  "Survey", 
                  "No", 
                  "Time", 
                  "Y", 
                  "X", 
                  "SVL_mm", 
                  "Color", 
                  "Nuptial_Pads", 
                  "gravid", 
                  "Sex", 
                  "Adjusted_sex", 
                  "Mass", 
                  "COMMENTS", 
                  "LOCAL_NAME", 
                  "FORM_NAME", 
                  "ALLIAN_SCI", 
                  "ALLIAN_ENG", 
                  "ASSOC_SCI", 
                  "AREASYMBOL", 
                  "MUSYM", 
                  "MUKEY"
                  )]
subdf$SVL_mm <- as.numeric(subdf$SVL_mm)

subdf <- subset(subdf, Survey != "R")
subdf <- subset(subdf, Survey != "Remove")
subdf <- subset(subdf, Survey != "Pilot")
subdf <- subset(subdf, Survey != "")

soillegend <- read.csv("Soil_legend.csv")
merged <- merge(subdf, soillegend, by = "MUSYM") 
merged <- merged[order(merged$Index, decreasing = FALSE), ]
df2016 <- subset(merged, Year=="2016")
nrow(df2016)
df2016$lineOrig <- c(0:588) 
write.csv(df2016, "2016_data_cleaned.csv", row.names=FALSE)

df2017 <- subset(merged, Year=="2017")
nrow(df2017)
df2017$lineOrig <- c(0:1150)
write.csv(df2017, "2017_data_cleaned.csv", row.names=FALSE)

#we want to keep 2 separate CSVs of 2017 and 2016 for ease in GIS, but also create a combined df: 
all_data_cleaned <- rbind(df2016, df2017)
write.csv(all_data_cleaned, file="all_data_cleaned.csv", row.names=FALSE)

