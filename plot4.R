### plot 4 ###

setwd("~/Desktop/Statistical_Programming/Coursera/Exploratory_Data_Analysis/EDA_Final_Project")

library("readr")
library("tidyverse")
library("data.table")
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

fileNames <- c("summarySCC_PM25.rds", "Source_Classification_Code.rds")

if(!file.exists(fileNames[1]) || !file.exists(fileNames[2])){
    download.file(url, destfile = "project.zip", method = 'curl')
    unzip("project.zip")
    file.remove("project.zip")
    
}

NEI <- readRDS(fileNames[1])
SCC <- readRDS(fileNames[2])

# 4 
mergedDF <- merge(NEI, SCC, by='SCC', all.x = TRUE, all.y = FALSE)
coal_index <- grep("coal", mergedDF$SCC.Level.Four, ignore.case = TRUE)
coalDF <- mergedDF[coal_index,]

plot4DF <- coalDF %>%
    group_by(year) %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)

ggplot(aes(x = year, y = Total_Emissions), data = plot4DF)+
            geom_bar(stat='identity')


