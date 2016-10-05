### plot3 ### 
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

plot3DF <- NEI %>%
    group_by(year, type) %>%
    filter(fips == "24510") %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year, type)

#order from nonpoint, point, non-road, on-road 
ggplot(aes(year, Total_Emissions, fill = type), data = plot3DF) + 
    geom_bar(stat = 'identity', position = 'dodge')