### plot 2 ###

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

plot2DF <- NEI %>%
    group_by(year) %>%
    filter(fips == "24510") %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)
barplot(plot2DF$Total_Emissions, names = plot2DF$year) #yes, but not steadily
