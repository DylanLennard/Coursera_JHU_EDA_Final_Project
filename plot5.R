### plot 5 ### 

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

# 5 
mergedDF <- merge(NEI, SCC, by = 'SCC', all.x = TRUE, all.y = FALSE)
mobile_index <- grep("vehicle|highway", mergedDF$SCC.Level.Two,
                     ignore.case = TRUE) #paved and unpaved roads? 
mobileDF <- mergedDF[mobile_index, ]

plot5DF <- mobileDF %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)

ggplot(aes(year, Total_Emissions), data = plot5DF) +
    geom_point()+
    geom_line()