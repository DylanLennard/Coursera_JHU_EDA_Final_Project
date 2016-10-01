### plot 1###
setwd("~/Desktop/Statistical_Programming/Coursera/Exploratory_Data_Analysis/EDA_Final_Project") # edit this to just be project 2, and rename project_2 directory to JHU_EDA_Final_Project

###
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

# 1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

plot1DF <- NEI %>%
    group_by(year) %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)

barplot(plot1DF$Total_Emissions, names = plot1DF$year)# Yes


# 2) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
plot2DF <- NEI %>%
    group_by(year) %>%
    filter(fips == "24510") %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)
barplot(plot2DF$Total_Emissions, names = plot2DF$year) #yes, but not steadily

# 3) Of the four types of sources indicated by the 'type' (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
plot3DF <- NEI %>%
    group_by(year, type) %>%
    filter(fips == "24510") %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year, type)

#order from nonpoint, point, non-road, on-road 
ggplot(aes(year, Total_Emissions, fill = type), data = plot3DF) + 
    geom_bar(stat = 'identity', position = 'dodge')


# need to merge NEI with SCC

mergedDF <- merge(NEI, SCC, by='SCC', all.x=TRUE, all.y=FALSE)

# need to see if they actually want something more like a heat map here. That would be very cool, and we could do that. 
coal_index <- grep("coal", mergedDF$SCC.Level.Four, ignore.case = TRUE)
coalDF <- mergedDF[coal_index,]
plot4DF <- coalDF %>% 
    group_by(year) %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)

ggplot(aes(x = year, y = Total_Emissions), data = plot4DF) + 
    geom_bar(stat='identity')

# 5) 

mergedDF <- merge(NEI, SCC, by='SCC', all.x=TRUE, all.y=FALSE)

mobile_index <- grep("vehicle|highway", mergedDF$SCC.Level.Two, ignore.case = TRUE) #paved and unpaved roads? 
mobileDF <- mergedDF[mobile_index, ] 

plot5DF <- mobileDF %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)

ggplot(aes(year, Total_Emissions), data = plot5DF) + 
    geom_bar(stat='identity')


# 6 
plot6DF <- mobileDF %>%
    filter(fips == "24510" | fips == "06037") %>%
    group_by(year, fips) %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year)

ggplot(aes(year, Total_Emissions, fill = fips), data = plot6DF) +
    geom_bar(stat='identity', position = 'dodge')
