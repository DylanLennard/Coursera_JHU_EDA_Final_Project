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
# 4 


# plot4 <- mergedDF %>% #heat mapDF
#     group_by(year) %>%
#     filter(# add in filter for coal combustion related) %>%
#     summarise(Total_Emissions = sum(Emissions)) %>%
#     arrange(year)

# 5) 
plot6DF <- mergedDF %>%
    group_by(year, type) %>%
    filter(fips == "24510" | fips == "06037") %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year, type)

# 6) 
plot6DF <- mergedDF %>%
    group_by(year, type) %>%
    filter(fips == "24510" | fips == "06037") %>%
    summarise(Total_Emissions = sum(Emissions)) %>%
    arrange(year, type)

#####################
#hierarchical clustering  
set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1, 2, 1), each = 4), 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
dataFrame <- data.frame(x=x, y=y)
dist(dataFrame)
